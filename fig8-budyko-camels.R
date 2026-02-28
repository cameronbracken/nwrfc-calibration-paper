library(tidyverse)
import::from(hydroGOF, KGE, NSE, pbias)
import::from(xtable, xtable, print.xtable)
import::from(ggthemes, colorblind_pal, scale_color_colorblind)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6,
  dplyr.summarise.inform = FALSE
)

data_dir <- "nwrfc-calibration-paper-data/CAMELS_all_locations/2zone"
# calb_sum
calb_dir <- "results_por_01"
figure_dir <- "plots"

basins <- list.files(data_dir)
# usgs_locations <- read_csv("data/imputation/202005_usgs_locations.csv") |>
#   filter(lid %in% basins)

calb_data <- basins |>
  map(function(basin) {
    read_csv(file.path(data_dir, basin, calb_dir, "optimal_daily.csv")) |>
      mutate(basin = basin) |>
      rename(obs = flow_cfs, sim = sim_flow_cfs) |>
      mutate(
        date_chr = sprintf("%s-%02d-%02d", year, month, day),
        date = fast_strptime(date_chr, "%Y-%m-%d")
      )
  }) |>
  bind_rows()

calb_data_long <- calb_data |>
  pivot_longer(c(obs, sim), values_to = "flow", names_to = "data_type")

# overall metric table
kge <- calb_data |>
  group_by(basin) |>
  summarise(
    kge = KGE(sim, obs), # nse = NSE(sim, obs),
    .groups = "drop"
  )
kge |>
  xtable(digits = 3) |>
  print.xtable(include.rownames = FALSE)


par_limits <- basins |>
  map(function(basin) {
    read_csv(file.path(data_dir, basin, "pars_limits.csv")) |>
      mutate(basin = basin)
  }) |>
  bind_rows()

pars_optimal <- basins |>
  map(function(basin) {
    read_csv(file.path(data_dir, basin, calb_dir, "pars_optimal.csv")) |>
      mutate(basin = basin)
  }) |>
  bind_rows()

# zones <- optimal_pars |>
#   pull(zone) |>
#   unique() |>
#   as.character() |>
#   sort() |>
#   str_subset("-")

states_optimal_6h <- basins |>
  map(
    function(basin) {
      read_csv(
        file.path(data_dir, basin, calb_dir, "optimal_states_6hr.csv"),
        show = FALSE
      )[-(1:(366 * 4)), ] |>
        mutate(datetime = ISOdatetime(year, month, day, 0, 0, 0)) %>%
        pivot_longer(
          -c(datetime, year, month, day, hour),
          names_sep = "_",
          names_to = c("variable", "zonei")
        ) %>%
        mutate(
          wyear = ifelse(month >= 10, year + 1, year),
          type = "adjusted",
          basin = basin
        ) %>%
        pivot_wider(
          id_cols = c(
            datetime,
            basin,
            zonei,
            year,
            month,
            day,
            hour,
            wyear,
            type
          ),
          names_from = variable
        )
    },
    .progress = TRUE
  ) |>
  bind_rows() |>
  rename(zone = zonei)

# optimal_states_6h <- optimal_states_6h_ %>%
#  inner_join(data.frame(zone = zones, zonei = as.character(1:n_zones)), by = "zonei")

# budyko diagram with all zones
scf <- pars_optimal |>
  filter(name == "scf") |>
  select(zone, value) |>
  rename(scf = value) |>
  as_tibble()
zone_area <- pars_optimal |>
  filter(name == "zone_area") |>
  select(zone, value) |>
  rename(zone_area = value) |>
  as_tibble()

# weighted average based on zone area
states_basin_ave <- states_optimal_6h |>
  inner_join(scf, by = "zone") |>
  inner_join(zone_area, by = "zone") |>
  mutate(datetime = datetime + hours(hour)) |>
  mutate(map2 = map * (1 - ptps) + map * ptps * scf) |>
  select(datetime, basin, zone, map2, aet, pet, zone_area) |>
  group_split(basin) %>%
  # .[[1]] -> basin_data
  map(
    function(basin_data) {
      basin <- basin_data$basin[1]
      basin_area <- basin_data$zone_area[1:2] |> sum()
      wide_data <- basin_data |>
        select(-basin) |>
        pivot_wider(
          id_cols = c(datetime),
          names_from = zone,
          names_sep = "_",
          values_from = -c(datetime, zone)
        )
      map2_1 <- paste0("map2_", basin, "-1")
      map2_2 <- paste0("map2_", basin, "-2")
      pet_1 <- paste0("pet_", basin, "-1")
      pet_2 <- paste0("pet_", basin, "-2")
      aet_1 <- paste0("aet_", basin, "-1")
      aet_2 <- paste0("aet_", basin, "-2")
      area_1 <- paste0("zone_area_", basin, "-1")
      area_2 <- paste0("zone_area_", basin, "-2")

      wide_data |>
        mutate(
          map2 = !!as.name(map2_1) *
            !!as.name(area_1) /
              basin_area +
              !!as.name(map2_2) * !!as.name(area_2) / basin_area,
          pet = !!as.name(pet_1) *
            !!as.name(area_1) /
              basin_area +
              !!as.name(pet_2) * !!as.name(area_2) / basin_area,
          aet = !!as.name(aet_1) *
            !!as.name(area_1) /
              basin_area +
              !!as.name(aet_2) * !!as.name(area_2) / basin_area
        ) |>
        select(datetime, map2, pet, aet) |>
        mutate(basin = basin)
    },
    .progress = T
  ) |>
  bind_rows()

# for(z in sort(unique(x$zone))){
b <- states_basin_ave |>
  mutate(month = month(datetime)) |>
  # mutate(wyear = ifelse(month >= 10, year + 1L, year)) %>%
  # inner_join(scf, by = "zone") %>%
  group_by(basin) %>%
  # mutate(map2 = map * (1 - ptps) + map * ptps * scf) %>%
  summarise(
    pet = mean(pet),
    map = mean(map2),
    aet = mean(aet),
    .groups = "drop"
  ) %>%
  mutate(x = pet / map, y = aet / map) |>
  # mutate(zonei = zone |> str_split("-") |> sapply("[", 2)) |>
  left_join(kge, by = "basin")

# bx <- seq(0, max(max(b$x), 2), by = 0.05)
bx <- seq(0, 2, by = 0.05)
by <- (bx * tanh(1 / bx) * (1 - exp(-bx)))^(1 / 2)

# p = ggplot(b %>% filter(zone==z))+
p_budyko <- ggplot(b) +
  geom_line(aes(x, y), data = data.frame(x = c(0, 1), y = c(0, 1))) +
  geom_hline(aes(yintercept = 0.997)) +
  geom_vline(aes(xintercept = 1), linetype = "dashed") +
  geom_line(aes(x, y), data = data.frame(x = bx, y = by), linetype = "dotted") +
  geom_point(aes(x, y, fill = kge), color = "black", shape = 21) +
  # scale_shape_manual("Zone", values = c(21, 22, 23)) +
  scale_fill_viridis_c("KGE", option = "F") +
  # xlim(c(0, 2)) +ylim(c(0, 1)) +
  xlab("Dryness Index (PET/P)") +
  ylab("Evaporative Index (AET/P)") +
  theme_minimal() +
  geom_text(
    aes(x, y, label = label),
    data = data.frame(x = 0.5, y = 0.5, label = "Energy Limit"),
    angle = 45,
    vjust = -1
  ) +
  geom_text(
    aes(x, y, label = label),
    data = data.frame(x = 0.25, y = 0.98, label = "Water Limit"),
    vjust = 1
  ) +
  geom_text(
    aes(x, y, label = label),
    data = data.frame(x = 0.5, y = 0.05, label = "Energy Limited")
  ) +
  geom_text(
    aes(x, y, label = label),
    data = data.frame(x = 1.5, y = 0.05, label = "Water Limited")
  ) +
  # coord_cartesian(xlim = c(0, 1.05 * max(b$x)))
  coord_equal(expand = F, clip = "on", xlim = c(0, 2))
p_budyko
sprintf("%s/budyko.pdf", figure_dir) |>
  ggsave(p_budyko, width = 6, height = 3)
# }
