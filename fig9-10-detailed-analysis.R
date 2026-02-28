library(tidyverse)
import::from(hydroGOF, KGE, NSE, pbias)
import::from(xtable, xtable)
import::from(ggthemes, colorblind_pal)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6,
  dplyr.summarise.inform = FALSE
)

figure_dir <- "plots"
dir.create(figure_dir, showWarnings = FALSE)

basins <- c("FSSO3", "WGCM8", "SAKW1")
zones <- c("FSSO3" = "1zone", "WGCM8" = "2zone", "SAKW1" = "2zone")
usgs_locations <- read_csv("data/202005_usgs_locations.csv") |>
  filter(lid %in% basins)

calb_types <- c("cv_1", "cv_2", "cv_3", "cv_4", "por")
forcing_types <- c("FA", "noFA")
calb_data_list <- list()
i <- 0
for (basin in basins) {
  for (calb_type in calb_types) {
    for (forcing_type in forcing_types) {
      i <- i + 1
      basin_dir <- sprintf(
        "nwrfc-calibration-paper-data/Optimization_CAMELS_%s/%s/%s",
        forcing_type,
        zones[basin],
        basin
      )
      calb_dir <- list.files(basin_dir, paste0("results_", calb_type, "_*"))
      calb_data_list[[i]] <- read_csv(file.path(
        basin_dir,
        calb_dir,
        "optimal_daily.csv"
      )) |>
        mutate(
          basin = basin,
          forcing_type = forcing_type,
          calb_type = calb_type
        )
    }
  }
}

calb_data <- calb_data_list |>
  bind_rows() |>
  rename(obs = flow_cfs, sim = sim_flow_cfs) |>
  mutate(
    date_chr = sprintf("%s-%02d-%02d", year, month, day),
    date = fast_strptime(date_chr, "%Y-%m-%d")
  )

calb_data_long <- calb_data |>
  pivot_longer(c(obs, sim), values_to = "flow", names_to = "data_type")

# overall metric table
calb_data |>
  group_by(basin, calb_type, forcing_type) |>
  summarise(kge = KGE(sim, obs), nse = KGE(sim, obs), .groups = "drop") |>
  pivot_wider(
    id_cols = c(basin, forcing_type),
    names_from = calb_type,
    values_from = kge
  ) |>
  xtable(digits = 3)

# monthly metric table
calb_data |>
  group_by(basin, month, calb_type, forcing_type) |>
  summarise(kge = KGE(sim, obs), nse = KGE(sim, obs), .groups = "drop") |>
  pivot_wider(
    id_cols = c(basin, forcing_type, month),
    names_from = calb_type,
    values_from = nse
  ) |>
  mutate(month = as.integer(month)) |>
  arrange(basin, forcing_type, month) |>
  group_by(basin) |>
  group_split() |>
  map(function(x) {
    xtable(x, digits = 3) |>
      print()
  }) -> shhh


low_high_flow <- calb_data |>
  group_by(basin, forcing_type, calb_type) |>
  mutate(
    p05 = quantile(obs, .05),
    p95 = quantile(obs, .95)
  ) |>
  mutate(
    flow = case_when(
      obs < p05 ~ "Low Flow",
      obs > p95 ~ "High Flow",
      .default = NA
    )
  ) |>
  na.omit()
low_high_flow_pbias <- low_high_flow |>
  group_by(basin, forcing_type, calb_type, flow) |>
  summarise(
    pbias = pbias(sim, obs),
    nse = NSE(sim, obs),
    x = 0.1,
    y = 0.9
  ) |>
  mutate(label = paste0("bias = ", round(pbias, 1), "%"))

# cyclical plots
cyclical <- calb_data_long |>
  group_by(basin, calb_type, forcing_type, data_type, month, day) |>
  summarise(
    q10 = quantile(flow, .1),
    mean = mean(flow),
    q90 = quantile(flow, .9),
    .groups = "drop"
  ) |>
  mutate(plot_date = ISOdate(ifelse(month < 10, 2000, 1999), month, day)) #|>
# pivot_longer(c(q10,mean,q90),names_to='variable')

p_cyclical <- cyclical |>
  filter(forcing_type == "FA") |>
  filter(calb_type == "por") |>
  mutate(data_type = ifelse(data_type == "obs", "Observed", "Simulated")) |>
  ggplot() +
  geom_ribbon(
    aes(plot_date, ymin = q10, ymax = q90),
    alpha = 0.2,
    data = cyclical |> filter(data_type == "sim")
  ) +
  geom_line(aes(plot_date, mean, color = data_type)) +
  facet_wrap(~basin, ncol = 1) +
  scale_x_datetime(
    date_breaks = "month",
    date_labels = "%b",
    expand = c(0.01, 0.01)
  ) +
  theme_minimal() +
  scale_color_manual("", values = c("black", "cornflowerblue")) +
  scale_fill_manual(values = colorblind_pal()(8)[c(2, 3)]) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  labs(y = "Streamflow [cfs]", x = "")
p_cyclical
sprintf("%s/cyclical.pdf", figure_dir) |>
  ggsave(p_cyclical, width = 8, height = 6)

p_timeseries <- calb_data_long |>
  filter(year %in% c(2019:2021), calb_type == "por") |>
  mutate(data_type = ifelse(data_type == "obs", "Observed", "Simulated")) |>
  filter(forcing_type == "FA") |>
  ggplot() +
  geom_line(aes(as.POSIXct(date), flow, color = data_type), alpha = .75) +
  facet_wrap(~basin, ncol = 1, scales = "free_y") +
  scale_color_manual("", values = colorblind_pal()(8)[1:2]) +
  theme_minimal() +
  labs(x = "", y = "Flow [cfs]") +
  scale_x_datetime(minor_breaks = "month") +
  theme(legend.position = "top")
p_timeseries
sprintf("%s/timeseries.pdf", figure_dir) |>
  ggsave(p_timeseries, width = 8, height = 6)


p_peak_flow <- low_high_flow |>
  filter(flow == "High Flow") |>
  filter(calb_type == "por", forcing_type == "FA") |>
  ggplot() +
  geom_point(aes(sim, obs)) +
  geom_label(
    aes(x = 0, y = Inf, label = label),
    vjust = 1.5,
    hjust = 0,
    data = low_high_flow_pbias |>
      filter(calb_type == "por", forcing_type == "FA") |>
      filter(flow == "High Flow")
  ) +
  facet_grid(flow ~ basin) +
  coord_equal() +
  geom_abline(slope = 1, intercept = 0) +
  theme_minimal() +
  labs(x = "Simulated flow [cfs]", y = "Observed flow [cfs]")
p_peak_flow
sprintf("%s/peak_flow.pdf", figure_dir) |>
  ggsave(p_peak_flow, width = 8, height = 3)


p_low_flow <- low_high_flow |>
  filter(flow == "Low Flow") |>
  filter(calb_type == "por", forcing_type == "FA") |>
  ggplot() +
  geom_point(aes(sim, obs)) +
  geom_label(
    aes(x = Inf, y = 0, label = label),
    vjust = 0.2,
    hjust = 1.1,
    data = low_high_flow_pbias |>
      filter(calb_type == "por", forcing_type == "FA") |>
      filter(flow == "Low Flow")
  ) +
  facet_grid(flow ~ basin) +
  geom_abline(slope = 1, intercept = 0) +
  theme_minimal() +
  labs(x = "Simulated flow [cfs]", y = "Observed flow [cfs]")
p_low_flow
sprintf("%s/low_flow.pdf", figure_dir) |>
  ggsave(p_low_flow, width = 8, height = 3)

# bydyko plot for camels basins
