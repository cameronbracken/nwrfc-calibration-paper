library(tidyverse)
library(sf)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6
)

basins_to_plot <- c("FSSO3", "WGCM8", "SAKW1")

nwrfc_domain <- st_read(
  "nwrfc-calibration-paper-data/NWRFC_Boundary/NWRFC_boundary.shp"
)
nwrfc_camels <- st_read(
  "nwrfc-calibration-paper-data/CAMELS_NWRFC/CAMELS_NWRFC.shp"
)
# basins <- st_read("data/hlr-clustering/shape/basins_operational.shp")
basins <- st_read(
  "nwrfc-calibration-paper-data/nwrfc_basins_sf/NWRFC_Forecast_Basins_20240113.shp"
)
fc_points <- st_read(
  "nwrfc-calibration-paper-data/NWRFC_forecast_points/NWRFC_Forecast_Points_20240512.shp"
)

states <- st_read(
  "nwrfc-calibration-paper-data/cb_2018_us_state_5m/cb_2018_us_state_5m.shp"
) |>
  filter(
    NAME %in%
      c(
        "Washington",
        "Oregon",
        "Idaho",
        "Montana",
        "Wyoming",
        "California",
        "Nevada",
        "Utah"
      )
  )
countries <- "nwrfc-calibration-paper-data/world-administrative-boundaries/world-administrative-boundaries.shp" |>
  st_read() |>
  filter(name %in% c("Canada"))

map <- ggplot() +
  geom_sf(data = states, fill = NA, color = gray(.8)) +
  geom_sf(data = countries, fill = NA, color = gray(.8)) +
  geom_sf(data = nwrfc_domain, fill = NA, linewidth = .8) +
  geom_sf(data = basins, fill = NA) +
  # geom_sf(data = nwrfc_camels, fill = grey(.7)) +
  geom_sf(data = fc_points, size = .9, color = "steelblue") +
  # geom_sf(aes(fill = LID), data = basins |> filter(substr(LID, 1, 5) %in% basins_to_plot)) +
  coord_sf(xlim = c(-124.5, -110), ylim = c(41, 52.5)) +
  # scale_fill_manual("", values = c("#337357", "#FFD23F", "#EE4266")) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.8, .8),
    panel.grid = element_blank()
  )
print(map)
ggsave("plots/basin-map.png", width = 8, height = 8, dpi = 600)
