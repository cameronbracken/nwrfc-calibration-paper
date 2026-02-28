library(tidyverse)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6
)

data_dir <- "nwrfc-calibration-paper-data/CAMELS_all_locations/2zone"

# Get all basins
basins <- list.dirs(data_dir, recursive = FALSE, full.names = FALSE)

# Read all pars_limits.csv files
all_limits <- map_dfr(basins, function(basin) {
  file_path <- file.path(data_dir, basin, "pars_limits.csv")
  if (file.exists(file_path)) {
    read_csv(file_path) |>
      mutate(basin = basin)
  }
})

# Filter to zone-specific parameters (zone 1 and zone 2)
# Extract base parameter name by removing the basin-zone suffix (e.g., _ARGW1-1)
zone_limits <- all_limits |>
  filter(str_detect(zone, "-[12]$")) |>
  mutate(
    # Extract zone number
    zone_num = str_extract(zone, "[12]$"),
    zone_label = paste("Zone", zone_num),
    # Remove the basin-zone suffix to get base parameter name
    param = str_replace(name, "_[A-Z]+[0-9]*-[12]$", "")
  )

# Create summary table of parameter limits across all basins by zone
param_summary <- zone_limits |>
  group_by(param, zone_label) |>
  summarise(
    n_basins = n(),
    lower_min = min(lower),
    lower_max = max(lower),
    lower_mean = mean(lower),
    upper_min = min(upper),
    upper_max = max(upper),
    upper_mean = mean(upper),
    range_mean = mean(upper - lower),
    .groups = "drop"
  ) |>
  arrange(param, zone_label)

print(param_summary, n = 60)

# Create a figure showing parameter ranges with actual values
# Summary for plotting: show actual limits and variation across basins
param_range_summary <- zone_limits |>
  group_by(param, zone_label) |>
  summarise(
    lower_min = min(lower),
    lower_median = median(lower),
    lower_max = max(lower),
    upper_min = min(upper),
    upper_median = median(upper),
    upper_max = max(upper),
    lower_sd = sd(lower),
    upper_sd = sd(upper),
    .groups = "drop"
  )

# Plot each parameter on its own scale using facet_wrap with free scales
param_plot_long <- zone_limits |>
  pivot_longer(
    cols = c(lower, upper),
    names_to = "limit_type",
    values_to = "value"
  ) |>
  mutate(limit_type = factor(limit_type, levels = c("lower", "upper")))

# Create individual faceted plots showing distribution across basins
# Color by zone to distinguish zone 1 and zone 2
p <- ggplot(
  param_plot_long,
  aes(x = limit_type, y = value, fill = zone_label)
) +
  geom_boxplot(
    alpha = 0.6,
    outlier.shape = NA,
    position = position_dodge(0.8)
  ) +
  geom_point(
    aes(color = zone_label),
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
    alpha = 0.4,
    size = 1
  ) +
  facet_wrap(~param, scales = "free_y", ncol = 4) +
  scale_fill_manual(
    values = c("Zone 1" = "steelblue", "Zone 2" = "darkorange")
  ) +
  scale_color_manual(
    values = c("Zone 1" = "steelblue", "Zone 2" = "darkorange")
  ) +
  labs(
    x = "Limit Type",
    y = "Parameter Value",
    fill = "Zone",
    color = "Zone",
    title = "SAC-SMA/SNOW-17 Parameter Limits",
    subtitle = "Distribution of lower and upper bounds across CAMELS basins"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(size = 8),
    legend.position = "bottom"
  )

print(p)
ggsave("figures/parameter-limits-zones.pdf", p, width = 12, height = 14)

# Create a table showing actual (non-normalized) values
param_table <- param_summary |>
  select(param, zone_label, lower_mean, upper_mean, range_mean) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )

print(param_table, n = 60)
