library(tidyverse)
library(ggthemes)
import::from(hydroGOF, KGE, NSE, pbias)
import::from(xtable, xtable, print.xtable)
import::from(ggthemes, colorblind_pal)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6,
  dplyr.summarise.inform = FALSE
)

# data_dir = "nwrfc-calibration-paper-data/CAMELS_all_locations/2zone"
calb_summary_fn = "nwrfc-calibration-paper-data/AC+Legacy_Metrics.csv"
figure_dir = "figures"
regional_model_dir = "nwrfc-calibration-paper-data/single-basin-vs-regional-model/run_dirs/regional_model"

# Load AC+Legacy metrics
calb_summary =
  calb_summary_fn |>
  read_csv() |>
  rename(nKGE = NKGE, nNSE = NNSE, nPBIAS = NPBIAS) |>
  pivot_longer(-c(LID, Run, Sim, Obs), names_to = "metric") |>
  filter(metric %in% c("nKGE", "nNSE", "nPBIAS", "R2")) |>
  rename_all(tolower)

# Load USGS locations to map basin IDs
usgs_locations =
  read_csv("data/202005_usgs_locations.csv") |>
  mutate(basin = as.character(gsno)) |>
  select(basin, lid)

# Load regional model results (531 basins, all ensemble members)
lstm_ensemble_dirs =
  list.dirs(
    regional_model_dir,
    recursive = FALSE,
    full.names = TRUE
  ) |>
  str_subset("531_basins_multi_forcings_temporal_split_ensemble_member")

lstm_metrics =
  lstm_ensemble_dirs |>
  map(function(ens_dir) {
    # Find the test metrics file (may be in different epoch folders)
    test_metrics_file = list.files(
      file.path(ens_dir, "test"),
      pattern = "test_metrics.csv",
      recursive = TRUE,
      full.names = TRUE
    )[1]

    if (!is.na(test_metrics_file) && file.exists(test_metrics_file)) {
      read_csv(test_metrics_file) |>
        mutate(
          ensemble_member = str_extract(
            basename(ens_dir),
            "ensemble_member_\\d+"
          )
        )
    }
  }) |>
  bind_rows() |>
  # Join with USGS locations to get LID
  inner_join(usgs_locations, by = "basin") |>
  # Filter to only basins in our study
  filter(lid %in% unique(read_csv(calb_summary_fn)$LID)) |>
  # Select and rename metrics to match AC+Legacy format
  select(
    lid,
    ensemble_member,
    NSE,
    KGE,
    r = `Pearson-r`,
    bias_kge = `Beta-KGE`
  ) |>
  mutate(
    nNSE = 1 / (2 - NSE),
    nKGE = 1 / (2 - KGE),
    nPBIAS = 1 - abs(bias_kge - 1),
    R2 = r^2 # Not available in regional model output
  ) |>
  select(-NSE, -KGE, -bias_kge, -r) |>
  pivot_longer(
    -c(lid, ensemble_member),
    names_to = "metric",
    values_to = "value"
  ) |>
  filter(!is.na(value)) |>
  rename(LID = lid) |>
  mutate(Run = "LSTM") |>
  rename_all(tolower) |>
  group_by(lid, metric, run) |>
  summarise(value = mean(value))

# (531 basins)

plot_names = c(
  'NWRFC SAC-SMA/SNOW-17 auto-calibration',
  'NWRFC SAC-SMA/SNOW-17 legacy calibration',
  'LSTM (Kratzert et al., 2024)'
)

# Combine AC+Legacy with regional model results
combined_metrics =
  calb_summary |>
  select(lid, run, metric, value) |>
  bind_rows(lstm_metrics |> select(lid, run, metric, value)) |>
  mutate(
    run = case_when(
      run == 'AutoCalb' ~ plot_names[1],
      run == 'Legacy' ~ plot_names[2],
      run == 'LSTM' ~ plot_names[3]
    )
  ) |>
  pivot_wider(id_cols = c(lid, metric), names_from = run) |>
  # drop missing basins from
  na.omit() |>
  pivot_longer(-c(lid, metric), names_to = 'run') |>
  mutate(metric = factor(metric, levels = c('nNSE', 'nPBIAS', 'R2', 'nKGE')))

cdf_pal = colorblind_pal()(8)[1:3]
names(cdf_pal) = plot_names
# cdf_colors = c(
#   'NWRFC auto-calibration' = cdf_pal[1],
#   'NWRFC legacy calibration' = cdf_pal[2],
#   'LSTM (Kratzert et al., 2024)' = cdf_pal[3]
# )

# Create CDF plot comparing all methods
p_cdf_summary =
  combined_metrics |>
  ggplot() +
  stat_ecdf(aes(value, color = run)) +
  theme_minimal() +
  scale_color_manual(values = cdf_pal, breaks = plot_names) +
  facet_wrap(~metric, scales = "free_x", nrow = 1) +
  theme(legend.position = "bottom") +
  labs(
    x = "Metric Value",
    y = "Cumulative Distribution\nFunction (CDF)",
    color = "Calibration\nMethod"
  ) +
  guides(color = guide_legend(nrow = 2)) +
  scale_x_continuous(limits = c(0.71, 1.0)) +
  theme(axis.text = element_text(size = 8))
print(p_cdf_summary)
sprintf("%s/cdf_camels_summary.pdf", figure_dir) |>
  ggsave(p_cdf_summary, width = 8, height = 4)

# Summary statistics comparison
summary_stats =
  combined_metrics |>
  group_by(run, metric) |>
  summarise(
    median = median(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(metric, run)

print(summary_stats)
summary_stats |>
  xtable(digits = 3) |>
  print.xtable(include.rownames = FALSE)
