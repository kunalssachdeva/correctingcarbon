# -------------------------------------------------------------------
# Title: Restatements — Figures for NCC Submission
# Purpose: Build figures from scope1.csv with clean, reproducible code
# Notes:
#   - Variable names preserved exactly as in the original script.
#   - Paths are configurable via environment variables:
#       DATA_DIR        (default: "./Submission Packages/Nature Climate Change/Data")
#       ACCT_DATA_DIR   (default: "./../Analysis/Submission Packages/Nature Climate Change/Data")
#       FIGURES_DIR     (default: "./Output/Figures")
#   - No setwd(); all paths use file.path() and hooks above.
# -------------------------------------------------------------------

# (Optional) Clear workspace for a clean run
# rm(list = ls()); gc()

# ---- Packages ------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)  # ggplot2, dplyr, readr, tibble, etc.
  library(scales)     # percent(), comma()
  library(viridis)    # viridis color scales
})

# ---- Path hooks (customize via env vars) ---------------------------
data_dir      <- Sys.getenv("DATA_DIR",      unset = "./Submission Packages/Nature Climate Change/Data")
acct_data_dir <- Sys.getenv("ACCT_DATA_DIR", unset = "./../Analysis/Submission Packages/Nature Climate Change/Data")
figures_dir   <- Sys.getenv("FIGURES_DIR",   unset = "./Output/Figures")

# Ensure output directory exists
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# Input files
scope1_path     <- file.path(data_dir, "scope1.csv")
accounting_path <- file.path(acct_data_dir, "AccountingRestatements.csv")

# Fail fast if inputs are missing
if (!file.exists(scope1_path)) {
  stop("scope1.csv not found at: ", normalizePath(scope1_path, mustWork = FALSE))
}
if (!file.exists(accounting_path)) {
  stop("AccountingRestatements.csv not found at: ", normalizePath(accounting_path, mustWork = FALSE))
}

# Read base data once; create fresh df copies per section as needed
df_scope1 <- read.csv(scope1_path, stringsAsFactors = FALSE)

# ===================================================================
# 1) CUMULATIVE EMISSIONS RESTATEMENT OVER TIME (Figure2b)
# ===================================================================

# Fresh copy (intentional isolation per section)
df <- df_scope1

# Defensive: ensure numeric columns
df$latestPollution   <- as.numeric(df$latestPollution)
df$originalPollution <- as.numeric(df$originalPollution)

# Compute cumulative sums of restatement deltas by label over time
originalSeries <- df %>%
  mutate(
    restatedFromOriginalAndGreater = as.logical(restatedFromOriginalAndGreater),
    UnderstatedLabel               = ifelse(restatedFromOriginalAndGreater, "Understated", "Overstated"),
    delta                          = latestPollution - originalPollution
  ) %>%
  group_by(fyear, UnderstatedLabel) %>%
  summarise(agg_delta = sum(delta, na.rm = TRUE), .groups = "drop") %>%
  arrange(fyear) %>%
  group_by(UnderstatedLabel) %>%
  mutate(agg_delta_cumsum = cumsum(agg_delta)) %>%
  ungroup()

# Plot cumulative missing emissions over time (million metric tons)
ggplot(
  originalSeries,
  aes(x = fyear,
      y = agg_delta_cumsum / 1e6,
      group = UnderstatedLabel,
      color = UnderstatedLabel,
      linetype = UnderstatedLabel)
) +
  geom_point(size = 6) +
  geom_line(size = 2) +
  scale_color_manual(values = c("Understated" = "blue", "Overstated" = "red")) +
  scale_linetype_manual(values = c("Understated" = "dashed", "Overstated" = "solid")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.25) +
  scale_x_continuous(
    breaks = seq(min(originalSeries$fyear, na.rm = TRUE),
                 max(originalSeries$fyear, na.rm = TRUE), by = 1)
  ) +
  scale_y_continuous(labels = comma, limits = c(-100, 200)) +
  labs(x = "Year", y = "CO2E (Million Metric Tons)") +
  theme_minimal(base_size = 20) +
  theme(
    plot.title       = element_text(face = "bold", size = 32, hjust = 0.5),
    axis.title.x     = element_text(margin = margin(t = 10)),
    axis.title.y     = element_text(margin = margin(r = 10)),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(figures_dir, "Figure2b_Cumulative.pdf"), width = 10.6, height = 6)

# ===================================================================
# 2) HISTOGRAM OF RELATIVE RESTATEMENTS (Figure2a)
# ===================================================================

df <- df_scope1

# Keep only restated observations; take log of relative restatement
df_histogram <- df %>%
  mutate(restatedFromOriginal = as.logical(restatedFromOriginal)) %>%
  filter(restatedFromOriginal) %>%
  mutate(logRR = log(relativeRestatement))

# Quartiles for annotations (IQR shading)
percentiles <- quantile(df_histogram$logRR, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Axis limits and bin settings
x_lo     <- -0.5
x_hi     <-  0.5
bins     <- 75
binwidth <- (x_hi - x_lo) / bins

# Histogram with percent-per-bin density and IQR shading
ggplot(df_histogram, aes(x = logRR)) +
  # IQR shading
  geom_rect(xmin = percentiles[1], xmax = percentiles[3], ymin = 0, ymax = Inf, alpha = 0.01) +
  # Histogram: percent of total in each bin
  geom_histogram(
    aes(y = ..density.. * binwidth * 100),
    binwidth = binwidth,
    fill     = viridis(1, option = "C"),
    alpha    = 0.6,
    color    = "white",
    size     = 0.3
  ) +
  # Density curve: same percent-per-bin scaling
  geom_density(
    aes(y = ..density.. * binwidth * 100),
    size  = 1,
    color = viridis(1, option = "A")
  ) +
  # Quartile lines & labels
  geom_vline(xintercept = percentiles, linetype = "dashed") +
  annotate("text", x = percentiles[1],   y = 15, label = "25th Percentile", angle = 90,
           vjust = -0.25, size = 6, fontface = "bold") +
  annotate("text", x = percentiles[2]*2.5, y = 15, label = "Median", angle = 90,
           hjust = 1.1, size = 6, fontface = "bold") +
  annotate("text", x = percentiles[3],   y = 15, label = "75th Percentile", angle = 90,
           vjust = 1.25, size = 6, fontface = "bold") +
  # Axes
  scale_x_continuous(name = "Relative Restatement", limits = c(x_lo, x_hi)) +
  scale_y_continuous(name = "Density (%)", labels = percent_format(scale = 1)) +
  # Theme
  theme_classic(base_size = 28) +
  theme(
    plot.title       = element_text(face = "bold", size = 32, hjust = 0.5),
    axis.title.x     = element_text(margin = margin(t = 10)),
    axis.title.y     = element_text(margin = margin(r = 10)),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(figures_dir, "Figure2a_Histogram.pdf"), width = 10.6, height = 6)

# ===================================================================
# 3) TIME TREND: EMISSIONS VS ACCOUNTING RESTATEMENTS
# ===================================================================

df <- df_scope1

# Emissions restatements (per-year share)
df_yearly_emission_restatement <- df %>%
  group_by(fiscal_year = fyear) %>%
  summarise(
    total_restatements = sum(restatedFromOriginal > 0, na.rm = TRUE),
    observation_count  = n(),
    .groups            = "drop"
  ) %>%
  mutate(
    deflated_restatements = total_restatements / observation_count,
    restatement_type      = "Emissions Restatements"
  ) %>%
  filter(fiscal_year >= 2010 & fiscal_year <= 2020)

# Accounting restatements (revenue/net income)
df_corporate_restatements <- read.csv(accounting_path, stringsAsFactors = FALSE) %>%
  mutate(
    deflated_restatements = Restatement / Reporting,
    restatement_type      = "Revenue or Net Income Restatements"
  ) %>%
  rename(fiscal_year = Year) %>%
  select(fiscal_year, deflated_restatements, restatement_type)

# Combine for comparison plot
df_combined <- bind_rows(df_yearly_emission_restatement, df_corporate_restatements)

# Comparison plot (presentation-ready)
ggplot(
  df_combined,
  aes(x = fiscal_year, y = deflated_restatements, group = restatement_type, color = restatement_type)
) +
  geom_point(size = 6) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = 2010:2020) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(x = "Year of Original Statement", y = "Percentage of Restatements") +
  scale_color_manual(values = c("Emissions Restatements" = "red",
                                "Revenue or Net Income Restatements" = "blue")) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title      = element_text(face = "bold", size = 21, hjust = 0.5),
    legend.position = "bottom",
    legend.title    = element_blank()
  )

ggsave(file.path(figures_dir, "proportionOfRestatements.pdf"), width = 10.6, height = 6)

# ===================================================================
# 4) EMISSIONS RESTATEMENT RATE WITH MEAN LINE (Figure1)
# ===================================================================

emiss <- df_combined %>%
  filter(restatement_type == "Emissions Restatements")

# Mean and year range for annotation/axes
mean_val <- mean(emiss$deflated_restatements, na.rm = TRUE)
year_min <- min(emiss$fiscal_year, na.rm = TRUE)
year_max <- max(emiss$fiscal_year, na.rm = TRUE)

ggplot(emiss, aes(x = fiscal_year, y = deflated_restatements)) +
  # Baseline at zero
  geom_hline(yintercept = 0, color = "black", size = 1) +
  # Actual trajectory
  geom_point(size = 6, color = "red") +
  geom_line(size = 1.5, color = "red") +
  # Dashed mean line
  geom_hline(yintercept = mean_val, linetype = "dashed", size = 1, alpha = 0.5) +
  # Label the mean (top-right)
  annotate(
    "label",
    x         = year_max - 0.2,             # nudge left from max year
    y         = mean_val + 0.05,            # slightly above the mean line
    label     = paste0("Mean = ", percent(mean_val)),
    hjust     = 1,
    vjust     = 0,
    size      = 5,
    fill      = "white",
    label.size = 0.3
  ) +
  # Axes, labels, theme
  scale_x_continuous(breaks = seq(2010, 2020, 1)) +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  labs(x = "Year of Original Statement", y = "Percentage of Restatements") +
  theme_minimal(base_size = 20) +
  theme(
    axis.line.y      = element_line(color = "black", size = 0.8),
    axis.line.x      = element_line(color = "black", size = 0.8),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  )

ggsave(file.path(figures_dir, "Figure1_PercentOfRestatement.pdf"), width = 10.6, height = 6)

# -------------------------------------------------------------------
# End of script
# -------------------------------------------------------------------
