# -------------------------
# Install gghalves from CRAN archive if needed
# -------------------------
if (!requireNamespace("gghalves", quietly = TRUE)) {
  install.packages(
    "https://cran.r-project.org/src/contrib/Archive/gghalves/gghalves_0.1.4.tar.gz",
    repos = NULL,
    type = "source"
  )
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(viridis)
library(gghalves)

# -------------------------
# Load Excel
# -------------------------
data <- read_excel("dentine collagen.xlsx")

# -------------------------
# Long format + century parsing
# -------------------------
data_long <- data %>%
  pivot_longer(
    cols = c(`δ¹⁵N`, `δ¹³C`, `δ³⁴S`),
    names_to = "Isotope",
    values_to = "Value"
  ) %>%
  mutate(
    Isotope = recode(
      Isotope,
      `δ¹⁵N` = "d15N",
      `δ¹³C` = "d13C",
      `δ³⁴S` = "d34S"
    ),
    centuries = str_extract_all(Range, "\\d+"),
    start_c = as.numeric(sapply(centuries, `[`, 1)),
    end_c   = as.numeric(sapply(centuries, function(x) if (length(x) >= 2) x[2] else x[1])),
    mid_c   = (start_c + end_c) / 2
  ) %>%
  filter(!is.na(Value), !is.na(Site), !is.na(mid_c))

# -------------------------
# Century binning
# -------------------------
data_long <- data_long %>%
  mutate(
    PhaseBin = case_when(
      mid_c < 10.5 ~ "10th",
      mid_c < 11.5 ~ "11th",
      mid_c < 12.5 ~ "12th",
      mid_c < 13.5 ~ "13th",
      mid_c < 14.5 ~ "14th",
      mid_c < 15.5 ~ "15th",
      TRUE ~ "16th"
    ),
    PhaseBin = factor(
      PhaseBin,
      levels = c("10th", "11th", "12th", "13th", "14th", "15th", "16th")
    )
  )

# -------------------------
# Raincloud plot function — grey half violins
# -------------------------
plot_isotope_raincloud <- function(iso_code, y_lab) {
  
  d <- data_long %>% filter(Isotope == iso_code)
  
  ggplot(d, aes(x = PhaseBin, y = Value)) +
    gghalves::geom_half_violin(
      side = "r",
      fill = "grey85",
      colour = "grey85",
      trim = TRUE,
      scale = "width",
      alpha = 0.9
    ) +
    geom_point(
      aes(colour = Site),
      position = position_jitter(width = 0.12, height = 0),
      alpha = 0.8,
      size = 2.2
    ) +
    stat_summary(
      fun.data = function(z) {
        data.frame(
          y = median(z, na.rm = TRUE),
          ymin = quantile(z, 0.25, na.rm = TRUE),
          ymax = quantile(z, 0.75, na.rm = TRUE)
        )
      },
      geom = "pointrange",
      colour = "black",
      linewidth = 0.8
    ) +
    scale_colour_viridis(discrete = TRUE, option = "D", end = 0.9) +
    theme_classic(base_size = 12) +
    theme(
      panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.5),
      panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.title = element_text(face = "bold")
    ) +
    labs(
      x = "Century",
      y = y_lab,
      colour = "Site"
    )
}

# -------------------------
# Draw plots
# -------------------------
plot_isotope_raincloud("d15N", expression(delta^{15} * N[AIR] ~ ("\u2030")))
plot_isotope_raincloud("d13C", expression(delta^{13} * C[VPDB] ~ ("\u2030")))
plot_isotope_raincloud("d34S", expression(delta^{34} * S[VCDT] ~ ("\u2030")))