library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(scales)

data <- read_excel("dentine collagen.2.xlsx")

data_long <- data %>%
  pivot_longer(
    cols = c(`δ¹⁵N`, `δ¹³C`, `δ³⁴S`),
    names_to = "Isotope",
    values_to = "Value"
  ) %>%
  mutate(Isotope = recode(Isotope,
                          `δ¹⁵N` = "d15N",
                          `δ¹³C` = "d13C",
                          `δ³⁴S` = "d34S"))

iso_labs <- c(
  d15N = "delta^{15}*N",
  d13C = "delta^{13}*C",
  d34S = "delta^{34}*S"
)

ggplot(data_long, aes(x = Site, y = Value)) +
  geom_boxplot(
    aes(fill = Site),
    outlier.shape = NA,
    alpha = 0.6,
    show.legend = FALSE
  ) +
  geom_jitter(
    aes(colour = Site),
    width = 0.15,
    size = 2,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  scale_colour_viridis(discrete = TRUE, option = "D") +
  facet_wrap(
    ~ Isotope,
    scales = "free_y",
    labeller = as_labeller(iso_labs, label_parsed)
  ) +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  theme_classic() +
  labs(
    x = "Site",
    y = expression("("*"\u2030"*")")
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    # ---- light grid ----
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
    panel.grid.minor = element_line(colour = "grey92", linewidth = 0.2)
  )
