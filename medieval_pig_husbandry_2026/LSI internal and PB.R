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

# -------------------------
# Load packages
# -------------------------
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(viridis)
library(gghalves)
library(grid)

# -------------------------
# Load updated LSI sheet
# -------------------------
lsi <- read_excel("LSI pigs.xlsx") %>%
  mutate(Site = str_trim(as.character(Site))) %>%
  filter(!is.na(Site))

# -------------------------
# Site order + viridis palette
# -------------------------
site_order <- c("Glastonbury", "Grimsby", "Huntingdon", "Maldon", "Reading", "Romsey", "St Albans")
sites <- unique(lsi$Site)
sites <- c(site_order[site_order %in% sites], sort(setdiff(sites, site_order)))

site_cols <- setNames(
  viridis::viridis(length(sites), option = "D", direction = 1, end = 1),
  sites
)

# -------------------------
# Long format
# Payne & Bull on the left, Internal on the right
# -------------------------
lsi_long <- lsi %>%
  select(Site, `Mean LSI (internal standard)`, `Mean LSI (P&B 1988)`) %>%
  pivot_longer(
    cols = c(`Mean LSI (P&B 1988)`, `Mean LSI (internal standard)`),
    names_to = "Standard",
    values_to = "LSI"
  ) %>%
  filter(!is.na(LSI)) %>%
  mutate(
    Site = factor(Site, levels = sites),
    Standard = factor(
      Standard,
      levels = c("Mean LSI (P&B 1988)", "Mean LSI (internal standard)"),
      labels = c("Payne & Bull 1988", "Internal")
    ),
    xpos = ifelse(Standard == "Payne & Bull 1988",
                  as.numeric(Site) - 0.18,
                  as.numeric(Site) + 0.18)
  )

alpha_map <- c("Payne & Bull 1988" = 0.75, "Internal" = 0.35)

# -------------------------
# Combined raincloud plot
# -------------------------
ggplot() +
  gghalves::geom_half_violin(
    data = filter(lsi_long, Standard == "Payne & Bull 1988"),
    aes(x = xpos, y = LSI, fill = Site, alpha = Standard, group = interaction(Site, Standard)),
    side = "l",
    trim = TRUE,
    scale = "width",
    colour = "black",
    linewidth = 0.25,
    width = 0.35
  ) +
  gghalves::geom_half_violin(
    data = filter(lsi_long, Standard == "Internal"),
    aes(x = xpos, y = LSI, fill = Site, alpha = Standard, group = interaction(Site, Standard)),
    side = "r",
    trim = TRUE,
    scale = "width",
    colour = "black",
    linewidth = 0.25,
    width = 0.35
  ) +
  geom_point(
    data = lsi_long,
    aes(x = xpos, y = LSI, colour = Site, alpha = Standard),
    position = position_jitter(width = 0.06, height = 0),
    size = 2
  ) +
  stat_summary(
    data = lsi_long,
    aes(x = xpos, y = LSI, group = interaction(Site, Standard)),
    fun.data = function(z) {
      data.frame(
        y = median(z, na.rm = TRUE),
        ymin = quantile(z, 0.25, na.rm = TRUE),
        ymax = quantile(z, 0.75, na.rm = TRUE)
      )
    },
    geom = "pointrange",
    colour = "black",
    linewidth = 0.7
  ) +
  scale_x_continuous(
    breaks = seq_along(sites),
    labels = sites
  ) +
  scale_fill_manual(values = site_cols, drop = FALSE) +
  scale_colour_manual(values = site_cols, drop = FALSE) +
  scale_alpha_manual(values = alpha_map) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(0.4, "cm"),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)
  ) +
  labs(
    x = "Site",
    y = "LSI",
    alpha = "Standard"
  )