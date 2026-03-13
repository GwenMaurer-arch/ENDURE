
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(viridisLite)

# ----------------------------
# 1) Load Excel
# ----------------------------
df_raw <- read_excel("dentine collagen.xlsx")

# ----------------------------
# 2) Long format + century parsing + Period bins
# ----------------------------
df_long <- df_raw %>%
  pivot_longer(
    cols = c(`δ¹⁵N`, `δ¹³C`, `δ³⁴S`),
    names_to = "Isotope",
    values_to = "Value"
  ) %>%
  mutate(
    Isotope = recode(Isotope,
                     `δ¹⁵N` = "d15N",
                     `δ¹³C` = "d13C",
                     `δ³⁴S` = "d34S"),
    Site  = str_trim(as.character(Site)),
    Range = as.character(Range),
    
    centuries = str_extract_all(Range, "\\d+"),
    start_c   = as.numeric(sapply(centuries, `[`, 1)),
    end_c     = as.numeric(sapply(centuries, function(x) if (length(x) >= 2) x[2] else x[1])),
    mid_c     = (start_c + end_c) / 2
  ) %>%
  filter(!is.na(Value), !is.na(Site), !is.na(mid_c)) %>%
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
    Period = case_when(
      PhaseBin %in% c("10th","11th","12th","13th") ~ "Pre",
      PhaseBin %in% c("14th","15th")              ~ "Crises",
      PhaseBin %in% c("16th")                     ~ "Post",
      TRUE ~ NA_character_
    ),
    Period = factor(Period, levels = c("Pre","Crises","Post"))
  ) %>%
  filter(!is.na(Period))

# ----------------------------
# 3) Fixed orders (KEEP EMPTY SLOTS)
# ----------------------------
site_order   <- c("Glastonbury","Grimsby","Maldon","Huntingdon","Reading","Romsey","St Albans")
period_order <- c("Pre","Crises","Post")

siteperiod_levels <- as.vector(t(outer(site_order, period_order, paste, sep = " — ")))

n_slots       <- length(siteperiod_levels)                 # 21
x_positions   <- seq_len(n_slots)                          # 1..21
x_labels      <- rep(period_order, times = length(site_order))

# between site blocks (after each 3 phases): 3.5, 6.5, 9.5, ...
sep_positions <- seq(3.5, by = 3, length.out = length(site_order) - 1)

# ----------------------------
# 4) Colours aligned to site_order (St Albans ends yellow)
# ----------------------------
site_cols <- setNames(
  viridisLite::viridis(n = length(site_order), option = "D", end = 1),
  site_order
)

# ----------------------------
# 5) Plot function (POINTS + MEDIAN, NO JITTER)
# ----------------------------
plot_points_median_nojitter <- function(iso_code, ylab_expr) {
  
  d_real <- df_long %>%
    filter(Isotope == iso_code) %>%
    mutate(
      Site = factor(Site, levels = site_order),
      Period = factor(Period, levels = period_order),
      SitePeriod = factor(paste(Site, Period, sep = " — "), levels = siteperiod_levels),
      x = as.numeric(SitePeriod)
    )
  
  # medians
  meddf <- d_real %>%
    group_by(SitePeriod) %>%
    summarise(
      x   = first(x),
      med = median(Value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # lock full x-range without distorting y-range
  y_anchor <- median(d_real$Value, na.rm = TRUE)
  force_x  <- data.frame(x = x_positions, y = y_anchor)
  
  # y-limits with padding
  y_rng <- range(d_real$Value, na.rm = TRUE)
  pad   <- diff(y_rng) * 0.06
  if (!is.finite(pad) || pad == 0) pad <- 0.5
  ylim  <- c(y_rng[1] - pad, y_rng[2] + pad)
  
  ggplot(d_real, aes(x = x, y = Value)) +
    
    # keep full x-range
    geom_blank(data = force_x, aes(x = x, y = y), inherit.aes = FALSE) +
    
    # light uniform vertical lines at every slot
    geom_vline(xintercept = x_positions, colour = "grey85", linewidth = 0.4) +
    
    # ---- BLACK dividers between sites ----
  geom_vline(xintercept = sep_positions, colour = "black", linewidth = 0.7) +
    
    # raw points: NO jitter
    geom_point(
      aes(colour = Site),
      alpha = 0.95,
      size = 4
    ) +
    
    # median bar per slot
    geom_segment(
      data = meddf,
      aes(x = x - 0.22, xend = x + 0.22, y = med, yend = med),
      inherit.aes = FALSE,
      linewidth = 1.0,
      colour = "black"
    ) +
    
    scale_x_continuous(
      breaks = x_positions,
      labels = x_labels,
      limits = c(0.5, n_slots + 0.5),
      expand = c(0, 0)
    ) +
    
    scale_colour_manual(values = site_cols, breaks = site_order, drop = FALSE) +
    
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 4),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    
    coord_cartesian(ylim = ylim) +
    
    theme_classic(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.5),
      panel.grid.minor.y = element_blank(),
      
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.title.y = element_text(margin = margin(r = 10)),
      
      legend.title = element_text(face = "bold"),
      plot.margin  = margin(t = 6, r = 6, b = 10, l = 18)
    ) +
    labs(
      x = "Phase",
      y = ylab_expr,
      colour = "Site"
    )
}

# ----------------------------
# 6) Generate plots + save PDFs
# ----------------------------
p15N <- plot_points_median_nojitter("d15N", expression(delta^{15}*N[AIR]~("\u2030")))
p13C <- plot_points_median_nojitter("d13C", expression(delta^{13}*C[VPDB]~("\u2030")))
p34S <- plot_points_median_nojitter("d34S", expression(delta^{34}*S[VCDT]~("\u2030")))

ggsave("d15N_points_median_nojitter.pdf", p15N, width = 10, height = 6, units = "in")
ggsave("d13C_points_median_nojitter.pdf", p13C, width = 10, height = 6, units = "in")
ggsave("d34S_points_median_nojitter.pdf", p34S, width = 10, height = 6, units = "in")

p15N
p13C
p34S
