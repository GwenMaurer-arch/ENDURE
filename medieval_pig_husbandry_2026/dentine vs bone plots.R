library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(viridis)

bone_raw    <- read_excel("bone collagen.xlsx")
dentine_raw <- read_excel("dentine collagen.2.xlsx")

bone <- bone_raw %>%
  transmute(
    IDzoo  = as.character(IDzoo),
    Site   = str_trim(as.character(Site)),
    C_bone = suppressWarnings(as.numeric(`δ¹³C`)),
    N_bone = suppressWarnings(as.numeric(`δ¹⁵N`))
  ) %>%
  filter(!is.na(IDzoo), !is.na(Site))

dentine <- dentine_raw %>%
  transmute(
    IDzoo     = as.character(IDzoo),
    Site      = str_trim(as.character(Site)),
    C_dentine = suppressWarnings(as.numeric(`δ¹³C`)),
    N_dentine = suppressWarnings(as.numeric(`δ¹⁵N`))
  ) %>%
  filter(!is.na(IDzoo), !is.na(Site))

paired <- inner_join(bone, dentine, by = c("IDzoo", "Site"))


iqr_keep <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  i  <- q3 - q1
  x >= (q1 - 1.5 * i) & x <= (q3 + 1.5 * i)
}


site_order <- c("Glastonbury", "Grimsby", "Huntingdon", "Maldon", "Reading", "Romsey", "St Albans")

sites <- unique(paired$Site)
sites <- c(site_order[site_order %in% sites], sort(setdiff(sites, site_order)))

paired$Site <- factor(paired$Site, levels = sites)

site_cols <- setNames(
  viridis::viridis(length(sites), option = "D", direction = 1),
  sites
)


paired_C <- paired %>%
  group_by(Site) %>%
  filter(
    iqr_keep(C_dentine),
    iqr_keep(C_bone)
  ) %>%
  ungroup() %>%
  filter(is.finite(C_dentine), is.finite(C_bone))

paired_N <- paired %>%
  group_by(Site) %>%
  filter(
    iqr_keep(N_dentine),
    iqr_keep(N_bone)
  ) %>%
  ungroup() %>%
  filter(is.finite(N_dentine), is.finite(N_bone))


if (nrow(paired_C) >= 3) {
  r2_C <- summary(lm(C_bone ~ C_dentine, data = paired_C))$r.squared
  rng_C <- range(c(paired_C$C_dentine, paired_C$C_bone), na.rm = TRUE)
  
  p_C <- ggplot(paired_C, aes(C_dentine, C_bone, colour = Site)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.7) +
    coord_cartesian(xlim = rng_C, ylim = rng_C) +
    geom_point(size = 3, alpha = 0.9) +
    geom_smooth(method = "lm", se = FALSE, colour = "red", linewidth = 1) +
    scale_colour_manual(values = site_cols) +
    annotate(
      "text",
      x = rng_C[1], y = rng_C[2],
      hjust = 0, vjust = 1.1,
      label = paste0("R² = ", sprintf("%.2f", r2_C)),
      size = 4
    ) +
    theme_classic(base_size = 12) +
    theme(
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(colour = "grey92", linewidth = 0.2)
    ) +
    labs(
      x = expression(delta^{13} * C[VPDB] ~ "dentine (" * "\u2030" * ")"),
      y = expression(delta^{13} * C[VPDB] ~ "bone (" * "\u2030" * ")")
    )
  
  print(p_C)
}


if (nrow(paired_N) >= 3) {
  r2_N <- summary(lm(N_bone ~ N_dentine, data = paired_N))$r.squared
  rng_N <- range(c(paired_N$N_dentine, paired_N$N_bone), na.rm = TRUE)
  
  p_N <- ggplot(paired_N, aes(N_dentine, N_bone, colour = Site)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.7) +
    coord_cartesian(xlim = rng_N, ylim = rng_N) +
    geom_point(size = 3, alpha = 0.9) +
    geom_smooth(method = "lm", se = FALSE, colour = "red", linewidth = 1) +
    scale_colour_manual(values = site_cols) +
    annotate(
      "text",
      x = rng_N[1], y = rng_N[2],
      hjust = 0, vjust = 1.1,
      label = paste0("R² = ", sprintf("%.2f", r2_N)),
      size = 4
    ) +
    theme_classic(base_size = 12) +
    theme(
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(colour = "grey92", linewidth = 0.2)
    ) +
    labs(
      x = expression(delta^{15} * N[AIR] ~ "dentine (" * "\u2030" * ")"),
      y = expression(delta^{15} * N[AIR] ~ "bone (" * "\u2030" * ")")
    )
  
  print(p_N)
}