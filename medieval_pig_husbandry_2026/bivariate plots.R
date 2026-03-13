library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)
library(stringr)
library(tibble)


normalize_name <- function(x) {
  x <- chartr("⁰¹²³⁴⁵⁶⁷⁸⁹", "0123456789", as.character(x))
  x <- tolower(x)
  gsub("[^a-z0-9]+", "", x)
}

find_cols <- function(df) {
  nms <- normalize_name(names(df))
  list(
    site = names(df)[which(nms == "site")[1]],
    n15  = names(df)[which(nms == "15n")[1]],
    c13  = names(df)[which(nms == "13c")[1]],
    s34  = if ("34s" %in% nms) names(df)[which(nms == "34s")[1]] else NA_character_
  )
}


dentine_raw <- read_excel("dentine collagen.2.xlsx")
dc <- find_cols(dentine_raw)

dentine <- tibble(
  Site = str_trim(as.character(dentine_raw[[dc$site]])),
  d15N = as.numeric(dentine_raw[[dc$n15]]),
  d13C = as.numeric(dentine_raw[[dc$c13]]),
  d34S = if (!is.na(dc$s34)) as.numeric(dentine_raw[[dc$s34]]) else NA_real_
) %>%
  filter(!is.na(Site))


bone_raw <- read_excel("bone collagen.xlsx")
bc <- find_cols(bone_raw)

bone <- tibble(
  Site = str_trim(as.character(bone_raw[[bc$site]])),
  d15N = as.numeric(bone_raw[[bc$n15]]),
  d13C = as.numeric(bone_raw[[bc$c13]])
) %>%
  filter(!is.na(Site))


iqr_filter <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  i  <- q3 - q1
  x >= (q1 - 1.5 * i) & x <= (q3 + 1.5 * i)
}


dentine_CN <- dentine %>%
  group_by(Site) %>%
  filter(iqr_filter(d13C), iqr_filter(d15N)) %>%
  ungroup()

dentine_NS <- dentine %>%
  group_by(Site) %>%
  filter(!is.na(d34S)) %>%
  filter(iqr_filter(d15N), iqr_filter(d34S)) %>%
  ungroup()

bone_CN <- bone %>%
  group_by(Site) %>%
  filter(iqr_filter(d13C), iqr_filter(d15N)) %>%
  ungroup()


sites <- sort(unique(c(dentine$Site, bone$Site)))

site_cols <- setNames(
  viridis::viridis(length(sites), option = "D"),
  sites
)

dentine_CN$Site <- factor(dentine_CN$Site, levels = sites)
dentine_NS$Site <- factor(dentine_NS$Site, levels = sites)
bone_CN$Site    <- factor(bone_CN$Site, levels = sites)


theme_scatter <- theme_classic() +
  theme(
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
    panel.grid.minor = element_line(colour = "grey92", linewidth = 0.2)
  )


plot_scatter <- function(df, x, y, xlab, ylab, title = NULL) {
  ggplot(df, aes(.data[[x]], .data[[y]], colour = Site)) +
    stat_ellipse(type = "t", level = 0.69, linewidth = 0.9, alpha = 0.35) +
    geom_point(size = 2.2, alpha = 0.90) +
    scale_colour_manual(values = site_cols) +
    labs(x = xlab, y = ylab, title = title) +
    theme_scatter
}


p1 <- plot_scatter(
  dentine_CN, "d13C", "d15N",
  expression(delta^{13} * C[VPDB] ~ ("\u2030")),
  expression(delta^{15} * N[AIR] ~ ("\u2030")),
  "Dentine collagen"
)

p2 <- plot_scatter(
  dentine_NS, "d15N", "d34S",
  expression(delta^{15} * N[AIR] ~ ("\u2030")),
  expression(delta^{34} * S[VCDT] ~ ("\u2030")),
  "Dentine collagen"
)

p3 <- plot_scatter(
  bone_CN, "d13C", "d15N",
  expression(delta^{13} * C[VPDB] ~ ("\u2030")),
  expression(delta^{15} * N[AIR] ~ ("\u2030")),
  "Bone collagen"
)

p1
p2
p3