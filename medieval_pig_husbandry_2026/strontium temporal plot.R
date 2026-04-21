library(readxl)
library(dplyr)
library(ggplot2)
library(ggh4x)

df <- read_excel("sulfursr.xlsx")

colnames(df) <- df[1, ]
df <- df[-1, ]

df <- df %>%
  rename(
    Site = Site,
    Century = Century,
    Sr = `Enamel Results`
  ) %>%
  mutate(
    Sr = as.numeric(Sr),
    Century_num = as.numeric(sub("^([0-9]+).*", "\\1", Century)),
    Site = factor(Site)
  ) %>%
  filter(!is.na(Century_num), !is.na(Sr))

century_labels <- function(x) {
  ifelse(x %% 10 == 1 & x != 11, paste0(x, "st"),
         ifelse(x %% 10 == 2 & x != 12, paste0(x, "nd"),
                ifelse(x %% 10 == 3 & x != 13, paste0(x, "rd"),
                       paste0(x, "th"))))
}

y_min <- min(df$Sr, na.rm = TRUE)
y_max <- max(df$Sr, na.rm = TRUE)
y_range <- y_max - y_min
y_limits <- c(y_min - 0.15*y_range, y_max + 0.15*y_range)


bands <- data.frame(
  ymin = c(0.7075, 0.7085, 0.7095),
  ymax = c(0.7090, 0.7105, 0.7115),
  geology = factor(c("Chalk", "Clay / Alluvial", "Mixed lithologies"))
)

band_cols <- c(
  "Chalk" = "#bddf26",
  "Clay / Alluvial" = "#2e6f8e",
  "Mixed lithologies" = "#482173"
)

p <- ggplot(df, aes(x = Century_num, y = Sr)) +
  
  geom_rect(
    data = bands,
    aes(xmin = -Inf, xmax = Inf,
        ymin = ymin, ymax = ymax,
        fill = geology),
    alpha = 0.25,
    inherit.aes = FALSE
  ) +
  
  geom_point(
    colour = "black",
    size = 2.5
  ) +
  

  facet_wrap2(~Site, ncol = 3, axes = "all") +
  
  scale_fill_manual(
    values = band_cols,
    name = "Geological Zones"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(df$Century_num), max(df$Century_num), 1),
    labels = century_labels,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  coord_cartesian(ylim = y_limits) +
  
  labs(
    x = "Century",
    y = expression({}^{87}*Sr/{}^{86}*Sr)
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    text = element_text(colour = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 12, face = "bold"),
    
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.minor = element_blank(),
    
    legend.position = "bottom"
  )

print(p)

ggsave("Sr_time_panel_axes_all.png", p,
       width = 11, height = 8, dpi = 300)