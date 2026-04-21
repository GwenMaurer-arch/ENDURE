library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)

# ---- Load data ----
df <- read_excel("sulfursr.xlsx")

# ---- Fix headers ----
colnames(df) <- df[1, ]
df <- df[-1, ]

# ---- Clean data ----
df <- df %>%
  rename(
    Site = Site,
    Sulfur = `δ³⁴S`,
    Sr = `Enamel Results`
  ) %>%
  mutate(
    Sulfur = as.numeric(Sulfur),
    Sr = as.numeric(Sr),
    Site = factor(Site)
  ) %>%
  filter(!is.na(Sulfur), !is.na(Sr), !is.na(Site))

# ---- Fixed Viridis colours ----
sites <- levels(df$Site)
viridis_cols <- viridis(length(sites))
names(viridis_cols) <- sites

# ---- Build temporary ellipse plot to get full ellipse ranges ----
ellipse_plot <- ggplot(df, aes(Sulfur, Sr, colour = Site)) +
  stat_ellipse(level = 0.95, linewidth = 1)

ellipse_data <- ggplot_build(ellipse_plot)$data[[1]]

# ---- Global axis limits based on ellipses, not points ----
x_limits <- range(c(df$Sulfur, ellipse_data$x), na.rm = TRUE)
y_limits <- range(c(df$Sr, ellipse_data$y), na.rm = TRUE)

# small extra padding
x_pad <- diff(x_limits) * 0.05
y_pad <- diff(y_limits) * 0.05

x_limits <- c(x_limits[1] - x_pad, x_limits[2] + x_pad)
y_limits <- c(y_limits[1] - y_pad, y_limits[2] + y_pad)

# ---- Highlight plots ----
for (s in sites) {
  
  p <- ggplot(df, aes(Sulfur, Sr)) +
    
    # Background points
    geom_point(colour = "grey80", size = 3, alpha = 0.6) +
    
    # Highlighted site points
    geom_point(
      data = df %>% filter(Site == s),
      aes(colour = Site),
      size = 4
    ) +
    
    # Highlighted site ellipse
    stat_ellipse(
      data = df %>% filter(Site == s),
      aes(colour = Site),
      level = 0.95,
      linewidth = 1.5
    ) +
    
    scale_colour_manual(values = viridis_cols) +
    coord_cartesian(xlim = x_limits, ylim = y_limits) +
    
    labs(
      title = paste("Site:", s),
      x = expression(delta^{34}*S~("\u2030")),
      y = expression({}^{87}*Sr/{}^{86}*Sr)
    ) +
    
    theme_minimal(base_size = 18) +
    theme(
      text = element_text(colour = "black"),
      axis.title = element_text(colour = "black"),
      axis.text = element_text(colour = "black"),
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  print(p)
  
  ggsave(
    paste0("highlight_95_", s, ".png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
}
