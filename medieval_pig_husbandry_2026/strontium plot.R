library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggbeeswarm)
library(scales)


df <- read_excel("sulfursr.xlsx")

colnames(df) <- df[1, ]
df <- df[-1, ]


df <- df %>%
  rename(
    Site = Site,
    Sr = `Enamel Results`
  ) %>%
  mutate(
    Sr = as.numeric(Sr),
    Site = factor(Site)
  ) %>%
  filter(!is.na(Sr), !is.na(Site))


sites <- levels(df$Site)
viridis_cols <- viridis(length(sites))
names(viridis_cols) <- sites


p <- ggplot(df, aes(x = Site, y = Sr, fill = Site)) +
  

  geom_boxplot(
    width = 0.3,
    outlier.shape = NA,
    fill = "white",
    colour = "black",
    linewidth = 0.5
  ) +
  
  geom_beeswarm(
    shape = 21,
    fill = viridis_cols[df$Site],
    colour = "black",
    stroke = 0.5,
    size = 2.6,
    alpha = 1,
    cex = 2.5,
    priority = "density",
    groupOnX = TRUE
  ) +
  
  scale_fill_manual(values = viridis_cols) +
  
  # 4 decimal axis
  scale_y_continuous(
    breaks = seq(0.708, 0.712, by = 0.001),
    labels = number_format(accuracy = 0.0001)
  ) +
  
  labs(
    x = "Site",
    y = expression({}^{87}*Sr/{}^{86}*Sr)
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(colour = "grey85"),
    panel.grid.minor = element_line(colour = "grey92"),
    legend.position = "none"
  )

print(p)

ggsave(
  "sr_boxplot_beeswarm.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)
