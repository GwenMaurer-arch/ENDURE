# Install packages first if needed:
# install.packages(c("readxl", "ggplot2", "viridis", "scales"))

library(readxl)
library(ggplot2)
library(viridis)
library(scales)

# Read in the Excel file
df <- read_excel("strontium.xlsx")

# Make Site a factor
df$Site <- factor(df$Site)

# Create plot
ggplot(df, aes(x = Site, y = strontium, fill = Site)) +
  geom_point(
    shape = 21,
    size = 2.5,
    colour = "black",
    stroke = 0.5
  ) +
  scale_fill_viridis_d(option = "viridis") +
  scale_y_continuous(
    limits = c(0.7080, 0.7115),   # change these to make axis longer
    labels = label_number(accuracy = 0.0001)
  ) +
  labs(
    x = "Site",
    y = expression({}^{87}*Sr/{}^{86}*Sr~ratio)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.minor = element_line(colour = "grey92", linewidth = 0.2),
    legend.position = "none"
  )