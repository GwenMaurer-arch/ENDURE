library(tidyverse)
library(readxl)
library(viridis)
library(grid)   

# 1) Read original row-level data
raw <- read_excel("age size dentine.xlsx")

# 2) Bin ages into 4 meaningful groups
binned <- raw %>%
  mutate(
    Age = as.character(Age),
    Age_group = case_when(
      is.na(Age) ~ "Unknown",
      Age == "0" ~ "Unknown",
      
      str_detect(Age, "^8-12") ~ "Juvenile (8–12m)",
      
      # Subadult: 12–30 m (plus any 8- not 8-12, and 18-)
      str_detect(Age, "^12-") ~ "Subadult (12–30m)",
      str_detect(Age, "^18-") ~ "Subadult (12–30m)",
      str_detect(Age, "^8-")  ~ "Subadult (12–30m)",
      
      # Adult: strictly 30–52 m
      str_detect(Age, "^30-52") ~ "Adult (30–52m)",
      
      # Very old: anything beyond 52 m
      str_detect(Age, "^(52-|72-)") ~ "Old (>52m)",
      str_detect(Age, "^30-") ~ "Old (>52m)",
      
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(Age_group != "Unknown") %>%
  mutate(
    Age_group = factor(
      Age_group,
      levels = c(
        "Juvenile (8–12m)",
        "Subadult (12–30m)",
        "Adult (30–52m)",
        "Old (>52m)"
      )
    )
  )

# 3) Recalculate % per site (Unknown already removed)
site_pct <- binned %>%
  count(Site, Age_group, name = "N") %>%
  group_by(Site) %>%
  mutate(
    Total = sum(N),
    Percent = 100 * N / Total
  ) %>%
  ungroup()

# (Optional check – should be 100 for all sites)
site_pct %>%
  group_by(Site) %>%
  summarise(sum_percent = sum(Percent))

# 4) Stacked % bar chart with black axes + ticks
ggplot(site_pct, aes(x = Site, y = Percent, fill = Age_group)) +
  geom_col(width = 0.6) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  scale_fill_viridis_d(begin = 0, end = 1) +
  
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Black axes and ticks
    axis.line = element_line(colour = "black", linewidth = 0.6),
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    axis.ticks.length = unit(2, "mm"),
    
    axis.text.x = element_text(angle = 40, hjust = 1),
    legend.position = "right",
    legend.title = element_blank(),
    plot.margin = margin(6, 6, 6, 6)
  ) +
  labs(
    x = "Site",
    y = "% of Mandibles/Maxillae"
  )

# Optional save:
# ggsave("stacked_age_percent_4bins.png", width = 6.5, height = 4.5, dpi = 300, bg = "white")
