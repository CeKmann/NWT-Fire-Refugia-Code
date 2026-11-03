library(ggplot2)
library(tidyr)
library(dplyr)

# ---------------------------
# Odds ratios
# ---------------------------

OR_results <- data.frame(
  Season = c("Calving", "Summer", "Early fall", "Late fall",
             "Early winter", "Mid winter", "Late winter"),
  Low =      c(0.90, 0.95, 0.97, 1.07, 1.22, 1.37, 1.57),
  Moderate = c(1.16, 1.23, 1.24, 1.18, 1.11, 1.07, 1.09),
  High =     c(0.91, 0.79, 0.75, 0.71, 0.66, 0.60, 0.49)
)


# ---------------------------
# Long format
# ---------------------------

OR_long <- OR_results %>%
  pivot_longer(
    cols = -Season,
    names_to = "Refugia",
    values_to = "OR"
  ) %>%
  mutate(
    Refugia = factor(
      Refugia,
      levels = c("Low", "Moderate", "High")
    ),
    logOR = log(OR),
    Season = factor(
      Season,
      levels = OR_results$Season
    ),
    Season_num = as.numeric(Season)
  )


# ---------------------------
# Grid values
# ---------------------------

OR_breaks <- c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6)


# ---------------------------
# Convert log OR to positive radius
# ---------------------------

offset <- min(OR_long$logOR)

OR_long <- OR_long %>%
  mutate(
    radius = logOR - offset
  )


# ---------------------------
# Season angles
# ---------------------------

n <- length(OR_results$Season)

angles <- data.frame(
  Season_num = 1:n,
  angle = seq(0, 2*pi, length.out = n + 1)[1:n]
)


# ---------------------------
# Coordinates for polygons
# ---------------------------

OR_plot <- OR_long %>%
  left_join(angles, by = "Season_num") %>%
  mutate(
    x = radius * sin(angle),
    y = radius * cos(angle)
  )

# ---------------------------
# Radial grid lines (spokes)
# ---------------------------

spokes <- angles %>%
  mutate(
    x = max(OR_plot$radius) * 1.02 * sin(angle),
    y = max(OR_plot$radius) * 1.02 * cos(angle),
    x0 = 0,
    y0 = 0
  )


# Close polygons AFTER coordinates exist

OR_plot_closed <- OR_plot %>%
  group_by(Refugia) %>%
  arrange(Season_num) %>%
  bind_rows(slice(., 1)) %>%
  ungroup()


# ---------------------------
# Circular grid
# ---------------------------

grid_circle <- expand.grid(
  OR = OR_breaks,
  angle = seq(0, 2*pi, length.out = 300)
) %>%
  mutate(
    radius = log(OR) - offset,
    x = radius * sin(angle),
    y = radius * cos(angle)
  )


# ---------------------------
# OR labels
# ---------------------------
OR_labels <- data.frame(
  OR = OR_breaks
) %>%
  filter(OR != 0.4) %>%
  mutate(
    radius = log(OR) - offset,
    x = radius,
    y = 0
  )

OR_labels_regular <- subset(OR_labels, OR != 1)

OR_label_one <- subset(OR_labels, OR == 1)


# ---------------------------
# Season labels
# ---------------------------

season_labels <- angles %>%
  mutate(
    radius = max(OR_plot$radius) * 1.15,
    x = radius * sin(angle),
    y = radius * cos(angle),
    label = OR_results$Season
  )


# ---------------------------
# Plot
# ---------------------------

Out <- ggplot() +
  
  # Grid circles
  
  geom_path(
    data = grid_circle,
    aes(
      x = x,
      y = y,
      group = OR
    ),
    colour = "grey80",
    linewidth = 0.5
  ) +
  
  # Bold OR = 1 reference circle
  
  geom_path(
    data = subset(grid_circle, OR == 1),
    aes(
      x = x,
      y = y,
      group = OR
    ),
    colour = "grey60",
    linewidth = 1.5
  ) +
  
  geom_segment(
    data = spokes,
    aes(
      x = x0,
      y = y0,
      xend = x,
      yend = y
    ),
    colour = "grey80",
    linewidth = 0.4
  ) +
  
  # Spider-web polygons (lines between points)
  
  geom_path(
    data = OR_plot_closed,
    aes(
      x = x,
      y = y,
      group = Refugia,
      colour = Refugia
    ),
    linewidth = 1.2
  ) +
  
  # Data points
  
  geom_point(
    data = OR_plot,
    aes(
      x = x,
      y = y,
      colour = Refugia
    ),
    size = 3
  ) +
  
  # Regular OR labels
  
  geom_text(
    data = OR_labels_regular,
    aes(
      x = x,
      y = y,
      label = OR
    ),
    size = 7,
    colour = "grey30",
    hjust = -0.3
  ) +
  
  # Bold OR = 1 label
  
  geom_text(
    data = OR_label_one,
    aes(
      x = x,
      y = y,
      label = OR
    ),
    size = 8,
    fontface = "bold",
    colour = "grey30",
    hjust = -0.3
  ) +
  
  # Season labels
  
  geom_text(
    data = season_labels,
    aes(
      x = x,
      y = y,
      label = label
    ),
    size = 8,
    fontface = "bold"
  ) +
  
  coord_equal(
    expand = TRUE,
    clip = "off"
  ) +
  
  scale_colour_manual(
    values = c(
      Low = "#FDE725",
      Moderate = "#35B779",
      High = "#31688E"
    ),
    breaks = c("Low", "Moderate", "High")
  ) +
  
  labs(
    colour = "Refugia probability"
  ) +
  
  theme_void() +
  
  theme(
    legend.position = "right",
    legend.title = element_text(
      size = 22,
      face = "bold"
    ),
    legend.text = element_text(
      size = 20
    )
  )

plot(Out)

ggsave(
  filename = "Z:/Christine/NWT Fire Refugia Project Z/Graphs/RSF/V2/Spider plots/Refugia_LMH_LogOdds_Spiderplot_V8.png",
  plot = Out,
  width = 20,
  height = 14,
  units = "in",
  dpi = 300,
  bg = "white"
)
