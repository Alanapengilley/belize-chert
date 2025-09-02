#load packages
req <- c("ggtern", "readr", "dplyr", "tidyr", "stringr", "readxl")
for(p in req){ if(!requireNamespace(p, quietly = TRUE)) install.packages(p) }
library(ggtern)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Load data
df <- read_excel("LAICPMS DATA/Geology LAICPMS.xlsx")

View(df)

GROUP_COL <- "Group"   # Change to your grouping column name

# --- Convert ppm → wt% and clean ---
df_clean <- df %>%
  transmute(
    !!GROUP_COL := .data[[GROUP_COL]],
    Al = as.numeric(.data[["Al2O3"]]) / 10000,
    Fe = as.numeric(.data[["Fe2O3"]]) / 10000,
    Mn = as.numeric(.data[["Mn"]]) / 10000
  ) %>%
  filter(if_all(c(Al, Fe, Mn), ~ !is.na(.x) & .x > 0)) %>%
  mutate(total = Al + Fe + Mn) %>%
  filter(total > 0) %>%
  mutate(
    Al = 100 * Al / total,
    Fe = 100 * Fe / total,
    Mn = 100 * Mn / total
  ) %>%
  # Keep only points inside ternary range
  filter(Al >= 0 & Al <= 100,
         Fe >= 0 & Fe <= 100,
         Mn >= 0 & Mn <= 100)

range(df_clean$Mn)
summary(df_clean)

#scale Mn

Mn_max <- max(df_clean$Mn, na.rm = TRUE)
scaling_factor <- 30 / Mn_max  # adjust so max Mn = 30%
df_plot <- df_clean %>%
  mutate(Mn_plot = Mn * scaling_factor) %>%
  mutate(total_plot = Al + Fe + Mn_plot) %>%
  mutate(
    Al_plot = 100 * Al / total_plot,
    Fe_plot = 100 * Fe / total_plot,
    Mn_plot = 100 * Mn_plot / total_plot
  )


#--Groups---
# Hydrothermal region
hydrothermal <- data.frame(
  Al = c(5, 15, 25, 20, 10),
  Fe = c(80, 70, 60, 85, 90),
  Mn = c(15, 15, 15, 5, 0)
)

# Non-hydrothermal terrestrial zone
non_hydrothermal <- data.frame(
  Al = c(30, 40, 50, 60, 50),
  Fe = c(50, 40, 30, 20, 40),
  Mn = c(20, 20, 20, 20, 10)
)


# --- Plot ternary diagram (color only) ---

p <- ggtern(df_plot, aes(x = Al_plot, y = Fe_plot, z = Mn_plot, color = .data[[GROUP_COL]])) +
  # Fill polygons first
  geom_polygon(data = hydrothermal, aes(x = Al, y = Fe, z = Mn), fill = "pink", alpha = 0.4, inherit.aes = FALSE) +
  geom_polygon(data = non_hydrothermal, aes(x = Al, y = Fe, z = Mn), fill = "yellow", alpha = 0.4, inherit.aes = FALSE) +
  
  # Add points
  geom_point(size = 2.5, alpha = 0.85) +
  
  # Labels and theme
  labs(
    title = "Al–Fe–Mn Ternary Diagram",
    T = "Al (wt%)",
    L = "Fe (wt%)",
    R = "Mn (wt%)",
    color = GROUP_COL
  ) +
  theme_bw() +
  theme(legend.position = "right")

print(p)
