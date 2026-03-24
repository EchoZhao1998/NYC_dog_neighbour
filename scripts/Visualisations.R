# ══════════════════════════════════════════════════════════════════════════════
# Phase 3 — Q1 Visualisations
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(sf)
library(patchwork)
library(viridis)

# ── Load cleaned data ─────────────────────────────────────────────────────────
master_filtered        <- readRDS("data/cleaned/master_filtered.rds")
dogs_clean             <- readRDS("data/cleaned/dogs_clean.rds")
bites_per_year_borough <- readRDS("data/cleaned/bites_per_year_borough.rds")
pop_clean              <- readRDS("data/cleaned/nyc_borough_population.rds")

# ── Prepare ownership rate data ───────────────────────────────────────────────

# Aggregate licensed dogs by borough and year
dogs_borough_year <- dogs_clean |>
  filter(zipcode %in% master_filtered$zipcode) |>
  left_join(
    master_filtered |> st_drop_geometry() |> select(zipcode, borough),
    by = "zipcode"
  ) |>
  filter(!is.na(borough)) |>
  count(borough, extract_year, name = "licensed_dogs") |>
  rename(year = extract_year)

# Join with population to compute ownership rate
ownership_rate <- dogs_borough_year |>
  left_join(pop_clean, by = c("borough", "year")) |>
  mutate(
    dogs_per_1000 = licensed_dogs / population * 1000
  )

cat("Ownership rate table:\n")
ownership_rate |> arrange(borough, year) |> print(n = 30)

# Borough colour palette — consistent across all Q1 plots
borough_colours <- c(
  "Manhattan"     = "#2E86AB",
  "Brooklyn"      = "#A23B72",
  "Queens"        = "#F18F01",
  "Bronx"         = "#4CAF50",
  "Staten Island" = "#C73E1D"
)

# ── Plot 1A: Ownership rate bar chart ─────────────────────────────────────────

# Add a gap label for the missing years
gap_label <- tibble(
  x = 2020, y = max(ownership_rate$dogs_per_1000, na.rm = TRUE) * 0.5,
  label = "No licence\ndata 2019–21"
)

p1a <- ggplot(ownership_rate,
              aes(x = factor(year), y = dogs_per_1000, fill = borough)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  # Grey shading to mark the gap period
  annotate("rect", xmin = 3.5, xmax = 4.5,
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.6) +
  annotate("text", x = 4, y = max(ownership_rate$dogs_per_1000) * 0.7,
           label = "No licence\ndata 2019\u201321",
           size = 3, colour = "grey50", hjust = 0.5, lineheight = 0.9) +
  scale_fill_manual(values = borough_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Licensed dogs per 1,000 residents by borough",
    subtitle = "Annual licence extracts; 2019\u20132021 not published by NYC Open Data",
    x        = "Year",
    y        = "Licensed dogs per 1,000 residents",
    fill     = "Borough",
    caption  = "Sources: NYC Dog Licensing Dataset; ACS 1-year estimates"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

# ── Plot 1B: Bite counts line chart ───────────────────────────────────────────

# Add COVID annotation year
covid_year <- 2020

p1b <- ggplot(bites_per_year_borough,
              aes(x = year, y = bite_count,
                  colour = borough, group = borough)) +
  # COVID shading
  annotate("rect", xmin = 2019.5, xmax = 2020.5,
           ymin = -Inf, ymax = Inf,
           fill = "#FFF3CD", alpha = 0.8) +
  annotate("text", x = 2020, y = max(bites_per_year_borough$bite_count) * 0.92,
           label = "COVID-19\npeak", size = 3,
           colour = "grey40", hjust = 0.5, lineheight = 0.9) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = borough_colours) +
  scale_x_continuous(breaks = 2016:2023) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Reported dog bite incidents by borough",
    subtitle = "All years 2016\u20132023; Dataset B covers full period continuously",
    x        = "Year",
    y        = "Number of reported bites",
    colour   = "Borough",
    caption  = "Source: DOHMH Dog Bite Data"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ── Plot 1C: Choropleth — dog density 2022 ────────────────────────────────────

# Use 2022 as the most recent year with large licensing count
dogs_2022 <- dogs_clean |>
  filter(extract_year == 2022) |>
  count(zipcode, name = "dogs_2022")

map_2022 <- master_filtered |>
  left_join(dogs_2022, by = "zipcode") |>
  mutate(
    dogs_2022   = replace_na(dogs_2022, 0),
    density_2022 = dogs_2022 / (as.numeric(ALAND20) / 1e6)
  )

p1c <- ggplot(map_2022) +
  geom_sf(aes(fill = density_2022), colour = "white", linewidth = 0.15) +
  scale_fill_viridis_c(
    option    = "plasma",
    name      = "Dogs per km\u00b2",
    trans     = "sqrt",           # sqrt transform reduces extreme skew
    na.value  = "grey80",
    labels    = scales::comma
  ) +
  labs(
    title    = "Dog ownership density across NYC zipcodes (2022)",
    subtitle = "Square-root scale; darker = higher concentration",
    caption  = "Sources: NYC Dog Licensing Dataset; US Census TIGER/Line ZCTAs"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    legend.position = "right"
  )

# ── Preview each plot individually ────────────────────────────────────────────
print(p1a)
print(p1b)
print(p1c)

# ── Save plots ────────────────────────────────────────────────────────────────
ggsave("plots/plot1a_ownership_rate.png", p1a,
       width = 8, height = 5, dpi = 300, bg = "white")
ggsave("plots/plot1b_bite_counts.png",    p1b,
       width = 8, height = 5, dpi = 300, bg = "white")
ggsave("plots/plot1c_density_map.png",    p1c,
       width = 7, height = 6, dpi = 300, bg = "white")

cat("Q1 plots saved.\n")



# ── Plot 1A fix — narrow gap indicator ───────────────────────────────────────
p1a <- ggplot(ownership_rate,
              aes(x = factor(year), y = dogs_per_1000, fill = borough)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  annotate("rect", xmin = 3.4, xmax = 3.6,          # narrow gap between 2018 and 2022
           ymin = -Inf, ymax = Inf,
           fill = "grey70", alpha = 0.5) +
  annotate("text", x = 3.5,
           y = max(ownership_rate$dogs_per_1000) * 0.85,
           label = "2019\u201321\nno data",
           size = 2.8, colour = "grey30", hjust = 0.5, lineheight = 0.9) +
  scale_fill_manual(values = borough_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Licensed dogs per 1,000 residents by borough",
    subtitle = "Annual licence extracts; 2019\u20132021 not published by NYC Open Data",
    x        = "Year",
    y        = "Licensed dogs per 1,000 residents",
    fill     = "Borough",
    caption  = "Sources: NYC Dog Licensing Dataset; ACS 1-year estimates"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, colour = "grey40"),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

# ── Plot 1B fix — correct label to "dip" ─────────────────────────────────────
p1b <- ggplot(bites_per_year_borough,
              aes(x = year, y = bite_count,
                  colour = borough, group = borough)) +
  annotate("rect", xmin = 2019.5, xmax = 2020.5,
           ymin = -Inf, ymax = Inf,
           fill = "#FFF3CD", alpha = 0.8) +
  annotate("text", x = 2020,
           y = max(bites_per_year_borough$bite_count) * 0.92,
           label = "COVID-19\nreporting dip",
           size = 2.8, colour = "grey40", hjust = 0.5, lineheight = 0.9) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = borough_colours) +
  scale_x_continuous(breaks = 2016:2023) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Reported dog bite incidents by borough",
    subtitle = "All years 2016\u20132023; Dataset B covers full period continuously",
    x        = "Year",
    y        = "Number of reported bites",
    colour   = "Borough",
    caption  = "Source: DOHMH Dog Bite Data"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "grey40"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

# Re-save both
ggsave("plots/plot1a_ownership_rate.png", p1a,
       width = 8, height = 5, dpi = 300, bg = "white")
ggsave("plots/plot1b_bite_counts.png",    p1b,
       width = 8, height = 5, dpi = 300, bg = "white")
cat("Plots 1A and 1B re-saved.\n")


# Check gap index inputs
cat("=== Dog density range ===\n")
summary(master_filtered$dog_density)

cat("\n=== n_runs distribution ===\n")
master_filtered |> st_drop_geometry() |>
  count(n_runs) |> arrange(n_runs)

cat("\n=== dogs_per_run for ZCTAs with runs ===\n")
master_filtered |>
  st_drop_geometry() |>
  filter(n_runs > 0) |>
  group_by(borough) |>
  summarise(
    median_dogs_per_run = median(dogs_per_run, na.rm = TRUE),
    total_runs          = sum(n_runs),
    n_zctas_with_runs   = n()
  ) |>
  arrange(desc(median_dogs_per_run))



# ══════════════════════════════════════════════════════════════════════════════
# Phase 3 — Q2 Visualisations
# ══════════════════════════════════════════════════════════════════════════════

# ── Compute gap index ─────────────────────────────────────────────────────────

master_filtered <- master_filtered |>
  mutate(
    # Normalise both variables 0-1 across all ZCTAs
    density_scaled = (dog_density - min(dog_density, na.rm = TRUE)) /
      (max(dog_density, na.rm = TRUE) - min(dog_density, na.rm = TRUE)),
    runs_scaled    = (n_runs - min(n_runs)) /
      (max(n_runs) - min(n_runs)),
    # Gap index: high density + low run access = high gap
    gap_index      = density_scaled * (1 - runs_scaled),
    # Access category for Plot 2A
    run_access     = case_when(
      n_runs == 0 ~ "No runs",
      n_runs == 1 ~ "1 run",
      n_runs >= 2 ~ "2+ runs"
    ) |> factor(levels = c("No runs", "1 run", "2+ runs"))
  )

# Borough colour palette (reuse from Q1)
borough_colours <- c(
  "Manhattan"     = "#2E86AB",
  "Brooklyn"      = "#A23B72",
  "Queens"        = "#F18F01",
  "Bronx"         = "#4CAF50",
  "Staten Island" = "#C73E1D"
)

# ── Plot 2A: Choropleth — dog density + run locations overlaid ────────────────

# Compute run centroids for overlay dots
dog_runs_centroids <- dog_runs_sf |>
  st_transform(st_crs(master_filtered)) |>
  st_centroid()

p2a <- ggplot() +
  geom_sf(data  = master_filtered,
          aes(fill = dog_density),
          colour = "white", linewidth = 0.15) +
  geom_sf(data   = dog_runs_centroids,
          colour = "#4CAF50",
          size   = 1,
          alpha  = 0.85) +
  scale_fill_viridis_c(
    option   = "plasma",
    name     = "Dogs per km\u00b2",
    trans    = "sqrt",
    na.value = "grey80",
    labels   = scales::comma
  ) +
  labs(
    title    = "Dog ownership density and off-leash run locations across NYC",
    subtitle = "Green dots = dog run locations; darker areas = higher dog density",
    caption  = "Sources: NYC Dog Licensing Dataset; NYC Parks Dog Runs; US Census TIGER/Line"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "grey40"),
    legend.position  = "right",
    plot.margin      = margin(10, 10, 10, 10)
  )

# ── Plot 2B: Bar chart — dogs per run by borough ──────────────────────────────

# Borough-level summary: total dogs / total runs (only where runs exist)
borough_gap <- master_filtered |>
  st_drop_geometry() |>
  filter(!is.na(borough)) |>
  group_by(borough) |>
  summarise(
    total_dogs       = sum(total_dogs),
    total_runs       = sum(n_runs),
    pct_zctas_no_run = mean(n_runs == 0) * 100
  ) |>
  mutate(
    dogs_per_run = if_else(total_runs > 0,
                           total_dogs / total_runs,
                           NA_real_)
  ) |>
  arrange(desc(dogs_per_run))

p2b <- ggplot(borough_gap |> filter(!is.na(dogs_per_run)),
              aes(x = reorder(borough, dogs_per_run),
                  y = dogs_per_run,
                  fill = borough)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = scales::comma(round(dogs_per_run))),
            hjust  = -0.15,
            size   = 3.5,
            colour = "grey30") +
  coord_flip() +
  scale_fill_manual(values = borough_colours, guide = "none") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18)),
    labels = scales::comma
  ) +
  labs(
    title    = "Licensed dogs per off-leash run by borough",
    subtitle = "Higher = more dogs competing for each run space",
    x        = NULL,
    y        = "Licensed dogs per run",
    caption  = "Sources: NYC Dog Licensing Dataset; NYC Parks Dog Runs"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, colour = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

# ── Plot 2C: Scatter — dog density vs n_runs, coloured by borough ─────────────

# Annotate worst-gap ZCTAs (high density, zero runs)
top_gap <- master_filtered |>
  st_drop_geometry() |>
  filter(!is.na(borough)) |>
  arrange(desc(gap_index)) |>
  slice_head(n = 5)

p2c <- ggplot(
  master_filtered |> st_drop_geometry() |> filter(!is.na(borough)),
  aes(x = dog_density, y = n_runs, colour = borough)
) +
  # Highlight the "high density, no runs" danger zone
  annotate("rect",
           xmin = median(master_filtered$dog_density, na.rm = TRUE),
           xmax = Inf, ymin = -Inf, ymax = 0.5,
           fill = "#FFEEBA", alpha = 0.4) +
  annotate("text",
           x = max(master_filtered$dog_density, na.rm = TRUE) * 0.75,
           y = 0.3,
           label = "High density,\nno runs",
           size = 3, colour = "grey40", hjust = 0.5) +
  geom_jitter(size = 2, alpha = 0.75, width = 0, height = 0.15) +
  geom_text(data  = top_gap,
            aes(label = zipcode),
            size  = 2.8,
            vjust = -0.8,
            fontface = "bold") +
  scale_colour_manual(values = borough_colours) +
  scale_x_continuous(labels = scales::comma,
                     trans  = "sqrt") +
  scale_y_continuous(breaks = 0:4) +
  labs(
    title    = "Dog ownership density vs. number of off-leash runs per ZCTA",
    subtitle = "Top 5 highest-gap zipcodes labelled; x-axis square-root scaled",
    x        = "Dog density (dogs per km\u00b2, sqrt scale)",
    y        = "Number of off-leash runs",
    colour   = "Borough",
    caption  = "Sources: NYC Dog Licensing Dataset; NYC Parks Dog Runs"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 9, colour = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ── Plot 2D: Gap index choropleth ─────────────────────────────────────────────

p2d <- ggplot(master_filtered) +
  geom_sf(aes(fill = gap_index),
          colour = "white", linewidth = 0.15) +
  scale_fill_viridis_c(
    option   = "inferno",
    name     = "Gap index",
    na.value = "grey80"
  ) +
  labs(
    title    = "Infrastructure gap index across NYC zipcodes",
    subtitle = "High score = high dog density with few or no off-leash spaces",
    caption  = "Gap index = normalised dog density \u00d7 (1 \u2212 normalised run access)"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 9, colour = "grey40"),
    legend.position = "right",
    plot.margin     = margin(10, 10, 10, 10)
  )

# ── Print and save all ────────────────────────────────────────────────────────

print(p2a)
print(p2b)
print(p2c)
print(p2d)

ggsave("plots/plot2a_density_runs_overlay.png", p2a,
       width = 7, height = 6, dpi = 300, bg = "white")
ggsave("plots/plot2b_dogs_per_run_borough.png", p2b,
       width = 7, height = 4, dpi = 300, bg = "white")
ggsave("plots/plot2c_density_vs_runs_scatter.png", p2c,
       width = 8, height = 5, dpi = 300, bg = "white")
ggsave("plots/plot2d_gap_index_map.png", p2d,
       width = 7, height = 6, dpi = 300, bg = "white")

cat("Q2 plots saved.\n")


# Recompute top_gap cleanly
top_gap <- master_filtered |>
  st_drop_geometry() |>
  filter(!is.na(borough)) |>
  arrange(desc(gap_index)) |>
  slice_head(n = 5)

p2c <- ggplot(
  master_filtered |> st_drop_geometry() |> filter(!is.na(borough)),
  aes(x = dog_density, y = n_runs, colour = borough)
) +
  # Highlight danger zone
  annotate("rect",
           xmin = median(master_filtered$dog_density, na.rm = TRUE),
           xmax = Inf, ymin = -0.4, ymax = 0.4,
           fill = "#FFEEBA", alpha = 0.5) +
  annotate("text",
           x     = max(master_filtered$dog_density, na.rm = TRUE) * 0.75,
           y     = 0,
           label = "High density, no runs",
           size  = 3, colour = "grey40", hjust = 0.5) +
  # Use geom_point with NO jitter — exact coordinates
  geom_point(size = 2, alpha = 0.75, position = position_dodge2(width = 0)) +
  # Labels only on top_gap — nudge vertically only
  geom_text(
    data     = top_gap,
    aes(label = zipcode),
    size     = 2.8,
    nudge_y  = 0.25,          # move label above dot only
    nudge_x  = 0,             # no horizontal movement
    fontface = "bold"
  ) +
  scale_colour_manual(values = borough_colours) +
  scale_x_continuous(
    labels = scales::comma,
    trans  = "sqrt"
  ) +
  scale_y_continuous(
    breaks = 0:4,
    limits = c(-0.5, 4.5)     # give room for labels at top
  ) +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  labs(
    title    = "Dog ownership density vs. number of off-leash runs per ZCTA",
    subtitle = "Top 5 highest gap-index zipcodes labelled; x-axis square-root scaled",
    x        = "Dog density (dogs per km\u00b2, sqrt scale)",
    y        = "Number of off-leash runs",
    colour   = "Borough",
    caption  = "Sources: NYC Dog Licensing Dataset; NYC Parks Dog Runs"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "grey40"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

print(p2c)
ggsave("plots/plot2c_density_vs_runs_scatter.png", p2c,
       width = 8, height = 5, dpi = 300, bg = "white")


# Fix 3 — legend symbol size on 2C

p2c <- ggplot(
  master_filtered |> st_drop_geometry() |> filter(!is.na(borough)),
  aes(x = dog_density, y = n_runs, colour = borough)
) +
  annotate("rect",
           xmin = median(master_filtered$dog_density, na.rm = TRUE),
           xmax = Inf, ymin = -0.4, ymax = 0.4,
           fill = "#FFEEBA", alpha = 0.5) +
  annotate("text",
           x     = max(master_filtered$dog_density, na.rm = TRUE) * 0.75,
           y     = -0.2,
           label = "High density, no runs",
           size  = 2, colour = "grey40", hjust = 0.5) +
  # Jitter Y only — keeps x coordinates exact on sqrt scale
  geom_point(
    size     = 1.5,
    alpha    = 0.75,
    position = position_jitter(width = 0, height = 0.2, seed = 42)
    # seed = 42 fixes the random positions — same every render
  ) +
  # Labels use true coordinates — no jitter on these
  geom_text(
    data     = top_gap,
    aes(label = paste0(zipcode, "\n(", round(dog_density), "/km\u00b2)")),
    size     = 1.5,
    nudge_y  = 0.35,
    nudge_x  = 0,
    fontface = "bold",
    lineheight = 0.85
  ) +
  scale_colour_manual(values = borough_colours) +
  scale_x_continuous(
    labels = scales::comma,
    trans  = "sqrt"
  ) +
  scale_y_continuous(
    breaks = 0:4,
    limits = c(-0.5, 4.8)
  ) +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  labs(
    title    = "Dog ownership density vs. number of off-leash runs per ZCTA",
    subtitle = "Top 5 highest gap-index zipcodes labelled with density; vertical jitter applied to show overlapping points; x-axis square-root scaled",
    x        = "Dog density (dogs per km\u00b2, sqrt scale)",
    y        = "Number of off-leash runs",
    colour   = "Borough",
    caption  = "Sources: NYC Dog Licensing Dataset; NYC Parks Dog Runs"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "grey40"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

print(p2c)
ggsave("plots/plot2c_density_vs_runs_scatter.png", p2c,
       width = 8, height = 5, dpi = 300, bg = "white")