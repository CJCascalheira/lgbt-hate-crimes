# Dependencies
library(cowplot)
library(tidyverse)
library(viridis)
library(gganimate)
library(gifski)
library(fiftystater)
library(sf)
library(transformr)

# Import data
hate <- read_csv("data/hate_crimes.csv")
couples <- read_csv("data/2010-census-same-sex-couples-state.csv")

# Set defaults
theme_set(theme_minimal())

# SO HATE CRIMES OVER TIME ------------------------------------------------

# Select sexual orientation
hate_so <- hate %>%
  filter(bias == "sexual_orientation")

# Total SO incidents by year
so_total_year <- hate_so %>%
  group_by(year) %>%
  summarize(total_so = sum(incidents))

# Total SO hate crimes from 2006-2017
so_total <- sum(so_total_year$total_so)

# Total sexual orientation hate crimes over time
so_crimes_time <- ggplot(so_total_year, aes(x = year, y = total_so)) +
  geom_line(color = "#440154FF", size = 1.25) +
  scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(0, 1400),
                     expand = c(0, 0), labels = scales::comma) +
  scale_x_continuous(breaks = seq(2006, 2017, 1), limits = c(2006, 2017),
                     minor_breaks = seq(1, 11, 1)) +
  labs(y = "Incidents",
       x = "Year",
       title = "Hate Crimes Based on Sexual Orientation",
       subtitle = "United States, 2006—2017",
       caption = "Source: FBI UCR Hate Crime Statistics")

# Save plot
ggsave("sexual-orientation-hate-crimes-time.png", plot = so_crimes_time, 
       path = "data/results/", width = 6.5, height = 5)

# Animated line graph
so_crimes_time_animate <- ggplot(so_total_year, aes(x = year, y = total_so)) +
  geom_line(color = "#440154FF", size = 1.25) +
  geom_point(size = 3) +
  geom_point(size = 1.5, color = "white") +
  scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(0, 1400),
                     expand = c(0, 0), labels = scales::comma) +
  scale_x_continuous(breaks = seq(2006, 2017, 1), limits = c(2006, 2017),
                     minor_breaks = seq(1, 11, 1)) +
  labs(y = "Incidents",
       x = "Year",
       title = "Hate Crimes Based on Sexual Orientation",
       subtitle = "United States, 2006—2017",
       caption = "Source: FBI UCR Hate Crime Statistics") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16)) +
  transition_reveal(year)

# Save animation
animate(so_crimes_time_animate, width = 700, height = 500)
anim_save("sexual-orientation-hate-crimes-time.gif",
          animation = last_animation(), path = "data/results/")

# PROPORTION OF HATE CRIMES -----------------------------------------------

# Grand total incidents by year
all_total_year <- hate %>%
  group_by(bias, year) %>%
  summarize(total = sum(incidents)) %>%
  group_by(year) %>%
  summarize(total_incidents = sum(total)) 

# Grand total hate crimes 2006-2017
all_total <- sum(all_total_year$total_incidents)

# Overall proportion of SO 2006-2017
so_total / all_total

# LGBT total incidents 2006-2017
lgbt_total <- hate %>%
  group_by(bias) %>%
  summarize(total = sum(incidents)) %>%
  mutate(bias = fct_collapse(
    factor(bias),
    lgbt = c("gender_identity", "sexual_orientation")
  )) %>%
  group_by(bias) %>%
  summarize(total = sum(total)) %>%
  filter(bias == "lgbt") %>%
  pull()

# LGB vs Straight Individuals
lgb_v_straight <- data.frame(sexuality = c("Lesbian, Gay, Bisexual", "Straight"),
                             percent = c(0.035, 0.965)) %>%
  ggplot(aes(x = "", y = percent, fill = sexuality)) +
  geom_col() +
  geom_col(color = "white", size = 1, show.legend = FALSE) +
  coord_polar("y") +
  geom_text(y = .89, x = 1.8, label = "3.5%", color = "#26828EFF", size = 10) +
  geom_text(y = .87, x = 1.74, label = "LGB", color = "#26828EFF", size = 5) +
  geom_text(y = .89, x = -.6, label = "96.5%", color = "#440154FF", size = 10) +
  geom_text(y = .905, x = -.7, label = "Straight", color = "#440154FF", size = 5) +
  scale_fill_manual(values = c("#26828EFF", "#440154FF")) +
  labs(caption = "Source: Gates, G. J. (2011). The Williams Institute",
       fill = "",
       title = "Proportion of Population Identifying as LGB",
       subtitle = "United States, 2010") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

# LGB vs Other Incidents
lgb_v_other <- data.frame(type = c("Lesbian, Gay, Bisexual", "Other"),
                          n = c(so_total, all_total)) %>%
  ggplot(aes(x = "", y = n, fill = type)) +
  geom_col() +
  geom_col(color = "white", size = 1, show.legend = FALSE) +
  coord_polar("y") +
  geom_text(y = 83000, x = 1.8, label = "18.1%", color = "#26828EFF", size = 10) +
  geom_text(y = 81100, x = 1.74, label = "LGB", color = "#26828EFF", size = 5) +
  geom_text(y = 83000, x = -.6, label = "81.9%", color = "#440154FF", size = 10) +
  geom_text(y = 84400, x = -.7, label = "Other", color = "#440154FF", size = 5) +
  scale_fill_manual(values = c("#26828EFF", "#440154FF")) +
  labs(caption = "Source: FBI UCR Hate Crime Statistics",
       fill = "",
       title = "Proportion of Hate Crimes Directed at LGB",
       subtitle = "United States, 2006—2017") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

# Combine proportional plots
lgb_pop_hate <- plot_grid(lgb_v_straight, lgb_v_other)

# Save plot
ggsave("lgb-population-vs-hate-crimes.png", plot = lgb_pop_hate,
       path = "data/results/", width = 10, height = 5)

# OTHER VS. SO BY YEAR ----------------------------------------------------

# Other hate crimes by year
other_total_year <- hate %>%
  filter(bias != "sexual_orientation") %>%
  group_by(bias, year) %>%
  summarize(total = sum(incidents)) %>%
  group_by(year) %>%
  summarize(total_incidents = sum(total))

# Total vs. sexual orientation by year
hate_other_v_so <- other_total_year %>%
  left_join(so_total_year, by = "year") %>%
  gather(key = totals, value = n, -year) %>%
  mutate(
    year = as.integer(year),
    totals = fct_recode(
      factor(totals),
      "Other" = "total_incidents",
      "Sexual Orientation" = "total_so"
  )) %>%
  ggplot(aes(x = totals, y = n, fill = totals)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0, 7000, 1000),
                     labels = scales::comma) +
  scale_fill_manual(values = c("#440154FF", "#26828EFF")) +
  labs(title = "Hate Crimes by Year: {frame_time}",
       fill = "",
       x = "",
       y = "Incidents",
       caption = "Source: FBI UCR Hate Crime Statistics") +
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 20)) +
  transition_time(year)

# Save animation
animate(hate_other_v_so, width = 700, height = 500)
anim_save("hate-crime-incidents-other-v-sexual-orientation.gif",
          animation = last_animation(), path = "data/results/")

# CREATE BASIC SF VECTOR MAP ----------------------------------------------

# Convert point-based data frame into simple features
sf_fifty <- st_as_sf(fifty_states, coords = c("long", "lat")) %>%
  # Group simple features and transform into polygons
  group_by(id, piece) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  # Group polygons into multipolygons
  group_by(id) %>%
  summarize()

# Set projection method
st_crs(sf_fifty) <- 4326

# Render vector map
ggplot(sf_fifty) +
  geom_sf() +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100")

# MAPS OVER TIME ----------------------------------------------------------

# Prepare key column
sf_fifty$id <- str_to_title(sf_fifty$id)
couples$state <- str_to_title(couples$state)

# Merge data frames
sf_hate <- left_join(sf_fifty, hate, by = c("id" = "state"))
sf_couples <- left_join(sf_fifty, couples, by = c("id" = "state"))

# Render choropleth for population
same_sex_couples_state_2010 <- ggplot(sf_couples) +
  geom_sf(aes(fill = n_couples), color = "white") +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100", ndiscr = 0) +
  scale_fill_viridis_c(option = "viridis", labels = scales::comma) +
  labs(title = "Number of Same-Sex Couples by State, 2010",
       caption = "Source: 2010 U.S. Census",
       fill = "") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        legend.key.width = unit(3, "cm"),
        legend.key.height = unit(0.2, "cm"),
        plot.title = element_text(hjust = 0.5))

# Render choropleth for hate crimes in 2010
hate_across_states_2010 <- sf_hate %>%
  filter(bias == "sexual_orientation", year == 2010) %>%
  ggplot() +
  geom_sf(aes(fill = incidents), color = "white") +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100", ndiscr = 0) +
  scale_fill_viridis_c(option = "viridis", labels = scales::comma) +
  labs(title = "Number of Hate Crimes Against LGB Individuals by State, 2010",
       caption = "Source: FBI UCR Hate Crime Statistics",
       fill = "") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        legend.key.width = unit(3, "cm"),
        legend.key.height = unit(0.2, "cm"),
        plot.title = element_text(hjust = 0.5))

# Combine choropleths
two_maps <- plot_grid(same_sex_couples_state_2010, hate_across_states_2010, ncol = 1, align = "v")

# Save combined choropleths
ggsave("lgb-population-vs-hate-crimes-across-states.png", plot = two_maps,
       path = "data/results/", width = 7, height = 10)

# Render animated choropleth
hate_across_states <- sf_hate %>%
  filter(bias == "sexual_orientation") %>%
  mutate(year = as.integer(year)) %>%
  ggplot() +
  geom_sf(aes(fill = incidents), color = "white") +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100", ndiscr = 0) +
  scale_fill_viridis_c(option = "viridis", labels = scales::comma) +
  labs(title = "Hate Crimes Against LGB Individuals Across States",
       subtitle = "Year: {frame_time}",
       caption = "Source: FBI UCR Hate Crime Statistics",
       fill = "") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        legend.key.width = unit(3, "cm"),
        legend.key.height = unit(0.2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 18)) +
  transition_time(year)

# Save animation
animate(hate_across_states, width = 700, height = 500)
anim_save("hate-crimes-against-lgb-across-states.gif",
          animation = last_animation(), path = "data/results/")