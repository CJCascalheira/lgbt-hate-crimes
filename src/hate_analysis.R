# Dependencies
library(cowplot)
library(tidyverse)
library(viridis)
library(gganimate)
library(ggrepel)

# Import data
hate <- read_csv("data/hate_crimes.csv")

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
  scale_y_continuous(breaks = seq(1000, 1300, 50), limits = c(1000, 1300)) +
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
(lgb_v_straight <- data.frame(sexuality = c("Lesbian, Gay, Bisexual", "Straight"),
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
)

# LGB vs Other Incidents
(lgb_v_other <- data.frame(type = c("Lesbian, Gay, Bisexual", "Other"),
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
)

# Combine proportional plots
lgb_pop_hate <- plot_grid(lgb_v_straight, lgb_v_other)

# Save plot
ggsave("lgb-population-vs-hate-crimes.png", plot = lgb_pop_hate,
       path = "data/results/", width = 10, height = 5)

#
all_total_year %>%
  left_join(so_total_year, by = "year") %>%
  mutate(prop_so = total_so / total_incidents)