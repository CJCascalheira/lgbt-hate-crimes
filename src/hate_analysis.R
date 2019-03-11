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
crimes <- read_csv("data/crime_type.csv")

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

# Total SO hate crimes from 2006-2017 with hetero
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

# LGBT total incidents 2006-2017 w/ hetero
lgbt_total_with_hetero <- hate %>%
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
  labs(title = "Number of Crimes Based on Sexual Orientation by State, 2010",
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
  labs(title = "Crimes Based on Sexual Orientation Across States",
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
        legend.key.width = unit(4, "cm"),
        legend.key.height = unit(0.2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 18)) +
  transition_time(year)

# Save animation
animate(hate_across_states, width = 700, height = 500)
anim_save("crimes-based-sexual-orientation-across-states.gif",
          animation = last_animation(), path = "data/results/")

# Greatest number of crimes by state
hate %>%
  group_by(state) %>%
  summarize(total = sum(incidents)) %>%
  arrange(desc(total)) %>%
  head()

# Least number of crimes by state
hate %>%
  group_by(state) %>%
  summarize(total = sum(incidents)) %>%
  arrange(total) %>%
  tail()

# ANALYZE TABLE 4 ---------------------------------------------------------

# Total sexual orientation crimes 1996-2017
crimes %>%
  filter(bias == "sexual_orientation", crime == "total") %>%
  select(-c(bias_group, crime)) %>%
  summarize(total = sum(n))

# Sexual orientation total incidents 2006-2017
(non_hetero_total_year <- crimes %>%
   filter(bias %in% c("sexual_orientation", "anti_hetero"),
          crime == "total") %>%
   select(-c(bias_group, crime)) %>%
   spread(key = bias, value = n) %>%
   filter(!(year %in% c(1996, 1997, 1998, 1999, 2000,
                        2001, 2002, 2003, 2004, 2005))) %>%
   group_by(year) %>%
   summarize(non_hetero = sexual_orientation - anti_hetero)
)

# Anti-Heterosexual total incidents 2006-2017
(anti_hetero_year <- crimes %>%
    filter(bias == "anti_hetero", crime == "total") %>%
    select(-c(bias_group, crime)) %>%
    spread(key = bias, value = n) %>%
    filter(!(year %in% c(1996, 1997, 1998, 1999, 2000,
                         2001, 2002, 2003, 2004, 2005)))
)

# Anti-heterosexual total
(anti_hetero_total <- sum(anti_hetero_year$anti_hetero))

# LGB total ONLY, no hetero, 2006-2017
(non_hetero_total <- sum(non_hetero_total_year$non_hetero))

# LGBT total ONLY, no hetero, 2006-2017
(lgbt_total <- crimes %>%
    filter(bias %in% c("sexual_orientation", "gender_identity"),
           crime == "total") %>%
    filter(!(year %in% c(1996, 1997, 1998, 1999, 2000,
                         2001, 2002, 2003, 2004, 2005))) %>%
    group_by(year) %>%
    summarize(total = sum(n)) %>%
    summarize(lgbt = sum(total) - anti_hetero_total) %>%
    pull()
)

# Longer time line
so_crimes_longer <- crimes %>%
  filter(bias == "sexual_orientation",
         crime == "total") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(color = "#440154FF", size = 1.25) +
  scale_y_continuous(breaks = seq(0, 1800, 200), limits = c(0, 1800),
                     expand = c(0, 0), labels = scales::comma) +
  scale_x_continuous(breaks = seq(1996, 2017, 2), limits = c(1996, 2017),
                     minor_breaks = seq(1, 11, 1)) +
  labs(y = "Incidents",
       x = "Year",
       title = "Hate Crimes Based on Sexual Orientation",
       subtitle = "United States, 1996—2017",
       caption = "Source: FBI UCR Hate Crime Statistics")

# Save plot
ggsave("sexual-orientation-hate-crimes-longer-time.png", plot = so_crimes_longer, 
       path = "data/results/", width = 6.5, height = 5)

# TYPE OF CRIME AGAINST PERSONS -------------------------------------------

# As factors and integers
crimes <- within(crimes, {
  crime <- factor(crime, labels = c("Aggravated Assault", "Arson", "Burglary", "Human Trafficking",
                                    "Intimidation", "Larceny", "Motor Theft", "Murder", "Other: Person",
                                    "Other: Property", "Rape", "Robbery", "Simple Assault",
                                    "Society", "Total Crimes", "Vandalism"))
  bias_group <- factor(bias_group, labels = c("Gender Identity", "Sexual Orientation"))
  bias <- factor(bias, labels = c("Anti-Bisexual", "Anti-Gay", "Anti-Non-Binary",
                                  "Anti-Heterosexual", "Anti-Homosexual", "Anti-Lesbian",
                                  "Anti-LGBT", "Anti-Transgender", "Gender Identity",
                                  "Sexual Orientation"))
  year <- as.integer(year)
})

# Persons vs. property
persons <- c("Murder", "Rape", "Aggravated Assault", "Simple Assault",
             "Intimidation", "Human Trafficking", "Other: Person")
property <- c("Robbery", "Burglary", "Larceny", "Motor Theft",
              "Arson", "Vandalism", "Other: Property", "Society")

# Breakdown of total crimes against person
crimes %>%
  filter(crime != "Total Crimes", bias_group == "Sexual Orientation") %>%
  filter(crime %in% persons) %>%
  group_by(crime) %>%
  summarize(totals = sum(n)) %>%
  arrange(totals)

# Relevel factors
crimes$crime <- fct_relevel(crimes$crime, "Simple Assault", "Intimidation", "Aggravated Assault",
                            "Other: Person", "Rape", "Murder", "Human Trafficking")

# Barchart of total crimes against person 
(so_total_crime_person <- crimes %>%
  filter(!(crime %in% c("Total Crimes", "Human Trafficking")),
         bias_group == "Sexual Orientation") %>%
  filter(crime %in% persons) %>%
  group_by(crime) %>%
  summarize(totals = sum(n)) %>%
  ggplot(aes(x = crime, y = totals, fill = crime)) +
  geom_col() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Type of Hate Crime Based on Sexual Orientation",
       subtitle = "Total Crimes Against Persons, United States, 1996—2017",
       caption = "Source: FBI UCR Hate Crime Statistics",
       y = "Reported Incidents",
       x = "Crimes Against Persons",
       fill = "") +
  theme(legend.position = "none")
)

# Save barchart
ggsave("sexual-orientation-crime-type-against-person-1996-2017.png", plot = so_total_crime_person, 
       path = "data/results/", width = 7, height = 5)

# Breakdown of total crimes against person over time
so_yearly_crime_person <- crimes %>%
  filter(!(crime %in% c("Total Crimes", "Human Trafficking")),
         bias_group == "Sexual Orientation") %>%
  filter(crime %in% persons) %>%
  ggplot(aes(x = crime, y = n, fill = crime)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Crimes Against Persons Based on Sexual Orientation",
       subtitle = "United States, Year: {frame_time}",
       caption = "Source: FBI UCR Hate Crime Statistics",
       y = "Reported Incidents",
       x = "Crimes Against Persons",
       fill = "") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16)) +
  transition_time(year)

# Save animation
animate(so_yearly_crime_person, width = 700, height = 500)
anim_save("sexual-orientation-crime-type-against-person-yearly.gif",
          animation = last_animation(), path = "data/results/")

# TYPE OF CRIME AGAINST PROPERTY ------------------------------------------

# Breakdown of total crimes against property
crimes %>%
  filter(crime != "Total Crimes", bias_group == "Sexual Orientation") %>%
  filter(crime %in% property) %>%
  group_by(crime) %>%
  summarize(totals = sum(n)) %>%
  arrange(totals)

# Relevel factors
crimes$crime <- fct_relevel(crimes$crime, "Vandalism", "Robbery", "Larceny", "Burglary", "Arson",
                            "Society", "Other: Property", "Motor Theft")

# Barchart total crimes against property
(so_total_crime_property <- crimes %>%
  filter(crime != "Total Crimes", bias_group == "Sexual Orientation") %>%
  filter(crime %in% property) %>%
  group_by(crime) %>%
  summarize(totals = sum(n)) %>%
  ggplot(aes(x = crime, y = totals, fill = crime)) +
  geom_col() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Type of Hate Crime Based on Sexual Orientation",
       subtitle = "Total Crimes Against Property, United States, 1996—2017",
       caption = "Source: FBI UCR Hate Crime Statistics",
       y = "Reported Incidents",
       x = "Crimes Against Property",
       fill = "") +
  theme(legend.position = "none")
)

# Save barchart
ggsave("sexual-orientation-crime-type-against-property-1996-2017.png", plot = so_total_crime_property,
       path = "data/results/", width = 7, height = 5)

# Breakdown of total crimes against property over time
so_yearly_crime_property <- crimes %>%
  filter(!(crime %in% c("Total Crimes", "Human Trafficking")),
         bias_group == "Sexual Orientation") %>%
  filter(crime %in% property) %>%
  ggplot(aes(x = crime, y = n, fill = crime)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Crimes Against Property Based on Sexual Orientation",
       subtitle = "United States, Year: {frame_time}",
       caption = "Source: FBI UCR Hate Crime Statistics",
       y = "Reported Incidents",
       x = "Crimes Against Property",
       fill = "") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16)) +
  transition_time(year)

# Save animation
animate(so_yearly_crime_property, width = 700, height = 500)
anim_save("sexual-orientation-crime-type-against-property-yearly.gif",
          animation = last_animation(), path = "data/results/")

# MOTIVATIONAL BIASES -----------------------------------------------------

# Breakdown of total bias
crimes %>%
  filter(!(crime %in% c("Total Crimes", "Human Trafficking")),
         bias != "Sexual Orientation", bias_group != "Gender Identity") %>%
  group_by(bias) %>%
  summarize(totals = sum(n)) %>%
  arrange(totals)

# Relevel factors
crimes$bias <- fct_relevel(crimes$bias, "Anti-Gay", "Anti-Homosexual", "Anti-Lesbian", "Anti-LGBT",
                           "Anti-Bisexual", "Anti-Heterosexual")

# Barchart of total bias
(so_bias_type <- crimes %>%
  filter(!(crime %in% c("Total Crimes", "Human Trafficking")),
         bias != "Sexual Orientation", bias_group != "Gender Identity") %>%
  group_by(bias) %>%
  summarize(totals = sum(n)) %>%
  ggplot(aes(x = bias, y = totals, fill = bias)) +
  geom_col() +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(0, 12000, 3000), limits = c(0, 12000), 
                     labels = scales::comma) +
  labs(title = "Sexual Orientation Hate Crimes by Motivational Bias",
       subtitle = "Total Reported Incidents, United States, 2004—2017",
       caption = "Source: FBI UCR Hate Crime Statistics",
       y = "Reported Incidents",
       x = "Type of Bias",
       fill = "") +
  theme(legend.position = "none")
)

# Save barchart
ggsave("sexual-orientation-breakdown-bias.png", plot = so_bias_type,
       path = "data/results/", width = 6.5, height = 5)

# Bias by crime against persons
(so_bias_persons <- crimes %>%
    filter(!(crime %in% c("Total Crimes", "Human Trafficking")),
           bias != "Sexual Orientation", bias_group != "Gender Identity") %>%
    filter(crime %in% persons) %>%
    group_by(bias, crime) %>%
    summarize(totals = sum(n)) %>%
    filter(!(bias %in% c("Anti-LGBT", "Anti-Bisexual", "Anti-Heterosexual"))) %>%
    ggplot(aes(x = crime, y = totals, fill = crime)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ bias) +
    scale_fill_viridis_d() +
    scale_y_continuous(labels = scales::comma, limits = c(min(0), max(4000))) +
    labs(title = "Crimes Against Persons by Motivational Bias",
         subtitle = "Total Reported Incidents, United States, 2004—2017",
         caption = "Source: FBI UCR Hate Crime Statistics",
         y = "Reported Incidents",
         x = "Crimes Against Persons",
         fill = "") +
    theme(legend.position = "none",
          strip.text = element_text(size = 12))
)

# Save faceted barchart
ggsave("sexual-orientation-bias-crimes-persons.png", plot = so_bias_persons,
       path = "data/results/", width = 6.5, height = 5)

# Bias by crime against property
(so_bias_property <- crimes %>%
  filter(!(crime %in% c("Total Crimes", "Human Trafficking")),
         bias != "Sexual Orientation", bias_group != "Gender Identity") %>%
  filter(crime %in% property) %>%
  group_by(bias, crime) %>%
  summarize(totals = sum(n)) %>%
  filter(!(bias %in% c("Anti-LGBT", "Anti-Bisexual", "Anti-Heterosexual"))) %>%
  ggplot(aes(x = crime, y = totals, fill = crime)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ bias) +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma, limits = c(min(0), max(2000))) +
  labs(title = "Crimes Against Property by Motivational Bias",
       subtitle = "Total Reported Incidents, United States, 2004—2017",
       caption = "Source: FBI UCR Hate Crime Statistics",
       y = "Reported Incidents",
       x = "Crimes Against Property",
       fill = "") +
  theme(legend.position = "none",
        strip.text = element_text(size = 12))
)

# Save faceted barchart
ggsave("sexual-orientation-bias-crimes-property.png", plot = so_bias_property,
       path = "data/results/", width = 6.5, height = 5)

# GENDER IDENTITY: BASIC EDA ----------------------------------------------

# Total gender identity crimes over time
(gi_over_time <- crimes %>%
  filter(bias_group != "Sexual Orientation", crime == "Total Crimes",
         bias == "Gender Identity") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(color = "#3B528BFF", size = 1.25) +
  scale_y_continuous(breaks = seq(0, 150, 25), limits = c(0, 150)) +
  labs(y = "Reported Incidents",
       x = "Year",
       title = "Hate Crimes Based on Gender Identity",
       subtitle = "Total Reported Incidents, United States, 2013—2017")
)

# Relevel factors
crimes$bias <- fct_relevel(crimes$bias, "Anti-Transgender", "Anti-Non-Binary")

# Breakdown total bias for gender identity
(gi_bias <- crimes %>%
  filter(bias_group != "Sexual Orientation", crime == "Total Crimes",
         bias != "Gender Identity") %>%
  group_by(bias) %>%
  summarize(totals = sum(n)) %>%
  ggplot(aes(x = bias, y = totals, fill = bias)) +
  geom_col() +
  scale_fill_manual(values = c("#3B528BFF", "#5DC863FF")) +
  labs(y = "Reported Incidents",
       x = "",
       title = "Gender Identity Crimes by Motivational Bias",
       subtitle = "Total Reported Incidents, United States, 2013—2017",
       caption = "Source: FBI UCR Hate Crime Statistics") +
  theme(legend.position = "none")
)

# Combine plots
(gi_basic_eda <- plot_grid(gi_over_time, gi_bias, ncol = 1, align = "v"))

# Save plot
ggsave("gender-identity-hate-crimes-2013-2017.png", plot = gi_basic_eda,
       path = "data/results/", width = 7, height = 10)

# GENDER IDENTITY: CRIME TYPE ---------------------------------------------

# Relevel factors
crimes$crime <- fct_relevel(crimes$crime, "Simple Assault", "Aggravated Assault", "Intimidation",
                            "Other: Person", "Rape", "Murder")

# Crimes against persons based on gender identity
(gi_persons <- crimes %>%
  filter(bias == "Gender Identity",
         !(crime %in% c("Total Crimes", "Human Trafficking"))) %>%
  filter(crime %in% persons) %>%
  group_by(bias, crime) %>%
  summarize(totals = sum(n)) %>%
  ggplot(aes(x = crime, y = totals, fill = crime)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Gender Identity: Crimes Against Persons",
       subtitle = "Total Reported Incidents, United States, 2013—2017",
       x = "Crimes Against Persons",
       y = "Reported Incidents") +
  theme(legend.position = "none")
)

# Relevel factors
crimes$crime <- fct_relevel(crimes$crime, "Larceny", "Vandalism", "Robbery", "Burglary",
                            "Society", "Other: Property", "Motor Theft", "Arson")

# Crimes against property based on gender identity
(gi_property <- crimes %>%
    filter(bias == "Gender Identity",
           !(crime %in% c("Total Crimes"))) %>%
    filter(crime %in% property) %>%
    group_by(bias, crime) %>%
    summarize(totals = sum(n)) %>%
    ggplot(aes(x = crime, y = totals, fill = crime)) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis_d() +
    labs(title = "Gender Identity: Crimes Against Property",
         subtitle = "Total Reported Incidents, United States, 2013—2017",
         caption = "Source: FBI UCR Hate Crime Statistics",
         x = "Crimes Against Property",
         y = "Reported Incidents") +
    theme(legend.position = "none")
)

# Combine plots
(gi_crime_types <- plot_grid(gi_persons, gi_property, ncol = 1, align = "v"))

# Save plot
ggsave("gender-identity-crimes-against-persons-property-2013-2017.png", plot = gi_crime_types,
       path = "data/results/", width = 7, height = 10)
