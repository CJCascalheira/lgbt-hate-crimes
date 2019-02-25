# Dependencies
library(tidyverse)
library(sf)
library(fiftystater)

# Import data
couples <- read_csv("data/2010-census-same-sex-couples-state.csv")

# Default plot option
theme_set(theme_minimal())

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

# CREATE CHOROPLETH -------------------------------------------------------

# Identical key columns
sf_fifty$id <- str_to_title(sf_fifty$id)
couples$state <- str_to_title(couples$state)

# Merge data frames
sf_couples <- left_join(sf_fifty, couples, by = c("id" = "state"))
sum(is.na(sf_couples$n_couples))

# Render choropleth
same_sex_couples_state_2010 <- ggplot(sf_couples) +
  geom_sf(aes(fill = n_couples), color = "white") +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100", ndiscr = 0) +
  scale_fill_viridis_c(option = "viridis", labels = scales::comma) +
  labs(title = "Number of Same-Sex Couples by State",
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

# Save choropleth
ggsave("same-sex-couples-state-2010.png", plot = same_sex_couples_state_2010,
       path = "data/results/", width = 6.5, height = 5)