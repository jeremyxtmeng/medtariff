# Install packages if not yet installed
# install.packages(c("dplyr", "ggplot2", "rnaturalearth", "sf"))

library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)

# semaglutide ----------
# Your data
df <- data.frame(
  Country = c("Israel", "India", "China", "Hong Kong", "Japan", "South Korea", "Taiwan", "Australia", "New Zealand",
              "Austria", "Belgium", "Denmark", "Estonia", "France", "Germany", "Ireland", "Italy", "Latvia",
              "Netherlands", "Spain", "Sweden", "Switzerland", "United Kingdom", "Canada",
              "Argentina", "Brazil", "Uruguay"),
  Value = c(6975795, 15867141, 122621983, 1415000, 182200, 74675, 27400, 407565, 100000,
            4762035, 4202971, 94022148, 2440, 37883, 496647, 15415235436, 2019862, 81150,
            2435519, 8524045, 45573494, 57116815, 672440,  446570,
            650379122, 705534, 2130)
)


# Load world map and remove Antarctica
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")

# Match country names for joining
df$Country[df$Country == "South Korea"] <- "Republic of Korea"
df$Country[df$Country == "United States"] <- "United States of America"

# Merge trade data with world map
world_merged <- world %>% left_join(df, by = c("name" = "Country"))


# Use a darker, high-contrast palette (deep blue to black-red)
dark_colors <- c("#08306b", "#08519c", "#2171b5", "#6baed6", "#bdd7e7", "#fcae91", "#fb6a4a", "#cb181d", "#67000d")

# Plot
ggplot(data = world_merged) +
  geom_sf(aes(fill = Value), color = "grey60", size = 0.1) +
  scale_fill_gradientn(
    colors = dark_colors,
    na.value = "grey98",
    name = NULL,  
    trans = "log10"
  ) +
  theme_void() +
  theme(legend.position = "right")


# insulin ---------
# Updated dataset
df2 <- data.frame(
  Country = c("United Arab Emirates", "India", "China", "Japan", "Malaysia", "Austria",
              "Belgium", "Denmark", "France", "Germany", "Canada"),
  Value = c(1105200, 5433833, 9004392, 7400, 1487362, 18007,
            50624, 73750325, 2728543, 2187694, 12868)
)

# Merge trade data with the world map
world_merged2 <- world %>% left_join(df2, by = c("name" = "Country"))


# Plot the map
ggplot(data = world_merged2) +
  geom_sf(aes(fill = Value), color = "grey70", size = 0.3) +
  scale_fill_gradientn(
    colors = dark_colors,
    na.value = "grey98",
    name = NULL,  
    trans = "log10"
  ) +
  theme_void() +
  theme(legend.position = "right")+
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 85),
    expand = FALSE
  ) 
