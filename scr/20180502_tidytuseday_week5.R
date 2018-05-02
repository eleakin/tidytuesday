

library(tidyverse)
library(viridis)
library(maps)

# read data
acs2015_county_data_raw <- read_csv("data/acs2015_county_data.csv")

#get map data
usa_map <- map_data("state")


# calculate population per stat
acs2015_county_data <- acs2015_county_data_raw %>% 
  select(State, TotalPop) %>% 
  group_by(State) %>% 
  summarise(Total_Population = sum(TotalPop)) %>% 
  ungroup() %>% 
  mutate(State = tolower(State))

# joining measue with map data
usa_map <- usa_map %>% 
  left_join(acs2015_county_data, by = c("region" = "State"))


# ploting
gg <- ggplot(data = usa_map) + 
  geom_polygon(aes(x = long, y = lat, fill = Total_Population, group = group), color = "white") + 
  coord_fixed(1.3) +
  scale_fill_viridis(option = "plasma", name = "Total population") +
  theme_bw() +
  theme(panel.border=element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Total popultion by State", 
       subtitle = "source: census.gov",
       caption = "@T_bobi")

gg

#ggsave("../graphs/20180502tidyTuseday_week_5.png", gg, units = "in", dpi = 300)

