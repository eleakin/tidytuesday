

library(tidyverse)
library(readxl)
library(ggalt)
library(scales)

# read in data
df_glob_mortality_raw <- readxl::read_xlsx(paste0(here::here(),"/data/global_mortality.xlsx"))

# tip from @thomas_mock
names(df_glob_mortality_raw) <- str_trim(
  str_remove_all(names(df_glob_mortality_raw), "[[:punct:]]")
)


## recreating world plot as lollipop plot 2016
# creat data
df_world_2016 <- df_glob_mortality_raw %>% 
  filter(country == "World", year == 2016) %>% 
  gather(key = "disease", value = "value", `Cardiovascular diseases`:Terrorism) %>% 
  mutate(disease = fct_reorder(disease, value), value = value /100)

# create plot
gg_worls_2016 <- df_world_2016 %>% 
  ggplot(aes(x = value, y = disease)) +
  geom_lollipop(point.colour="red4", point.size=2, horizontal=TRUE) +
  geom_text(aes(x = value + 0.03, label = percent(value)))+
  scale_x_continuous(labels = percent) +
  theme_minimal(base_family="Arial Narrow") +
  labs(x = NULL, y = NULL, 
       title = "Share of deaths couse by, World 2016",
       subtitle = "source: IHME, GTD & Amnesty International",
       caption = "@T_Bobin") +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.grid.major.y = element_blank())

gg_worls_2016
# save plot
ggsave(filename = "../graphs/20180417_tidyTuseday_week_3_2016.png", width = 15, height = 14, dpi = 300, units = "cm")

# rank disease over time 
## create data
df_rank_diesease <- df_glob_mortality_raw %>% 
  filter(country == "World") %>% 
  gather(key = "disease", value = "value", `Cardiovascular diseases`:Terrorism) %>% 
  group_by(year) %>% 
  mutate(rank = dense_rank(value))

#create plot
df_rank_diesease %>% 
  ggplot(aes(y = rank, x = year, color = disease)) + 
  geom_line() +
  geom_point() +
  #scale_y_continuous(breaks = c(1:32)) +
  scale_y_continuous(breaks = c(1:32), labels = (df_rank_diesease %>%  ungroup %>% filter(year == min(year)) %>% arrange(rank))$disease, 
                     sec.axis = sec_axis(~., breaks = seq(2,32,2), labels = seq(31, 1, -2), name = "rank")) +
  theme_minimal() +
  labs(y = NULL, x = NULL,
       title = "Rank of death reason over the World by year",
       subtitle = "source: IHME, GTD & Amnesty International",
       caption = "@T_Bobin") +
  theme(legend.position = "none") 

# save plot
ggsave("../graphs/20180417_tidyTuseday_week_3_rank.png", units = "cm", dpi = 600)
