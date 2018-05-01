
library(tidyverse)
library(ggalt)
library(scales)

# read data
df_au_salery_raw <- read.csv("./data/week4_australian_salary.csv")


# transform 
# select only income for both male and female
# calculate the gap between both
df_au_sal <- df_au_salery_raw %>% 
  select(-c(X, individuals, gender_rank)) %>% 
  spread(key = gender, value = average_taxable_income) %>% 
  mutate(gap = abs(Male - Female)) %>% 
  filter(!is.na(gap))


# ploting the 15 occupations
# with the biggest gap

k_dollar <- function(x){
  dollar_format(suffix = "k")(x/1000)
}

gg <- df_au_sal %>% 
  top_n(15,gap) %>% 
  arrange(gap) %>%
  mutate(occupation = factor(occupation, levels = occupation)) %>% 
  ggplot(aes(x = Female, xend = Male, y = occupation, group = occupation)) +
  
  geom_dumbbell(colour="#e3e2e1", size=2, colour_x = "#d481db" , colour_xend="#a3c4dc", 
                dot_guide=FALSE, dot_guide_size=0.15) +
  # formating the x axis
  scale_x_continuous(labels = function(x){dollar_format(suffix = "k")(x/1000)}) +
  theme_bw() +
  theme(panel.border=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.ticks=element_blank())+
  labs(x = "", y = "",
       title = "Occupations with the highest gender income gap in Australia",
       caption = "@T_Bobin")

gg

# save plot
ggsave("../graphs/20180426_tidyTuseday_week_4.png", gg, units = "in", dpi = 300)
