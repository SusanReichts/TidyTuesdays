# TidyTuesdays
contributions to the weekly TidyTuesday challenges

#load data

planting_2020 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/planting_2020.csv')
planting_2021 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/planting_2021.csv')

#load packages

library(tidyverse)
library(emoGG)
library(showtext)

font_add_google("Salsa", family = "Salsa")


planting_A <- planting_2020 %>% 
  select(vegetable, variety, number_seeds_planted, date)

planting_B <- planting_2021 %>% 
  select(vegetable, variety, number_seeds_planted, date)

planting <- bind_rows(planting_A,planting_B)

planting <- planting %>%  
  separate_wider_delim(date, "-", names = c("year", "month", "day")) %>% 
  mutate(vegetable = recode(vegetable,
                            "pumpkins" = "pumpkin",
                            "rudabaga" = "rutabaga",
                            "Swiss chard" = "swiss chard")) 

planting %>% 
  drop_na() %>% 
  group_by(year, vegetable) %>% 
  summarise(seeds = sum(number_seeds_planted)) %>% 
  filter(vegetable == "kale" | 
           vegetable == "cabbage" |
           vegetable == "brussels sprouts" |
           vegetable == "spinach" |
           vegetable == "swiss chard") %>% 
  ggplot(aes(x=year,
             y=seeds,
             fill=vegetable)) +
  geom_col(colour="black") +
  scale_fill_brewer(palette="Greens") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f9f7f3",
                                        color = "#ddb892"),
        text = element_text(size = 15), 
        axis.title = element_text(size=15),
        legend.title = element_text(size=20, 
                                    family = "Salsa"),
        plot.title = element_text(size = 30, 
                             family = "Salsa",
                             margin = margin(20,0,10,0)),
        plot.subtitle = element_text(size=15, 
                                     family = "Salsa"),
        plot.caption = element_text(size=10,
                                    family="sans"),
        panel.grid = element_line(linetype = "dotted",
                                  color = "#ddb892"),
        legend.position = c(.18, .8)) +
  labs(title = "Plant your greens",
       subtitle = "number of seeds planted per year \n(based on Lisa's Vegetable Garden Data)",
       fill = "vegetables",
       x="",
       y="",
       caption = "TidyTuesday, 28/05/24 \ndata: Lisa Lendway, {gardenR}") +
  geom_emoji(aes(x = .95, 
                 y = 414),
             emoji = "1f96c",
             size = .04)
