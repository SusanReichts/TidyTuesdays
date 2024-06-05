#load data ----------------------------------------------------------------------------------

cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv')

#load packages ------------------------------------------------------------------------------

library(tidyverse)
library(waffle)
library(ggforce)
library(scales)

library(showtext)
font_add_google("Lora", family = "Lora")

#data prep ----------------------------------------------------------------------------------

#group by color

cheeses_colour <- cheeses %>% 
  group_by(color) %>% 
  summarise(n = n()) %>% 
  drop_na() 

cheeses_colour <- cheeses_colour %>% 
  mutate(color=factor(color)) %>% 
  mutate(color=fct_relevel(color,
                           c("pale white",
                             "white",
                             "ivory",
                             "cream",
                             "pale yellow",
                             "straw",
                             "yellow",
                             "golden yellow",
                             "golden orange",
                             "orange",
                             "red",
                             "pink and white",
                             "brown",
                             "brownish yellow",
                             "green",
                             "blue",
                             "blue-grey"))) %>% 
  arrange(color)

cheeses_colour$share <- cheeses_colour$n/sum(cheeses_colour$n)
cheeses_colour$ymax <- cumsum(cheeses_colour$share)
cheeses_colour$ymin <- c(0,head(cheeses_colour$ymax, n=-1))


#plotting -----------------------------------------------------------------------------------

cheeses_colour %>% 
  mutate_at(vars(starts_with("y")), 
            rescale, 
            to=pi*c(-.5,0), 
            from=0:1) %>%
  ggplot + 
  geom_arc_bar(aes(x0 = 0, 
                   y0 = 0, 
                   r0 = .27, 
                   r = 1.05, 
                   start = ymin, 
                   end = ymax, 
                   fill=color),
               color="white",
               linewidth=.1,
               alpha=0,
               linetype="dotted") + 
  geom_arc_bar(aes(x0 = 0, 
                   y0 = 0, 
                   r0 = .3, 
                   r = 1, 
                   start = ymin, 
                   end = ymax, 
                   fill=color),
               color="NA",
               linewidth=.1) + 
  coord_fixed() +
  scale_fill_manual(values = c( "#f4f1de",
                                "white",
                               "ivory",
                               "#fefae0",
                               "#faf0ca",
                               "#ffd166",
                               "#f4d35e",
                               "#f9c74f",
                               "#ffb627",
                               "#ff9505",
                               "#e63946",
                               "#e5989b",
                               "#d4a373",
                               "#edc531",
                               "#d4d700",
                               "#99d6ea",
                               "#a9d6e5")) +
  theme_void() +
  labs(title="cheese",
       subtitle="\"Cheese is nutritious food made mostly from the milk of cows \nbut also other mammals, including sheep, goats, buffalo, reindeer, camels and yaks. \nAround 4000 years ago people have started to breed animals and process their milk. \nThat's when the cheese was born.\"\n\n\n\n\n\n\n\nRelative cheese colours according to cheese.com",
       caption="TidyTuesday, 04/06/24") +
  theme(legend.position = "none",
        plot.background = element_rect(fill="#240046"),
        plot.title = element_text(hjust=1,
                                  vjust=-6.5,
                                  color="white",
                                  size=80,
                                  face="bold"),
        plot.subtitle = element_text(color="white",
                                     size=10),
        plot.caption = element_text(color="white",
                                    size=10))

