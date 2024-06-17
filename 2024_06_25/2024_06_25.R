## load data --------------------------------------------------------------------

lgbtq_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-25/lgbtq_movies.csv')

## load packages ----------------------------------------------------------------

library(tidyverse)
library(paletteer)
library(ggrepel)

## prepare data -----------------------------------------------------------------

# divide release date variable into year - month - day
# only include data with at least 1 vote

lgbtq_movies <- 
  lgbtq_movies %>% 
  filter(vote_count!=0) %>% 
  separate_wider_delim(release_date, 
                       "-",
                       names = c("year", "month", "day")) %>% 
  mutate(year = parse_number(year))

# collect data for labels

lgbtq_movies %>% 
  filter(year>1999) %>% 
  summarise(n=n())

lgbtq_movies %>% 
  filter(year>1999) %>% 
  count(original_language) %>% 
  n_distinct()

lgbtq_movies %>% 
  group_by(year) %>% 
  summarise(Mean = mean(vote_average, na.rm=TRUE))



## build plot -------------------------------------------------------------------

lgbtq_movies %>%  
  ggplot(aes(
  x= year,
  y= vote_average,
  fill= vote_average)) +
  geom_smooth(method=lm,
              size= .2,
              color= "#9a8c98",
              fill="#d8e2dc") +
  geom_jitter(shape= 21,
              alpha= .4) +
  annotate("text",
           label="the oldest LGBT+ movie in the \ndatabase is from 1882 \nand is called The Kiss",
           x= 1885.2,
           y= 9,
           size= 4,
           color= "#9a8c98",
           fontface = "italic",
           hjust = "inward",
           vjust= "outward") +
  geom_curve(x= 1885,
             xend= 1882,
             y=9.1,
             yend=11,
             curvature = -.4,
             size= .1,
             color= "#9a8c98") +
  annotate("text",
           label="Since 2020, there have been 3478 movies recorded in the database. \nThis represents movies in 55 languages with at least one rating vote. \nThe average rating across all time is 5.62.",
           x= 1882,
           y= 3,
           size= 4,
           color= "#9a8c98",
           fontface = "italic",
           hjust = "inward",
           vjust= "outward") +
  scale_x_continuous(position="top") +
  scale_y_binned() +
  scale_fill_paletteer_c("viridis::plasma") +
  theme_minimal() +
  theme(plot.background = element_rect(fill= "#edede9"),
        panel.grid = element_line(color= "#9a8c98",
                                  size= .05),
        legend.position = "none",
        plot.title = element_text(face="bold"),
        text = element_text(size=15)) +
  labs(x= "",
       y= "",
       title= "LGBTQ movies, release time x rating",
       subtitle= "based on The Movie Database (TMDB) and LGBT+ keywords",
       caption= "TidyTuesday, 25/06/24 \n\n data: TidyRainbow \n\"a data project for the LGBTQ+ community who use the R language ecosystem") 



       