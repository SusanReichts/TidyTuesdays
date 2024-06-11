#load data ---------------------------------------------------------------------

pride_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-11/pride_index.csv')

#load packages -----------------------------------------------------------------

library(tidyverse)
library(sf)

install.packages("tidygeocoder")
library(tidygeocoder)

install.packages("mapdata")
library(maps)
library(mapdata)

#data prep ---------------------------------------------------------------------

#adding geo-information based on campus_location -------------------------------

pride_geo <- pride_index %>% 
  geocode(campus_location, 
          method="osm",
          lat=latitude,
          long=longitude,
          full_results=TRUE)

#creating map ------------------------------------------------------------------

ggplot(pride_geo) +
  borders("usa", 
          fill= "#f8f9fa") +
  aes(x = longitude,
             y = latitude,
             z = rating,
             group = 1) +
  stat_summary_hex(color = "#495057", 
                   linewidth= .2,
                   alpha = .95,
                   fun = mean, 
                   bins = 20) +
  scale_fill_steps(limits=c(1,5),
                   breaks=seq(1,5, by=1),
                   low= "#ced4da",
                   high= "#f72585") +
  geom_point(size= .2,
             color= "#495057") +
  coord_sf(default_crs = st_crs(4326)) +
  theme_void() +
  labs(title="Campus Pride Index",
       subtitle="\n\"Since 2007, the Campus Pride Index has been the premier LGBTQ national \nbenchmarking tool for colleges and universities to create safer, more inclusive campus communities. \n\nThe free online tool allows prospective students, families/parents and those interested in higher education \nto search a database of LGBTQ-friendly campuses who have \ncome out to improve the academic experience and quality of campus life.\"\n\n",
       caption="TidyTuesday, 11/06/24 \n\n data: https://campusprideindex.org \n\n geocoding: Cambon J, Hernangómez D, Belanger C, Possenriede D (2021).\ntidygeocoder: An R package for geocoding. \nJournal of Open Source Software, \n6(65), 3544, https://doi.org/10.21105/joss.03544 (R package version 1.0.5)\n",
       fill="index rating (1-5)") +
  theme(plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill="#6d6875"),
        plot.title = element_text(color="#6d6875",
                                  size=70,
                                  face="bold",
                                  hjust=.5),
        plot.subtitle = element_text(color="#6d6875",
                                     size=12,
                                     hjust=.5),
        plot.caption = element_text(color="#6d6875",
                                    size=10),
        legend.text = element_text(color="#6d6875",
                                   size=8),
        legend.title = element_text(color="#6d6875",
                                   size=10),
        legend.position = "bottom",
        legend.title.position = "top")
  
