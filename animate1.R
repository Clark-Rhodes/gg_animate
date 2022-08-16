# animate 3 - world population

# packages
library(ggplot2)
library(gganimate)
library(tidyverse)

# get and ready data
pop <- as.data.frame(read.csv("population_total_long.csv"))
popNA <- filter(pop, Country.Name %in% c("Canada","United States","Mexico"))

# plot our data
p1 <- popNA %>% ggplot(aes(x=Year, y=Count, color = Country.Name))+
  geom_point()+
  geom_line(size = 1.25)+
  scale_y_continuous("Population")+
  geom_point(size = 2.0)+
  theme_hc(style = "darkunica") +
  scale_colour_hc("darkunica")+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.justification='left',
        legend.text = element_text(size = 10),
        plot.title = element_text(size=18))+
  labs(title = "Population in North America (1960-2018)",
       subtitle = 'Year: {frame_along}')

# animate data
p1.animate <- p1 + transition_reveal(Year)
p1.animate

# customize animation
animate(p1.animate, height = 500, width = 750,
        fps = 30, duration = 8, end_pause = 90)

# save your animation - consider setting your working directory
anim_save("Population in North America.gif")

