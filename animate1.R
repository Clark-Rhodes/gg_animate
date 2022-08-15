#ready packages

library(ggplot2)
library(gganimate)
library(tidyverse)
library(dplyr)
library(ggthemes)


# ready data
df <- read.csv("population_total_long.xls")
df1 <- subset(df, df$Country.Name %in% c("Canada",
                                         "Mexico","United States")) 
df1 <- mutate(df1, Population = df1$Count/1000000) 

# Create first ggplot (p1)
p1 <- df1 %>% ggplot(aes(x = Year, y = Population, color = Country.Name))+
  geom_line(size = 1.3)+
  theme_hc(style = "darkunica") +
  scale_colour_hc("darkunica")+
  labs(title = "Population in North America (1960-2018)")+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.justification='left',
        legend.text = element_text(size = 8))+
  scale_y_continuous("Population (in Millions)")




