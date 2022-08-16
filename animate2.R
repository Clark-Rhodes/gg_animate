# packages
library(ggplot2)
library(gganimate)
library(tidyverse)
library(gifski)
library(ggthemes)

# Data via Koustav Ghosh on Kaggle 
# link: "https://www.kaggle.com/datasets/koustavghosh149/co2-emission-around-the-world/discussion?select=CO2_emission.csv"
# Ready and clean data - 'year' column is dirty
co <- read.csv("CO2_emission.csv")
co <- pivot_longer(co, cols=c(5:35), names_to = "Year", values_to = "CO2 MTpc")
co1 <- separate(co, Year, into = c("y1", "y2"), sep = "X")
co2 <- separate(co1, y2, into =c("YearX", "y3"))


# new df - remove dups and N/A
df <- cbind(Country = co2$Country.Name, Country_Code = co2$country_code,
            Region = co2$Region, CO2 = co2$`CO2 MTpc`, Year = co2$YearX)
df <- df[!duplicated(df),]
df <- na.omit(df, TRUE)
df <- as.data.frame(df)
df$CO2 <- as.numeric(df$CO2)
df$Year <- as.integer(df$Year)


# North America Only (Mexico labeled as Latin America)
df1 <- filter(df, Region == "North America" | Country == "Mexico")

p <- ggplot(data = df1, aes(x = Year, y = CO2, color = Country))+
  geom_line(size = 1.25)+
  scale_y_continuous("Metric tons per Capita")+
  geom_point(size = 2.0)+
  theme_hc(style = "darkunica") +
  scale_colour_hc("darkunica")+
  labs(title = "CO2 Emissions in North America (1990-2019)",
        subtitle = 'Year: {frame_along}')+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.justification='left',
        legend.text = element_text(size = 10),
        plot.title = element_text(size=18))
p

# animate with transition_reveal()
p.animate <- p + transition_reveal(Year)
p.animate

# enhance the animation - adjust size, frames per second, duration of animation, end pause
animate(p.animate, height = 500, width = 750,
        fps = 30, duration = 8, end_pause = 90)

# save your animation - consider setting your working directory
anim_save("CO2 Emissions in North America.gif")





# use frame_along, not frame_time, if you only have a year, but not yyyy/mm/dd, 
# use transition_reveal() instead of transition_time()



