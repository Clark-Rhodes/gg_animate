# packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(ggthemes)

# get data from other sheets
# population data
pop <- as.data.frame(read.csv("population_total_long.csv"))
popNA <- filter(pop, Country.Name %in% c("Canada","United States","Mexico"))


#co2 data
co <- read.csv("CO2_emission.csv")
co <- pivot_longer(co, cols=c(5:35), names_to = "Year", values_to = "CO2 MTpc")
co1 <- separate(co, Year, into = c("y1", "y2"), sep = "X")
co2 <- separate(co1, y2, into =c("YearX", "y3"))
df <- cbind(Country = co2$Country.Name, Country_Code = co2$country_code,
            Region = co2$Region, CO2 = co2$`CO2 MTpc`, Year = co2$YearX)
df <- df[!duplicated(df),]
df <- na.omit(df, TRUE)
df <- as.data.frame(df)
df$CO2 <- as.numeric(df$CO2)
df$Year <- as.integer(df$Year)
df <- cbind(Country = co2$Country.Name, Country_Code = co2$country_code,
            Region = co2$Region, CO2 = co2$`CO2 MTpc`, Year = co2$YearX)
df <- df[!duplicated(df),]
df <- na.omit(df, TRUE)
df <- as.data.frame(df)
df$CO2 <- as.numeric(df$CO2)
df$Year <- as.integer(df$Year)
df1 <- filter(df, Region == "North America" | Country == "Mexico")

# clean pop data - filter years 
pop1 <- filter(popNA, Year >= 1990)


# add unique keys for joining and join
df1$key <- paste(df1$Country, df1$Year, sep = "_")
pop1$key <- paste(pop1$Country.Name, pop1$Year, sep = "_")

data <- left_join(pop1, df1, by = "key")


# clean joined data - new df
pc <- as.data.frame(cbind(Year = data$Year.x, Country = data$Country.Name, CO2 = data$CO2, Population = data$Count))
pc$CO2 <- as.numeric(pc$CO2)
pc$Year <- as.integer(pc$Year)
pc$Population <- as.numeric(pc$Population)


# ggplot
pcplot <- pc %>% ggplot(aes(x = Population, y = CO2, color = Country))+
  geom_jitter()+
  geom_point()

pcplot

# customize ggplot
pc1 <- pcplot +   
  scale_y_continuous("CO2 Metric tons per Capita")+
  geom_point(size = 3.5)+
  theme_hc(style = "darkunica") +
  scale_colour_hc("darkunica")+
  labs(title = "CO2:Population in North America (1990-2017)")+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.justification='left',
        legend.text = element_text(size = 10),
        plot.title = element_text(size=18))
  
# animate
pc1.animate <- pc1 + transition_reveal(Year)+
  labs(subtitle = 'Year: {frame_along}')+
  shadow_wake(wake_length = .1)
pc1.animate

# customize animation
animate(pc1.animate, height = 500, width = 600,
        fps = 30, duration = 10, end_pause = 60, res = 100)

# save your animation - consider setting your working directory
anim_save("CO2_Population in North America.gif")