### setup ###
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(maps)
library(stringr)

### Data Cleaning ###
# The ultimate goal is to get the data to be in 2 columns, one called country
# and the other called count, this will make it so that plotting the 
# death count in shiny will be much easier
setwd('C:/Users/alexp/Desktop/shiny')

city <- aus %>% select("Birthplace")
city$Birthplace <- tolower(city$Birthplace)
city$Birthplace <- trimws(city$Birthplace)

city_filtered <- city %>%
  filter(!str_detect(city$Birthplace, "[^A-Za-z0-9\\-]"))

world.cities$name <- tolower(world.cities$name)
world.cities$name <- trimws(world.cities$name)

city_country_df <- subset(world.cities, name %in% city$Birthplace)


result_df <- merge(city, city_country_df, by.x = "Birthplace", by.y = "name", all.x = TRUE)


result_df <- rename(result_df, country = country.etc)


result_df_filtered <- result_df %>%
  filter(!is.na(country))


country_count <- result_df_filtered %>%
  group_by(Country = country) %>%
  summarise(Count = n())


