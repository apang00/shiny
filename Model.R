
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(DT)
library(maps)
library(stringr)

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

print(result_df_filtered)

country_count <- result_df_filtered %>%
  group_by(Country = country) %>%
  summarise(Count = n())

