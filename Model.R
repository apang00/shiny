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


### Shiny Modelling ###
ui <- fluidPage(
  titlePanel("Partial Auschwitz Death Toll By Nationality"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country_group", "Select Country/Group:",
                  choices = unique(country_count$Country),
                  multiple = TRUE)
    ),
    mainPanel(
      plotlyOutput("plot"),
      DTOutput("table")
    )
  )
)


server <- function(input, output) {
  
  # Filter data based on user input
  filtered_data <- reactive({
    req(input$country_group)
    subset(country_count, Country %in% input$country_group)
  })
  
  output$plot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Country, y = ~Count, type = 'bar') %>%
      layout(title = 'Number of People by Nationality/Category',
             yaxis = list(title = 'Death Count'))  
  })
  
  # Render interactive table
  output$table <- renderDT({
    filtered_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)