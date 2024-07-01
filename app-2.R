library(shiny)
library(ggplot2)
library(plotly)
library(readxl)
library(dplyr)
library(tidyverse)
library(viridis)
library(ggrepel)
library(shinyWidgets)


# Define your data frame df1 here

df <- read_excel("carbon.xlsx", sheet = "data")

df1 <- df %>% pivot_longer(names_to = "year", values_to = "value", cols = 2:29)

df1$year <- as.numeric(df1$year)

# Define the UI
ui <- fixedPage(
  titlePanel("Multiple Line Chart"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "search",
        "Search Company",
        choices = unique(df1$Company),
        multiple = TRUE,
        options = list(
          placeholder = "Type a company name",
          maxItems = 5
        )
      )
    ),
    mainPanel(
      plotlyOutput("lineChart")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Set initial value for search input
  randomCompany <- sample(unique(df1$Company), 1)
  observe({
    updateSelectizeInput(session, "search", selected = randomCompany)
  })
  
  output$lineChart <- renderPlotly({
    # Filter the data based on search input
    filteredData <- subset(df1, Company %in% input$search)
    
    g <- ggplot(filteredData, aes(x = year, y = value, group = Company)) +
      geom_line() +
      labs(title = "Multiple Line Chart", x = "Year", y = "Value") +
      theme_minimal()
    
    ggplotly(g)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
