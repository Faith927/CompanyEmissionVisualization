library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(readxl)
library(dplyr)
library(tidyverse)
library(viridis)
library(ggrepel)
library(shinyWidgets)

# Define your data frame df1 here
df <- read_excel("emissions.xlsx", sheet = "Emissions")

df1 <- df[!(as.numeric(row.names(df)) %in% c(101, 102, 103, 104, 105, 106, 107, 108)), ]

df2 <- df1 %>%
  pivot_longer(cols = -c(Company, Status),
               names_to = "Year",
               values_to = "Emissions")

df2$Year <- as.numeric(df2$Year)



# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Multiple Line Chart"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Line Chart", tabName = "linechart", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 9,
             plotlyOutput("lineChart")
      ),
      column(width = 3,
             selectizeInput(
               "search",
               "Search Company",
               choices = unique(df2$Company),
               multiple = TRUE,
               options = list(
                 placeholder = "Type a company name",
                 maxItems = 5
               )
             ),
             selectizeInput(
               "search",
               "Search Rank",
               choices = unique(df2$Rank),
               multiple = TRUE,
               options = list(
                 placeholder = "Type a Rank number",
                 maxItems = 5
               )
             )
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  output$lineChart <- renderPlotly({
    # Check if any companies are selected
    if (!is.null(input$search) && length(input$search) > 0) {
      # Filter the data based on search input
      filteredData <- subset(df2, Company %in% input$search)
      
      g <- ggplot(filteredData, aes(x = Year, y = Emissions, group = Company, color = Company, text = paste("Status:", Status))) +
        geom_line(show.legend = FALSE) +
        labs(title = "Examination of top 100 companies gtco2e emissions", x = "Year", y = "Gtco2e Emissions") +
        theme_minimal() +
        scale_color_viridis_d()
      
      ggplotly(g, tooltip = c("x", "y", "color", "text"))
    } else {
      # Return an empty ggplot with axis labels and formatted axes
      ggplot() +
        labs(title = "Examination of top 100 companies gtco2e emissions", x = "Year", y = "Gtco2e Emissions") +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank()) +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
