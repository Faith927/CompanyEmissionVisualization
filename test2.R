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
library(shinyjs)


# Define your data frame df1 here
df <- read_excel("emissions.xlsx", sheet = "Emissions")

df1 <- df %>%
  pivot_longer(cols = -c(Company, Status, Rank),
               names_to = "Year",
               values_to = "Emissions")

df_total <- df1[df1$Company %in% c("Top 100", "Global"), ]

df_total1 <- df1[df1$Company %in% c("State", "Investor"), ]

df2 <- df1 %>%
  filter(!Company %in% c("Global", "Remaining", "Top 100", "State", "Investor", "T", "S", "I"))


# Define UI
ui <- fluidPage(
  useShinyjs(), 
  titlePanel("Emissions by Company and Rank"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("company", "Select Company", choices = unique(df2$Company), multiple = TRUE),
      selectizeInput("rank", "Select Rank", choices = unique(df2$Rank), multiple = TRUE)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)


    server <- function(input, output, session) {
      observe({
        updateSelectizeInput(session, "company", choices = unique(df2$Company), selected = input$company)
        updateSelectizeInput(session, "rank", choices = unique(df2$Rank), selected = input$rank)
        
        Company <- length(input$company)
        Ranks <- length(input$rank)
        Total <- Company + Ranks
        
        if (Total > 5) {
          selected_company <- input$company
          selected_rank <- input$rank
          
          # Adjust the number of selected options based on Total
          if (Total > 5) {
            excess <- Total - 5
            if (Company >= excess) {
              selected_company <- selected_company[1:(Company - excess)]
            } else {
              selected_company <- selected_company[1:Company]
              selected_rank <- selected_rank[1:(Ranks - (excess - Company))]
            }
          }
          
          # Update selectize inputs with restricted selections
          updateSelectizeInput(session, "company", choices = unique(df2$Company), selected = selected_company)
          updateSelectizeInput(session, "rank", choices = unique(df2$Rank), selected = selected_rank)
        }
      })
      

  filtered_df <- reactive({
    data <- df2
    
    if (!is.null(input$company) && !is.null(input$rank)) {
      selected_items <- c(input$company, input$rank)
      data <- data[data$Company %in% selected_items | data$Rank %in% selected_items, ]
    } else if (!is.null(input$company)) {
      data <- data[data$Company %in% input$company, ]
    } else if (!is.null(input$rank)) {
      data <- data[data$Rank %in% input$rank, ]
    }
    
    data
  })
  


  output$plot <- renderPlot({
    if (is.null(input$company) && is.null(input$rank)) {
      # Empty plot with only axis lines
      plot(NULL, xlim = c(1988, 2015), ylim = c(0, 100), xlab = "Year", ylab = "Emissions")
    } else {
      # Generate the plot with filtered data
      ggplot(filtered_df(), aes(x = Year, y = Emissions, group = interaction(Company, Rank), color = Company)) +
        geom_line() +
        labs(x = "Year", y = "Emissions", title = "Emissions by Year") +
        theme_minimal()
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)



