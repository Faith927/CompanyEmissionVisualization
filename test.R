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

df1 <- df %>%
  pivot_longer(cols = -c(Company, Status, Rank),
               names_to = "Year",
               values_to = "Emissions")

df_total <- df1[df1$Company %in% c("Top 100", "Global"), ]

df_total1 <- df1[df1$Company %in% c("State", "Investor"), ]

df2 <- df1 %>%
  filter(!Company %in% c("Global", "Remaining", "Top 100", "State", "Investor", "T", "S", "I"))

filtered_df <- df2 %>% filter(Year == 2015)

filtered_df$Rank_Group <- cut(filtered_df$Rank, breaks = c(0, 25, 50, 75, 100), labels = c("1-25", "26-50", "51-75", "76-100"), include.lowest = TRUE)

grouped_df <- filtered_df %>%
  group_by(Rank_Group, Status) %>%
  summarise(Total_Emissions = sum(Emissions))

company_counts <- filtered_df %>%
  filter(Year == 2015) %>%
  group_by(Rank_Group, Status) %>%
  summarise(Company_Count = n())

grouped_df <- left_join(grouped_df, company_counts, by = c("Rank_Group", "Status"))

df2$Year <- as.numeric(df2$Year)

count <- merge(filtered_df, df3, by = c("Company", "Status", "Rank", "Year", "Emissions"))

count1 <- count %>%
  filter(Status %in% c("Investor", "State") & value_change %in% c("Increased", "Decreased")) %>%
  group_by(Rank_Group, Status, value_change) %>%
  summarise(Count = n())

count2 <- count1 %>%
  filter(Status == "Investor")

count3 <- count1 %>%
  filter(Status == "State")

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Multiple Line and Area Chart"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global", tabName = "global", icon = icon("area-chart")),
      menuItem("Investor vs State", tabName = "investorstate", icon = icon("area-chart")),
      menuItem("Top 100", tabName = "linechart", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # Global tab
      tabItem(tabName = "global",
              fluidRow(
                infoBox(
                  "Top 100 Companies Represent", "72%", "of the world's emissions.", icon = icon("line-chart"), color = "light-blue",
                  fill = TRUE
                ),
                infoBox(
                  "Global Emissions Reached", "38 Gtco2e", "in 2015", icon = icon("line-chart"), color = "olive",
                  fill = TRUE
                ),
                infoBox(
                  "State Companies Emissions", "Increased", "Dramatically since 1988", icon = icon("line-chart"), color = "teal",
                  fill = TRUE
                )
              ),
              fluidRow(
                column(width = 6,
                       plotlyOutput("areaChart")
                ),
                column(width = 6,
                       plotlyOutput("areaChart1")
                )
              )
      ),
      # Investor vs State Chart tab
      tabItem(tabName = "investorstate",
              fluidRow(
                infoBox(
                  "Top 25 Companies represent", "51%", "Global Emissions", icon = icon("credit-card"),
                  fill = TRUE
                ),
                infoBox(
                  "Companies Generally", "Increasing", "Emissions",  icon = icon("line-chart"), color = "blue",
                  fill = TRUE
                ),
                infoBox(
                  "State Companies", "More Likely", "To Increase Emissions", icon = icon("users"), color = "teal",
                  fill = TRUE
                ),
                infoBox(
                  "Small Investor Companies", "Increasing", "Emissions", icon = icon("users"), color = "teal",
                  fill = TRUE
                )
              ),
              fluidRow(
                column(6, box(width = 12, plotlyOutput("investorChart1", height = 370))),
                column(6,
                       box(width = 12, plotlyOutput("investorChart2", height = 165)),
                       box(width = 12, plotlyOutput("investorChart3", height = 165))
                )
              )
      ),
      # Line Chart tab
      tabItem(tabName = "linechart",
              fluidRow(
                infoBox(
                  "Line Chart Orders", uiOutput("lineChartOrderNum"), "Subtitle", icon = icon("credit-card")
                ),
                infoBox(
                  "Line Chart Approval Rating", "80%", icon = icon("line-chart"), color = "red",
                  fill = TRUE
                ),
                infoBox(
                  "Line Chart Progress", uiOutput("lineChartProgress"), icon = icon("users"), color = "yellow"
                )
              ),
              sidebarLayout(
                sidebarPanel(
                  selectizeInput("company", "Select Company", choices = unique(df2$Company), multiple = TRUE),
                  selectizeInput("rank", "Select Rank", choices = unique(df2$Rank), multiple = TRUE)
                ),
                mainPanel(
                  plotlyOutput("linechart")
      )
    )
  )
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
  
  output$linechart <- renderPlotly({
    if (is.null(input$company) && is.null(input$rank)) {
      # Empty plot with only axis lines
      ggplot() +
        theme_minimal() +
        theme(axis.line = element_line(color = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        xlim(c(1988, 2015)) +
        ylim(c(0, 100)) +
        labs(x = "Year", y = "Emissions", title = "Emissions by Year")
    } else {
      # Generate the plot with filtered data
      ggplot(filtered_df(), aes(x = Year, y = Emissions, group = interaction(Company, Rank), color = Company)) +
        geom_line() +
        labs(x = "Year", y = "Emissions", title = "Emissions by Year") +
        theme_minimal()
    }
  })
  
  output$areaChart <- renderPlotly({
    df_total$Year <- as.numeric(df_total$Year)
    
    p <- ggplot(df_total, aes(x = Year, y = Emissions, col = Company, group = Company, text = paste('Emissions: ', round(Emissions, 2), '\nYear: ', Year))) +
      geom_line() +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.ticks.y = element_line(color = "black"),  # Add ticks on y-axis
        axis.text.y = element_text(hjust = 1),  # Align y-axis labels to the right
        axis.text.x = element_text(hjust = .5)  # Rotate x-axis labels for better readability
      ) +
      labs(x = "", y = "Global Gtco2e Emissions", 
           title = "Global vs Top 100 Companies' Emissions") +  # Shortened title
      scale_x_continuous(breaks = c(1988, 2002, 2015)) +
      scale_color_manual(values = c("#009966", "#33CC00")) +
      scale_y_continuous(limits = c(0, 40), expand = c(0, 0))  
    
    p + labs(col = "")  # Remove the legend title
    
    p1 <- ggplotly(p, tooltip = c("text"))
    
    p2 <- layout(p1, showlegend = TRUE, legend = list(title = ""))
    
    p2
    
  })
  
  output$areaChart1 <- renderPlotly({
    
    df_total1$Year <- as.numeric(df_total1$Year)
    
    
    gp1 <- ggplot(df_total1, aes(x = Year, y = round(Emissions, 2), col = Company, group = Company, text = paste('Emissions: ', round(Emissions, 2), '\nYear: ', Year))) +
      geom_line() +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.ticks.y = element_line(color = "black"),  # Add ticks on y-axis
        axis.text.y = element_text(hjust = 1),  # Align y-axis labels to the right
        axis.text.x = element_text(hjust = .5)  # Rotate x-axis labels for better readability
      ) +
      labs(x = "", y = "Gtco2e Emissions", 
           title = "Top 100 State vs. Investor Companies' Emissions") +  # Shortened title
      scale_x_continuous(breaks = c(1988, 2002, 2015)) +
      scale_color_manual(values = c("#003399", "#990033")) +
      scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) 
    
    gp <- ggplotly(gp1, tooltip = c("text"))
    
    gp2 <- layout(gp, showlegend = TRUE, legend = list(title = ""))
    
    gp2
    
    
    
  })
  
  output$investorChart1 <- renderPlotly({
    gp2 <- ggplot(grouped_df, aes(x = Rank_Group, y = round(Total_Emissions/1000, 2), fill = Status,
                                text = paste('Emissions: ', round(Total_Emissions/1000, 2), '\nNumber of Companies: ', Company_Count))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Group", y = "GtCO2e Emissions", fill = "Status") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)
      ) +
      scale_y_continuous(limits = c(0, 20), expand = c(0, 0))  + 
      scale_fill_manual(values = c("#003399", "#990033"))  # Add custom colors
    
    ggplotly(gp2, tooltip = c("text"))
  })
  
  output$investorChart2 <- renderPlotly({
    gp3 <- ggplot(count2, aes(x = Rank_Group, y = Count, fill = value_change)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Rank Group", y = "Company Number", fill = "") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)
      ) +
      scale_y_continuous(limits = c(0, 20), expand = c(0, 0))  + 
      scale_fill_manual(values = c("#339933", "#003399")) + # Add custom colors
      labs(title = "Investor Emissions", subtitle = "Decrease vs. Increase")
    
    ggplotly(gp3, tooltip = "Count")
  })
  
  output$investorChart3 <- renderPlotly({
    gp4 <- ggplot(count3, aes(x = Rank_Group, y = Count, fill = value_change)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Rank Group", y = "Company Number", fill = "") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)
      ) +
      scale_y_continuous(limits = c(0, 20), expand = c(0, 0))  + 
      scale_fill_manual(values = c("#339933", "#990033")) + # Add custom colors
      labs(title = "State Emissions", subtitle = "Increase vs. Decrease")
    
    ggplotly(gp4, tooltip = "Count") %>% 
      layout(legend = list(title = list(text = "")))
  })
  
  output$globalOrderNum <- renderUI({
    numericInput(
      inputId = "globalOrderNum",
      label = NULL,
      value = 5000
    )
  })
  
  output$globalProgress <- renderUI({
    progressBar(
      id = "globalProgress",
      value = 40,
      total = 100,
      display_pct = TRUE,
      color = "purple"
    )
  })
  
  output$investorOrderNum <- renderUI({
    numericInput(
      inputId = "investorOrderNum",
      label = NULL,
      value = 2000
    )
  })
  
  output$investorProgress <- renderUI({
    progressBar(
      id = "investorProgress",
      value = 70,
      total = 100,
      display_pct = TRUE,
      color = "orange"
    )
  })
  
  output$lineChartOrderNum <- renderUI({
    numericInput(
      inputId = "lineChartOrderNum",
      label = NULL,
      value = 3000
    )
  })
  
  output$lineChartProgress <- renderUI({
    progressBar(
      id = "lineChartProgress",
      value = 80,
      total = 100,
      display_pct = TRUE,
      color = "yellow"
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
