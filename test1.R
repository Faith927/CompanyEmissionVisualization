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
                  "The Top 100 Companies account for", "72%", "of the world's emissions.", icon = icon("line-chart"), color = "light-blue",
                  fill = TRUE
                ),
                infoBox(
                  "State owned entities produce <br> more emissions",  icon = icon("line-chart"), color = "olive",
                  fill = TRUE
                ),
                infoBox(
                  "Global Approval Rating", "60%", icon = icon("line-chart"), color = "teal",
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
                  "Investor Orders", uiOutput("investorOrderNum"), "Subtitle", icon = icon("credit-card")
                ),
                infoBox(
                  "Investor Approval Rating", "70%", icon = icon("line-chart"), color = "blue",
                  fill = TRUE
                ),
                infoBox(
                  "Investor Progress", uiOutput("investorProgress"), icon = icon("users"), color = "orange"
                )
              ),
              fluidRow(
                column(6, box(width = 12, plotlyOutput("investorChart1", height = 450))),
                column(6,
                       box(width = 12, plotlyOutput("investorChart2", height = 200)),
                       box(width = 12, plotlyOutput("investorChart3", height = 200))
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
              fluidRow(
                column(width = 9,
                       plotlyOutput("lineChart")
                ),
                column(width = 3,
                       selectizeInput(
                         "search",
                         "Search Company (max = 5)",
                         choices = unique(df2$Company),
                         multiple = TRUE,
                         options = list(
                           placeholder = "Type a company name",
                           maxItems = 5
                         )
                       )
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
      
      g <- ggplot(filteredData, aes(x = Year, y = Emissions, group = Company, color = Company, text = paste("Status:", Status, "Rank", Rank))) +
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
      labs(x = "Rank Group", y = "Total GtCO2e Emissions", fill = "Status") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_manual(values = c("#FF7F0E", "#1F77B4"))  # Add custom colors
    
    ggplotly(gp2, tooltip = c("text"))
  })
  
  output$investorChart2 <- renderPlotly({
    gp3 <- ggplot(count2, aes(x = Rank_Group, y = Count, fill = value_change)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Rank Group", y = "Total GtCO2e Emissions", fill = "") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_manual(values = c("#1F77B4", "#FF7F0E")) + # Add custom colors
      labs(title = "Investor Emissions", subtitle = "Decrease vs. Increase")
    
    ggplotly(gp3)
  })
  
  output$investorChart3 <- renderPlotly({
    gp4 <- ggplot(count3, aes(x = Rank_Group, y = Count, fill = value_change)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_manual(values = c("#1F77B4", "#FF7F0E")) + # Add custom colors
      labs(title = "State Emissions", subtitle = "Increase vs. Decrease")
    
    ggplotly(gp4)
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
