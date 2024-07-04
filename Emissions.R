rm(list=ls())

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(readxl)
library(dplyr)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(hrbrthemes)

# Read in Dataframe
df <- read_csv("emissions_low_granularity.csv")
df_emissions <- read_csv("emissions_high_granularity.csv")

# rename columns
names(df)[names(df) == "parent_entity"] <- "Company"
names(df)[names(df) == "parent_type"] <- "Status"
names(df)[names(df) == "year"] <- "Year"
names(df)[names(df) == "total_emissions_MtCO2e"] <- "Emissions"
names(df_emissions)[names(df_emissions) == "total_emissions_MtCO2e"] <- "Emissions"

# rename names in Status column
df <- df %>% 
  mutate(Status = recode(Status,  "State-owned Entity" = "State-Owned", 
                         "Investor-owned Company" = "Investor-Owned"))

# convert emissions to numeric format
df_emissions$Emissions <- as.numeric(as.character(df_emissions$Emissions))

# coal, gas, ect.
df_global <- df_emissions %>%
  filter(year >= 1960) %>%
  group_by(commodity, year) %>%
  summarize(Emissions = sum(Emissions, na.rm = TRUE))

# group all coal names together
processed_df <- df_global %>%
  mutate(commodity = ifelse(grepl("Coal$", commodity), "Coal", commodity)) %>%
  group_by(year, commodity) %>%
  summarize(Emissions = sum(as.numeric(Emissions), na.rm = TRUE), .groups = 'drop')



# Sum emissions by company and add rank
company_emissions <- df %>%
  group_by(Company) %>%
  summarize(Total_Emissions = sum(Emissions, na.rm = TRUE)) %>%
  arrange(desc(Total_Emissions)) %>%
  mutate(Rank = row_number())

# Merge ranks back into original df
df <- df %>%
  left_join(company_emissions %>% select(Company, Rank), by = "Company")

compute_value_change <- function(emissions, years) {
  # Check if 2015 and 2022 are present
  has_2015 <- 2015 %in% years
  has_2022 <- 2022 %in% years
  
  # Find the earliest and latest available years
  first_year <- min(years)
  last_year <- max(years)
  
  if (has_2015 && emissions[years == 2015] < emissions[years == last_year]) {
    return("Increased")
  } else if (!has_2015 || !has_2022) {
    first_year_value <- emissions[years == first_year]
    last_year_value <- emissions[years == last_year]
    
    if (last_year_value > first_year_value) {
      return("Increased")
    } else if (last_year_value < first_year_value) {
      return("Decreased")
    } else {
      return("No Change")
    }
  } else {
    return("Decreased")
  }
}

df_change <- df %>%
  group_by(Company) %>%
  mutate(value_change = compute_value_change(Emissions, Year))

filtered_df <- df_change
filtered_df$Rank_Group <- cut(filtered_df$Rank, 
                              breaks = c(0, 25, 50, 75, 100, 125), 
                              labels = c("1-25", "26-50", "51-75", "76-100", "101-125"), 
                              include.lowest = TRUE)

# Summarizing total emissions by Rank_Group and Status
grouped_df <- filtered_df %>%
  group_by(Rank_Group, Status) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE))

# Counting distinct companies by Rank_Group and Status
company_counts <- filtered_df %>%
  group_by(Rank_Group, Status) %>%
  summarise(Company_Count = n_distinct(Company))

# Joining the two summaries
grouped_df <- left_join(grouped_df, company_counts, by = c("Rank_Group", "Status"))

# Merging dataframes
count <- merge(filtered_df, df_change, by = c("Company", "Status", "Rank", "Year", "Emissions"))

# Summarizing the count of companies with 'Increased' or 'Decreased' value_change.y
company_count <- count %>%
  filter(Status %in% c("Investor-Owned", "State-Owned", "Nation State") & value_change.y %in% c("Increased", "Decreased")) %>%
  group_by(Rank_Group, Status, value_change.y) %>%
  summarise(Count = n_distinct(Company))

investor_count <- company_count %>%
  filter(Status == "Investor-Owned")

state_count <- company_count %>%
  filter(Status == "State-Owned")

nation_count <- company_count %>%
  filter(Status == "Nation State")

df_grouped <- df %>%
  filter(Year >= 1960) %>%
  group_by(Status, Year) %>%
  summarize(Emissions = sum(Emissions, na.rm = TRUE))

df_global <- df_emissions %>%
  filter(year >= 1960) %>%
  group_by(commodity, year) %>%
  summarize(Emissions = sum(Emissions, na.rm = TRUE))



############

# UI
ui <- dashboardPage(
  dashboardHeader(title = HTML("Breakdown of MtCO<sub>2</sub>e")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Emission Overview", tabName = "global", icon = icon("area-chart")),
      menuItem("Company Comparison", tabName = "investorstate", icon = icon("area-chart")),
      menuItem("Individual Company Comparison ", tabName = "linechart", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # Global tab
      tabItem(tabName = "global",
              fluidRow(
                infoBox(
                  "Top 122 Companies CO2 portion equivalent to", "72%", HTML("of global fossil fuel and cement CO2 emissions since 1751 <br> <br>"), icon
                  = icon("exclamation-circle"),
                  fill = TRUE
                ),
                infoBox(
                  "Over", HTML("70%"), "of these global CO2 emissions traced by the database historcally can be attributed to just 78 corporate and state companies", icon =
                    icon("tree"), color = "blue",
                  fill = TRUE
                ),
                infoBox(
                  HTML("MtCO<sub>2</sub>e refers to"), "Metric tons",  HTML("of carbon dioxide equivalent <br> <br>"), icon =
                    icon("info"), color = "teal",
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
              ),
              
      ),
      
      # Investor vs State Chart tab
      tabItem(tabName = "investorstate",
              fluidRow(
                infoBox(
                  "Emission Decrease/Increase calculated",
                  "Paris Agreement", 
                  HTML("<p style='margin-bottom:0'>in 2015.
          If a company was started after 2015, or ended before 2015, the decrease or increase was calculated
          by minusing the first year and last year.</p>"), 
                  icon = icon("calculator"),
                  fill = TRUE, width = 3, color = "olive"
                ),
                infoBox(
                  "75 Investor-owned Companies account for", "31% or 440 GtCO₂e", HTML("traced by the databas.
                                                                      3 largest companies:
                                                                           <ul>
                                                                           <li>Chevron</li>
                                                                           <li>ExxonMobil</li>
                                                                           <li>BP</li>
                                                                           </ul>
                                                                        
                                                                  "), icon =
                    icon("cloud"),
                  fill = TRUE, width = 3
                  
                ),
                infoBox(
                  "35 State Companies account for", "33% or 465 GtCO₂e ", HTML("traced by the database. 3 Largest contributers:
                                                             <ul>
                                                                           <li>Saudi Aramco</li>
                                                                           <li>Gazprom</li>
                                                                           <li>National Iranian Oil Company</li>
                                                                           </ul>"
                  ), icon =
                    icon("angle-double-up"), color = "blue",
                  fill = TRUE, width = 3
                ),
                infoBox(
                  "11 Nation State Companies account for", "36% or 516 GtCO₂e", HTML("traced by the database. Largest contributers:
                                                             <ul>
                                                                           <li>China's coal production</li>
                                                                           <li>Former Soviet Union</li>
                                                                           <li>China Cement Production</li>

                                                                           
                                                                           </ul>
                                                                                     "
                  ),
                  
                  icon("exclamation-circle"), color = "teal",
                  fill = TRUE, width = 3
                ),
              ),
              fluidRow(
                column(6, box(width = 12, plotlyOutput("investorChart1", height = 610))),
                column(6,
                       box(width = 12, plotlyOutput("investorChart2", height = 175)),
                       box(width = 12, plotlyOutput("investorChart3", height = 175)),
                       box(width = 12, plotlyOutput("investorChart4", height = 175))
                )
              )
      ),
      # Individual Company Comparison tab
      tabItem(tabName = "linechart",
              fluidRow(
                infoBox(
                  "China (Coal) is the", "Largest", HTML("MtCO<sub>2</sub>e Emitter"), icon = icon("cloud"),
                  fill = TRUE
                ),
                infoBox(
                  "Companies Generally", "Increasing", "Emissions", icon = icon("tree"), color = "blue",
                  fill = TRUE
                ),
                infoBox(
                  "State and Nation Companies", "Key to Reducing", HTML("Global MtCO<sub>2</sub>e"), icon =
                    icon("exclamation-circle"), color = "teal",
                  fill = TRUE
                )
              ),
              sidebarLayout(
                sidebarPanel(
                  selectizeInput("company", "Select Company", choices = unique(df$Company), multiple =
                                   TRUE),
                  selectizeInput("rank", "Select Rank", choices = 1:100, multiple = TRUE),
                  helpText("Maximum 5 companies allowed")
                  
                ),
                mainPanel(
                  plotlyOutput("linechart", height = "610px", width = "100%"),
                  fluidRow(
                    tags$div(
                      style = "margin-top: 10px; margin-left: 10px;",
                      tags$p("Data Reference:"),
                      tags$ul(
                        tags$li(
                          "Paul, G., Heede, R., & Vlugt, I. (2024).",
                          tags$em("Climate Accountability Institute"),
                          ". Climate Accountability Institute .",
                          tags$a(
                            href = "https://climateaccountability.org/publications.html",
                            "https://carbonmajors.org/Downloads"
                          )
                        )
                      )
                    )
                  )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observe({
    updateSelectizeInput(session, "company", choices = unique(df$Company), selected =
                           input$company)
    updateSelectizeInput(session, "rank", choices = 1:100, selected = input$rank)
    
    selected_company <- input$company
    selected_rank <- input$rank
    
    total_count <- length(selected_company) + length(selected_rank)
    excess <- max(0, total_count - 5)
    
    if (excess > 0) {
      if (length(selected_company) > excess) {
        selected_company <- selected_company[1:(length(selected_company) - excess)]
      } else {
        selected_company <- NULL
        selected_rank <- selected_rank[1:(length(selected_rank) - (excess - length(selected_company)))]
      }
    }
    
    updateSelectizeInput(session, "company", choices = unique(df$Company), selected =
                           selected_company)
    updateSelectizeInput(session, "rank", choices = 1:100, selected = selected_rank)
  })
  
  
  
  filtered_df <- reactive({
    data <- df
    
    if (!is.null(input$Company) && !is.null(input$rank)) {
      selected_items <- c(input$Company, input$rank)
      data <- data[data$Company %in% selected_items | data$Rank %in% selected_items, ]
    } else if (!is.null(input$company)) {
      data <- data[data$Company %in% input$company, ]
    } else if (!is.null(input$rank)) {
      data <- data[data$Rank %in% input$rank, ]
    }
    
    data
  })
  
  filtered_df <- reactive({
    data <- df
    
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
        xlim(c(1850, 2022)) +
        ylim(c(0, 100)) +
        labs(x = "Year", y = "Emissions", title = "MtCO₂e by Year")
    } else {
      max_emissions <- max(filtered_df()$Emissions, na.rm = TRUE)
      annotation_y <- max_emissions * 1.1 # Adjust the multiplier as needed to position higher
      
      # Generate the plot with filtered data
      gp <- ggplot(filtered_df(), aes(x = Year, y = Emissions, group = interaction(Company, Rank), color = Company, 
                                      text = paste("Year: ", Year, "<br> Company: ", Company, "<br> MtCO₂e: ", round(Emissions, 2), "<br> Rank: ", Rank, "<br>Status: ", Status))) +
        geom_line() +
        geom_vline(xintercept = 2015, linetype = "dashed", color = "red") +
        annotate("text", x = 2015, y = annotation_y, label = "Paris Agreement", angle = 90, vjust = -0.1, color = "red") +
        labs(x = "Year", y = "MtCO₂e", title = "Emissions by Year") +
        theme_minimal()
      
      gp1 <- ggplotly(gp, tooltip = c("text"))
      
      gp1 <- layout(gp1, margin = list(t = 65))
      
      gp1
      
    }
  })
  
  
  # Render the lineChart plot for the Individual Company Comparison tab
  output$areaChart <- renderPlotly({
    
    processed_df$year <- as.numeric(processed_df$year)
    
    emissions_chart <- ggplot(processed_df, aes(x=year, y=Emissions, fill=commodity)) +
      geom_area(alpha=0.8 , size=.5, colour="white") +
      scale_fill_manual(values = c( "#A4998E", "#507B6A", "#6A513C", "#4B1816")) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        axis.text.y = element_text(hjust = 1),
        axis.text.x = element_text(hjust = .5),
        legend.title = element_blank()
      ) +
      labs(x = "", y = "MtCO<sub>2</sub>e",
           title = "Combined MtCO<sub>2</sub>e Emissions by Commodity") +
      scale_x_continuous(breaks = c(1960, 1980, 2000, 2022), expand = c(0, 0)) + # Adjusted here
      scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
      labs(col = NULL) + 
      geom_vline(xintercept = 2015, linetype = "dashed", color = "red") +
      annotate("text", x = 2015, y = 30000, label = "Paris Agreement", angle = 90, vjust = -0.1, color = "red")
    
    p1 <- ggplotly(emissions_chart)
    
    p1 <- layout(p1, margin = list(t = 65))
    
    p1
    
    
  })
  
  
  
  # Render the areaChart1 plot for the Global tab
  output$areaChart1 <- renderPlotly({
    df_grouped$Year <- as.numeric(df_grouped$Year)
    
    df_grouped$Status <- factor(df_grouped$Status, levels = c("State-Owned", "Investor-Owned", "Nation State"))
    
    gp1 <- ggplot(df_grouped, aes(x = Year, y = round(Emissions, 2), col = Status, group = Status,
                                  text = paste(HTML("MtCO<sub>2</sub>e:"), round(Emissions, 2), '\nYear: ', Year))) +
      geom_line() +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        axis.text.y = element_text(hjust = 1),
        axis.text.x = element_text(hjust = .5),
        legend.title = element_blank()
      ) +
      labs(x = "", y = HTML("MtCO<sub>2</sub>e"),
           title = HTML("State, Investor, & Nation MtCO<sub>2</sub>e Emissions <br><span style='font-size: 12px;'>
</span>")) +
      scale_x_continuous(breaks = c(1960, 1980, 2000, 2022)) +
      scale_color_manual(values = c("#E95C20FF", "#006747FF", "#4F2C1DFF")) +
      scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
      labs(col = NULL)
    
    gp1
    
    gp <- ggplotly(gp1, tooltip = c("text"))
    
    gp <- layout(gp, margin = list(t = 65))
    
    gp
    
  })
  
  # Render the investorChart1 plot for the Investor vs State Chart tab
  output$investorChart1 <- renderPlotly({
    gp2 <- ggplot(grouped_df, aes(x = Rank_Group, y = round(Total_Emissions/1000, 2), fill = Status,
                                  text = paste(HTML("GtCO<sub>2</sub>e:"), round(Total_Emissions/1000, 2),
                                               '\nNumber of Companies: ', Company_Count))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)
      ) +
      scale_y_continuous(limits = c(0, 500), expand = c(0, 0)) +
      scale_fill_manual(values = c("#b8b8b8", "#031163", "#CD7E2A")) +
      labs(x = "Category", y = HTML("GtCO<sub>2</sub>e"), title = HTML("GtCO<sub>2</sub>e by
Category"), fill = NULL)
    
    ggplotly(gp2, tooltip = c("text"))
  })
  
  # Render the investorChart2 plot for the Investor vs State Chart tab
  output$investorChart2 <- renderPlotly({
    gp3 <- ggplot(state_count, aes(x = Rank_Group, y = Count, fill = value_change.y, text = paste("Number: ",
                                                                                                  Count))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Category", y = "Company Number", fill = "") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.line = element_line(colour = "black", size = 0.5)
      ) +
      scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
      scale_fill_manual(values = c("#CD7E2A", "#191970")) +
      labs(title = HTML("Investor Decrease vs. Increase"), subtitle = "Decrease vs. Increase")
    
    ggplotly(gp3, tooltip = "text")
    
  })
  
  # Render the investorChart3 plot for the Investor vs State Chart tab
  output$investorChart3 <- renderPlotly({
    gp4 <- ggplot(investor_count, aes(x = Rank_Group, y = Count, fill = value_change.y, text = paste("Number: ",
                                                                                                     Count))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8)
        
      ) +
      scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
      scale_fill_manual(values =c("#CD7E2A", "#191970")) +
      labs(x = "Category", y = "Company Number", title = HTML("State Decrease vs. Increase"))
    
    ggplotly(gp4, tooltip = "text") %>%
      layout(legend = list(title = list(text = "")))
  })
  
  # Render the investorChart4 plot for the Investor vs State Chart tab
  output$investorChart4 <- renderPlotly({
    gp4 <- ggplot(nation_count, aes(x = Rank_Group, y = Count, fill = value_change.y, text = paste("Number: ",
                                                                                                   Count))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8)
        
      ) +
      scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
      scale_fill_manual(values =c("#CD7E2A", "#191970")) +
      labs(x = "Category", y = "Company Number", title = HTML("Nation Decrease vs. Increase"))
    
    ggplotly(gp4, tooltip = "text") %>%
      layout(legend = list(title = list(text = "")))
  })
  
  
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
