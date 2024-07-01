# Global
library(shiny)
library(tidyr)
library(plotly)

# load data
df <- read_excel("carbon.xlsx", sheet = "data")
df1 <- df %>% pivot_longer(names_to = "year", values_to = "value", cols = 2:29)
df1$year <- as.numeric(df1$year)

# Define the UI
ui <- fixedPage(
  
  titlePanel("Exploring Gtco2e Emissions by Companies in the top 100 emitters"),
  
  sidebarPanel(
    
    radioButtons("colourby", "Colour by...",
                 choices = c("Company"),
                 inline = TRUE),
    
    conditionalPanel(
      condition = "input.colourby == 'Company'",
      selectizeInput(
        "selectcountry",
        label = "Select country(s) to track (max = 5):",
        choices = c(levels(df1$Company)),
        multiple = TRUE,
        selected = "All",
        options = list(maxItems = 5)
      ),
      radioButtons("trace", "Track country through time:",
                   choices = c("No", "Yes"),
                   inline = TRUE)
    ),
    
    radioButtons("logx", "Transform x:",
                 choices = c("Linear", "Log x"),
                 inline = TRUE)
  ),
  
  mainPanel(
    plotlyOutput("scatter"),
    p("By James Baglin")
  )
)

# Define the server

server <- function(input, output) {
  
  gm_filt <- reactive({
    gm <- df1 %>% filter(Company != "Kuwait")
    gm_filt <- gm %>% filter(Country %in% input$selectcountry)
    gm_filt$country <- droplevels(gm_filt$country)
    gm_filt
  })
  
  output$scatter <- renderPlotly({
    
    if(input$colourby == 'Continent') {
      
      gm <- df1 %>% filter(country != "Kuwait")
      
      xrange <- c(-2500,52500)
      xlogrange <- c(5.5,11)
      
      plot_ly(data = gm, type = "scatter", mode = "markers",
              color = ~continent, colors = "Dark2",
              y = ~lifeExp,
              size = ~pop,
              frame = ~year,
              text = paste("<b>Company</b>: ", gm$country,
                           "<br><b>Life expectancy</b>: ",
                           paste0(gm$lifeExp,"yrs."),
                           "<br><b>GDP per cap</b>: ",
                           paste0("$",round(gm$gdpPercap/1000,2),"k"),
                           "<br><b>Population</b>: ",
                           paste0(round(gm$pop/1000000,2),"M")),
              x = if(input$logx == "Log x") {~log(gdpPercap)} else {~gdpPercap}) %>% 
        
        layout(yaxis = list(zeroline = FALSE, 
                            title = "Life expectancy (Average at birth)",
                            range = c(25,85)),
               xaxis = list(zeroline = FALSE,
                            range = if(input$logx == "Linear") {xrange} else {xlogrange},
                            title = ifelse(input$logx == "Linear", "GDP per capita",
                                           "log(GDP per capita)"))
        ) %>% 
        
        config(displayModeBar = F) %>% 
        
        animation_slider(currentvalue = list(prefix = "",
                                             font = list(color="black")))
      
    } else {
      
      gm <- df1 %>% filter(Company != "Kuwait")
      
      xrange <- c(-2500,52500)
      xlogrange <- c(5.5,11)
      
      plot_ly(data = gm, type = "scatter", mode = "markers") %>%
        
        add_trace(x = if(input$logx == "Log x") {~log(gdpPercap)} else {~gdpPercap},
                  y = ~lifeExp,
                  size = ~pop,
                  name = "All",
                  marker = list(color = "#808080"),
                  opacity  = .1,
                  frame = ~year,
                  text = paste("<b>Country</b>: ", gm$Company,
                               "<br><b>Life expectancy</b>: ",
                               paste0(gm$lifeExp,"yrs."),
                               "<br><b>GDP per cap</b>: ",
                               paste0("$",round(gm$gdpPercap/1000,2),"k"),
                               "<br><b>Population</b>: ",
                               paste0(round(gm$pop/1000000,2),"M"))
        ) %>% 
        
        add_trace(inherit = FALSE,
                  data = gm_filt(),
                  x = if(input$logx == "Log x") {~log(gdpPercap)} else {~gdpPercap},
                  y = ~lifeExp, 
                  color = ~country, colors = "Dark2",
                  size = ~pop,
                  frame = ~year,
                  text = paste("<b>Country</b>: ", gm_filt()$country,
                               "<br><b>Life expectancy</b>: ",
                               paste0(gm_filt()$lifeExp,"yrs."),
                               "<br><b>GDP per cap</b>: ",
                               paste0("$",round(gm_filt()$gdpPercap/1000,2),"k"),
                               "<br><b>Population</b>: ",
                               paste0(round(gm_filt()$pop/1000000,2),"M"))) %>% 
        
        add_trace(inherit = FALSE,
                  data = gm_filt(),
                  x = if(input$logx == "Log x") {~log(gdpPercap)} else {~gdpPercap},
                  y = ~lifeExp, mode = "lines+markers", 
                  color = ~country, colors = "Dark2",
                  visible = ifelse(input$trace == "Yes", TRUE, FALSE),
                  size = ~pop,
                  showlegend = FALSE,
                  opacity  = .3,
                  text = paste("<b>Country</b>: ", gm_filt()$country,
                               "<br><b>Life expectancy</b>: ",
                               paste0(gm_filt()$lifeExp,"yrs."),
                               "<br><b>GDP per cap</b>: ",
                               paste0("$",round(gm_filt()$gdpPercap/1000,2),"k"),
                               "<br><b>Population</b>: ",
                               paste0(round(gm_filt()$pop/1000000,2),"M"))) %>%
        
        
        layout(yaxis = list(zeroline = FALSE, 
                            title = "Life expectancy (Average at birth)",
                            range = c(25,85)),
               xaxis = list(zeroline = FALSE,
                            range = if(input$logx == "Linear") {xrange} else {xlogrange},
                            title = ifelse(input$logx == "Linear", "GDP per capita",
                                           "log(GDP per capita)"))
        ) %>% 
        
        config(displayModeBar = F) %>% 
        
        animation_slider(currentvalue = list(prefix = "",
                                             font = list(color="black")))
      
      
    }
  })
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)
