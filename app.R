require(shiny)
require(shinythemes)
require(tidyverse)
require(ggmap)
require(DT)

df <- read.csv("data/Fastest_runs.csv", stringsAsFactors = F, encoding = "UTF-8")

names <- unique(df$Name)
nationalities <- unique(df$Nationality)

ui <- fluidPage(theme = shinytheme("cerulean"),
   
   titlePanel("Fastest Sprinters Tracker"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("eventName", "Event", choices = c("100m", "200m"), multiple = T, selected = c("100m", "200m")),
        
        conditionalPanel(
          condition = "100m" %in% "input.eventName",
          sliderInput("time100", "100m Time Range", 
                      min = 9.58, max = 10.09, 
                      value = c(9.58, 9.99), ticks = F, step = 0.01)),
          
        conditionalPanel(
          condition = "200m" %in% "input.eventName",
          sliderInput("time200", "200m Time Range", 
                      min = 19.19, max = 20.39, 
                      value = c(19.19, 19.99), ticks = F, step = 0.01)),
        
        dateRangeInput("dateRange", "Date Range", start = "1964-01-01", end = "2019-08-01"),
        
        selectInput("nation", "Nationality", choices = c(nationalities, "all"), selected = "all", multiple = T),
        
        selectInput("names", "Athelete Name", choices = c(names, "all"), selected = "all", multiple = T),
        
        hr(),
        
        actionButton("runButton", "Apply Filter")
        
      ),
      
      mainPanel(
        plotOutput("map"),
        hr(),
        h4("Filtered Data"),
        DT::dataTableOutput("df.filter")
        
        
      )
   )
)

server <- function(input, output, session) {
   
  theme1 <- list(theme(panel.grid.minor = element_blank(),
                        plot.background = element_blank()))
  
  my_data <- eventReactive(input$runButton, {
    req(input$eventName, input$dateRange, input$nation, input$names, df)
    df1 <- df %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
    
    if (input$eventName == "100m") {
      df1 <- df1 %>% filter(Event == "100m", Time >= input$time100[1], Time <= input$time100[2])
    }
    
    else if (input$eventName == "200m") {
      df1 <- df1 %>% filter(Event == "200m", Time >= input$time200[1], Time <= input$time200[2])
    }
    
    else {
      df1 <- df1 %>% filter(ifelse(Event == "100m", (Time >= input$time100[1] & Time <= input$time100[2]), 
                                   (Time >= input$time200[1] & Time <= input$time200[2])))
    }
    
    if (input$nation != "all") {
      df1 <- df1 %>% filter(Nationality %in% input$nation)
    }
  
    if (input$names != "all") {
      df1 <- df1 %>% filter(Name %in% input$names)
    }
  
    df1
  })
  
  output$df.filter <- DT::renderDT({
    req(my_data())
    
    return(select(my_data(), -lon, -lat))
  })
  
  output$map <- renderPlot({
    
    worldmap <- borders("world", color = "#f2ffe6", fill = "#f2ffe6", xlim = c(-140, 160), ylim = c(-60, 75))
    
    ggplot() + worldmap + 
      geom_point(data = my_data(), aes(x = lon, y = lat), shape = 19, col = "#ff0000", size = 2) + 
      theme(panel.background = element_rect(fill = "white"), 
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
      )
  })
  

}

shinyApp(ui = ui, server = server)

