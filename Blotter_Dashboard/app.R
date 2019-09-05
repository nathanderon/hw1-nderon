#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load packages, adapted from https://gist.github.com/stevenworthington/3178163
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2", "dplyr", "shiny", "DT", "tools")
ipak(packages)

#Load data
blotter_data <- read.csv("Blotter_Data_Trim.csv", header = T)
blotter_data <- blotter_data[2:16]
blotter_data$INCIDENTTIME <- strptime(x = as.character(blotter_data$INCIDENTTIME),
                                       format = "%Y-%m-%dT%H:%M:%S") #From factor to datetime
blotter_data$time <- format(blotter_data$INCIDENTTIME, "%H:%M:%S") #From datetime to character
blotter_data$time <- as.POSIXct(x = blotter_data$time, format = "%H:%M:%S") #From character to continuous time var
blotter_data$date <- format(blotter_data$INCIDENTTIME, "%m-%d")
blotter_data$date <- as.POSIXct(x = blotter_data$date, format = "%m-%d")
blotter_data$type <- cut(blotter_data$HIERARCHY, c(-Inf, 9, 98, Inf), labels = c("Type 1 - Major Crime", "Type 2 - Minor Crime", "No Data"))
blotter_data$INCIDENTTIME <- as.POSIXct(blotter_data$INCIDENTTIME) #to avoid POSIXlt error

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Pittsburgh Police Blotter Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
         checkboxGroupInput(inputId = "selected_districts",
                            label = "Select Council District(s)",
                            choices = c(1:9),
                            selected = c(1:9)
        ),
        
        dateRangeInput(inputId = "date_range",
                       label = "Date Range: yyyy-mm-dd",
                       start = "2009-01-01",
                       end = "2019-01-01",
                       startview = "year")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput(outputId = "time"),
         plotOutput(outputId = "date")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Council district filter
  blotter_subset <- reactive({
    req(input$selected_districts)
    filter(blotter_data, COUNCIL_DISTRICT %in% input$selected_districts & 
             between(INCIDENTTIME, as.POSIXct(input$date_range[[1]]), as.POSIXct(input$date_range[[2]])))
  })

  #Decrease data size
  blotter_sample <- reactive({
    sample_n(blotter_subset(), 5000)
  })
   
  #Map of incidents, date input, hierarchy input
  
  #time plot, hierarchy input, zone/district/neighborhood input?
  
  output$time <- renderPlot({
    ggplot(blotter_sample(), aes(x = blotter_sample()$time)) + geom_freqpoly(stat = "bin", binwidth = 3600)
  })
  
  output$date <- renderPlot({
    ggplot(blotter_sample(), aes(x = blotter_sample()$date, fill = type)) + 
      geom_histogram(stat = "bin", bins = 12) #, color = cut(blotter_sample()$HIERARCHY, breaks = c(-1, 9, 99)))
  })
  
  #Count by hierarchy and neighborhood, date input
  
  #data table of ??
  
  #Download button
  
  #Observer~~

}

# Run the application 
shinyApp(ui = ui, server = server)

