#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load packages
library("ggplot2")
library("dplyr")
library("shiny")
library("DT")
library("tools")
library("shinyjs")
library("shinythemes")

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
neighborhoods <- sort(unique(blotter_data$INCIDENTNEIGHBORHOOD))

# Define UI
ui <- fluidPage(theme = shinytheme("sandstone"),
  
  useShinyjs(), 
  
   # Application title
   titlePanel("Pittsburgh Police Blotter Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        #City council district input
         checkboxGroupInput(inputId = "selected_districts",
                            label = "Select Council District(s)",
                            choices = c(1:9),
                            selected = c(1:9)
        ),
        
        #date range input
        dateRangeInput(inputId = "date_range",
                       label = "Date Range: yyyy-mm-dd",
                       start = "2009-01-01",
                       end = "2019-01-01",
                       startview = "year"),
        
        #Neighborhood selection, adjusted from https://stackoverflow.com/questions/24916115/select-deselect-all-button-for-shiny-variable-selection
        radioButtons(
          inputId="radio",
          label="Neighborhood Selection Type:",
          choices=list(
            "All",
            "Manual Select"
          ),
          selected="All"),
        
        #If manual selection used above, give neighborhood options
        conditionalPanel(
          condition = "input.radio != 'All'",
          checkboxGroupInput(
            'neighborhoods', 
            'Neighborhoods to Show:',
            choices= neighborhoods, 
            selected = c("Central Oakland", "Shadyside", "Squirrel Hill North", "Squirrel Hill South")
          )
        ),
        
        downloadButton("downloadData", "Download")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput(outputId = "time"),
         br(),
         plotOutput(outputId = "date"),
         br(),
         plotOutput(outputId = "hier"),
         br(),
         dataTableOutput(outputId = "DT")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Neighborhood filter
  data <- reactive({
    if(input$radio == "All"){
      blotter_data
    } else {
      filter(blotter_data, INCIDENTNEIGHBORHOOD %in% input$neighborhoods)
    }
  })
  
  #Council district and date filter
  blotter_subset <- reactive({
    req(input$selected_districts)
    filter(data(), COUNCIL_DISTRICT %in% input$selected_districts & 
             between(INCIDENTTIME, as.POSIXct(input$date_range[[1]]), as.POSIXct(input$date_range[[2]])))
  })
  
  #Decrease data size or populate small n user selections ~might be misleading~
  blotter_sample <- reactive({
    sample_n(blotter_subset(), 5000, replace = T)
  })
  
  #Function that returns logical value for observer below
  is.all <- function(x){
    if(x == "All"){
      return(T)
    } else {
      return(F)
    }
  }
  
  #observer that hides district selection if manual neighborhood selection is used
  observe({
    toggle(id = "selected_districts", condition = is.all(input$radio))
  })
  
  #time vs blotter count plot
  output$time <- renderPlot({
    ggplot(blotter_sample(), aes(x = blotter_sample()$time)) + geom_freqpoly(stat = "bin", binwidth = 3600) +
      labs(x = "Time of Day (Ignore data date)", y = "Count", title = "Count of Police Blotter Incidents by Time of Day")
  })
  
  #time of year vs blotter count plot
  output$date <- renderPlot({
    ggplot(blotter_sample(), aes(x = blotter_sample()$date, fill = type)) + 
      geom_histogram(stat = "bin", bins = 12) +
      labs(x = "Time of Year (Ignore data year)", y = "Count", title = "Count of Police Blotter Incidents by Time of Year and Type")
  })
  
  #hierarchy status vs time plot
  output$hier <- renderPlot({
    ggplot(blotter_sample(), aes(x = type, y = time)) +
      geom_violin(scale = "area", fill = "forest green") +
      labs(x = "UCR Code Types", y = "Time of Day (Ignore data date)", Title = "UCR Code Type Distribution by Time of Day")
  })
  
  #Datatable output
  output$DT <- renderDataTable({
    blotter_subset()
  })
  
  #csv download
  output$downloadData <- downloadHandler(
    filename = "PGH_Blotter_Data_Subset.csv",
    content = function(file){
      write.csv(blotter_subset(), file, row.names = F)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

