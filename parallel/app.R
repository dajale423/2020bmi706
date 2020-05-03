library(shiny)
library(plotly)
library(rlist)
library(DT)

df <- read.csv('county_state_final_ver0429.csv')

all_parallel_variables <- c("Corona Deaths Per Million", "Death_per_M",
  "Corona Cases Per Thousand", "Case_per_thousand",
  "Corona Total Cases", "Cases",
  "Corona Virus Mortality Rate", "Mortality",
  "PM2.5", "Avr_PM2.5",
  "Poverty Level (All Ages)", "Poverty_all",
  "Poverty Level (Children)", "Poverty_child",
  "Population Density", "Pop_density",
  "Population size", "Population", 
  "Number of Large Aiports", "Large_airport",
  "Number of Medium-sized Airports", "Medium_airport",
  "Average Ozone Level", "Avr_Ozone")

all_parallel_variables_assignment <- c("Corona Deaths Per Million" = "Death_per_M",
                                       "Corona Cases Per Thousand" = "Case_per_thousand",
                                       "Corona Total Cases" = "Cases",
                                       "Corona Virus Mortality Rate" = "Mortality",
                                       "PM2.5" = "Avr_PM2.5",
                                       "Poverty Level (All Ages)" = "Poverty_all",
                                       "Poverty Level (Children)" = "Poverty_child",
                                       "Population Density" = "Pop_density",
                                       "Population size" = "Population", 
                                       "Number of Large Aiports" = "Large_airport",
                                       "Number of Medium-sized Airports" = "Medium_airport",
                                       "Average Ozone Level" = "Avr_Ozone")

all_parallel_variables.array = array(all_parallel_variables, dim = c(2,length(all_parallel_variables)/2))

# Define UI ----
ui <- fluidPage(
  titlePanel("COVID-19 Cases and Environmental Variables in the New England Region"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("parallel_variables", "Variables to include in Parallel Coordinate:",
                         all_parallel_variables_assignment),
      selectInput("parallel_constraint_variable", "Choose Variable to Constrain in the Parallel Coordinate:",
                    choices = all_parallel_variables_assignment),
      sliderInput("parallel_constraint_range", "Choose a range (percent) of constraint for the variable",
                  0, 100, c(0,0))
    ),
    mainPanel(
      plotlyOutput(outputId = "parallelCoord"), 
      DT::dataTableOutput("mytable")
    )
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  output$parallelCoord <- renderPlotly({

    parallel_dimensions <- list()
    
    #include variables that are checked "yes"
    
    for (variable_name in input$parallel_variables){
      if (identical(variable_name, input$parallel_constraint_variable)) {
        
        max_value <- max(df[variable_name])
        
        parallel_dimensions <- list.append(parallel_dimensions,
                                           list(range = c(as.formula(paste("~min(" , variable_name, ", na.rm = TRUE)")) ,
                                                          as.formula(paste("~max(" , variable_name, ", na.rm = TRUE)"))),
                                                constraintrange = c(0.01*max_value*input$parallel_constraint_range[1],
                                                                    0.01*max_value*input$parallel_constraint_range[2]),
                                                label = all_parallel_variables.array[1,all_parallel_variables.array[2,] == variable_name],
                                                values = as.formula(paste("~", variable_name))))
      }
      else {
          parallel_dimensions <- list.append(parallel_dimensions,
                                    list(range = c(as.formula(paste("~min(" , variable_name, ", na.rm = TRUE)")) ,
                                                  as.formula(paste("~max(" , variable_name, ", na.rm = TRUE)"))),
                                         label = all_parallel_variables.array[1,all_parallel_variables.array[2,] == variable_name], 
                                          values = as.formula(paste("~", variable_name))))
     }
    }
    

    df %>% plot_ly(type = 'parcoords', line = list(color = 'blue'), 
                   dimensions = parallel_dimensions
    )
  })
  
  #output the data table for values within the constraint range of parallel coordinates
  output$mytable = DT::renderDataTable({
    max_value <- max(df[input$parallel_constraint_variable])
    
    subset(df, get(input$parallel_constraint_variable) > 0.01*max_value*input$parallel_constraint_range[1] & 
             get(input$parallel_constraint_variable) < 0.01*max_value*input$parallel_constraint_range[2], 
           select = c("County","State",input$parallel_variables))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
