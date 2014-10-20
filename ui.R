#****************************#
#      MyGPSAnalyticsApp     #
#****************************#

#Programming Language : R
#Programming Environment : RStudio
#Web application framework: Shiny
#Datasets used in this analysis : _cabs.txt | new_abboip.txt

#Installing the Shiny Package in R
###---Install if necessary---# install.packages("shiny")  
#Include the package in the working directory
library(shiny)

#Creating a UI application
shinyUI(fluidPage(
  #Application title
  titlePanel("MyGPSAnalyticsApp"),
  wellPanel("The GPS Analytics App helps us analyze data by extracting various parameters from the GPS data we obtain from an automobile.",width = 36),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("parameter", "Select a parameter to analyze:", 
                  choices = c("Cab area density",
                              "Cab route",
                              "Cab route - Metered",
                              "Total distance travelled",
                              "Total time taken",
                              "Average speed",
                              "Total distance travelled - Metered",
                              "Total time taken - Metered",
                              "Average speed - Metered",
                              "Number of pickups",
                              "Total distance visualization",
                              "Total time visualization",
                              "Average speed visualization"
                              )
                  )
      ),
    
    mainPanel(
      conditionalPanel(condition = "input.parameter == 'Cab area density'",plotOutput("CabAreaDensity")
                       ),
      conditionalPanel(condition = "input.parameter == 'Cab route'",plotOutput("CabRoute")
                       ),
      conditionalPanel(condition = "input.parameter == 'Cab route - Metered'",plotOutput("CabRouteMetered")
                       ),
      conditionalPanel(condition = "input.parameter == 'Total distance travelled'",verbatimTextOutput("TotalDistance")
                       ),
      conditionalPanel(condition = "input.parameter == 'Total time taken'",verbatimTextOutput("TotalTime")
      ),
      conditionalPanel(condition = "input.parameter == 'Average speed'",verbatimTextOutput("AverageSpeed")
      ),
      conditionalPanel(condition = "input.parameter == 'Total distance travelled - Metered'",verbatimTextOutput("TotalDistanceMetered")
      ),
      conditionalPanel(condition = "input.parameter == 'Total time taken - Metered'",verbatimTextOutput("TotalTimeMetered")
      ),
      conditionalPanel(condition = "input.parameter == 'Average speed - Metered'",verbatimTextOutput("AverageSpeedMetered")
      ),
      conditionalPanel(condition = "input.parameter == 'Number of pickups'",verbatimTextOutput("NumberOfPickups")
      ),
      conditionalPanel(condition = "input.parameter == 'Total distance visualization'",plotOutput("DistanceVisualize")
      ),
      conditionalPanel(condition = "input.parameter == 'Total time visualization'",plotOutput("TimeVisualize")
      ),
      conditionalPanel(condition = "input.parameter == 'Average speed visualization'",plotOutput("AverageSpeedVisualize")
      )
      
      
      
      )
    )    

))
