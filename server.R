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
#library(shiny)

#Logic for the analysis
shinyServer(function(input,output){
  #Return the selected dataset
  parameterInput <- reactive({
    switch(input$parameter,
           "Cab area density" = CabAreaDensity,
           "Cab route" = CabRoute,
           "Cab route - Metered" = CabRouteMetered,
           "Total distance travelled" = TotalDistance,
           "Total time taken" = TotalTime,
           "Average speed" = AverageSpeed,
           "Total distance travelled - Metered" = TotalDistanceMetered,
           "Total time taken - Metered" = TotalTimeMetered,
           "Average speed - Metered" = AverageSpeedMetered,
           "Number of pickups" = NumberOfPickups,
           "Total distance visualization" = DistanceVisualize,
           "Total time visualization" = TimeVisualize,
           "Average speed visualization" =AverageSpeedVisualize
           )
  })
  
  output$CabAreaDensity <- renderPlot({
    #Calling necessary libraries
    library(maps)
    library(mapproj)
    library(rworldmap)
    library(rworldxtra)
    #Plotting the lat-long values
    cars_abboip_data  <- read.table("new_abboip.txt",header = FALSE)
    cars_abboip_data_df  <- data.frame(cars_abboip_data)
    newmap  <- getMap(resolution="high")
    lat  <- cars_abboip_data_df$V1
    long  <- cars_abboip_data_df$V2
    plot(newmap,ylim = c(37.75000,37.75160), xlim = c(-122.39000,-122.39450),asp = 1)
    points(long,lat, col = "red", cex = 1.2)
  })
  
  output$CabRoute <- renderPlot({
    #Calling necessary libraries
    library(maps)
    library(mapproj)
    library(rworldmap)
    library(rworldxtra)
    #Plotting the lat-long values
    cars_abboip_data  <- read.table("new_abboip.txt",header = FALSE)
    cars_abboip_data_df  <- data.frame(cars_abboip_data)
    newmap  <- getMap(resolution="high")
    lat  <- cars_abboip_data_df$V1
    long  <- cars_abboip_data_df$V2
    plot(newmap,ylim = c(37.75000,37.75160), xlim = c(-122.39000,-122.39450),asp = 1)
    points(long,lat,type = "o" ,col = "blue", cex = 1.2)
  })
  
  output$CabRouteMetered <- renderPlot({
    #Calling necessary libraries
    library(maps)
    library(mapproj)
    library(rworldmap)
    library(rworldxtra)
    #Select those columns which are metered(i.e == 1)
    cars_abboip_data_metered_temp  <- (cars_abboip_data$V3 == 1)
    cars_abboip_data_metered  <- cars_abboip_data[cars_abboip_data_metered_temp,]
    #Plotting the lat-long values
    newmap  <- getMap(resolution="high")
    lat_metered  <- cars_abboip_data_metered$V1
    long_metered  <- cars_abboip_data_metered$V2
    plot(newmap,ylim = c(37.75000,37.75160), xlim = c(-122.39000,-122.39450),asp = 1)
    points(long_metered,lat_metered,type = "o" ,col = "green", cex = 1.2)
  })
  
  output$TotalDistance <- renderText({
    #Getting the input values through thedataframe
    cars_abboip_data  <- read.table("new_abboip.txt",header = FALSE)
    cars_abboip_data_df  <- data.frame(cars_abboip_data)
    #Selecting only the first 2 columns of cars_abboip_data
    cars_abboip_data_col1  <- cbind(cars_abboip_data$V1)
    cars_abboip_data_col2  <- cbind(cars_abboip_data$V2)
    #Finding the difference between the successive rows in the 1st column
    cars_abboip_data_col1_diff  <- data.frame(diff(as.matrix(cars_abboip_data_col1)))
    cars_abboip_data_col2_diff  <- data.frame(diff(as.matrix(cars_abboip_data_col2)))
    #Applying distance formula to the variables above
    cars_abboip_data_col1_diff_sq  <- cars_abboip_data_col1_diff^2
    cars_abboip_data_col2_diff_sq  <- cars_abboip_data_col2_diff^2
    cars_abboip_data_col1and2_sum  <- cars_abboip_data_col1_diff_sq + cars_abboip_data_col2_diff_sq
    cars_abboip_data_distance  <- sqrt(cars_abboip_data_col1and2_sum)
    #Multiplying the differnce by 111kms - Conversion between lat-long to kms
    cars_abboip_data_TotalDistance  <- colSums(cars_abboip_data_distance)*111
    cars_abboip_data_TotalDistance_int  <- ceiling(cars_abboip_data_TotalDistance)
    #OUTPUT
    paste("Total distance travelled: ",cars_abboip_data_TotalDistance_int,"kms")
  })
  
  output$TotalTime <- renderText({
    #Getting the column 4 values from the dataframe
    cars_abboip_data_col_time  <- cbind(cars_abboip_data$V4)
    #Differnce between successive rows in seconds
    cars_abboip_data_col_time_diff  <- data.frame(diff(as.matrix(cars_abboip_data_col_time)))
    cars_abboip_data_col_time_diff_sum  <- sum(cars_abboip_data_col_time_diff)*-1
    total_time_hours  <- (cars_abboip_data_col_time_diff_sum %/% 3600)
    #OUTPUT
    paste("Total time taken: ",total_time_hours,"hrs")
    })
  
  output$AverageSpeed <- renderText({
    avg_speed  <- (cars_abboip_data_TotalDistance_int %/% total_time_hours)
    #OUTPUT
    paste("Average speed: ",avg_speed,"kmph")
  })
  
  output$TotalDistanceMetered <- renderText({
    #Select those columns which are metered(i.e == 1)
    cars_abboip_data_metered_temp  <- (cars_abboip_data$V3 == 1)
    cars_abboip_data_metered  <- cars_abboip_data[cars_abboip_data_metered_temp,]
    #Selecting only the first 2 columns of cars_abboip_data_metered
    cars_abboip_data_metered_col1  <- cbind(cars_abboip_data_metered$V1)
    cars_abboip_data_metered_col2  <- cbind(cars_abboip_data_metered$V2)
    #Finding the difference between the successive rows in the 1st & 2nd column
    cars_abboip_data_metered_col1_diff[(cars_abboip_data_metered_col1_diff>0.00999),]  <- 0.00000
    cars_abboip_data_metered_col2_diff[(cars_abboip_data_metered_col2_diff>0.00999),]  <- 0.00000
    cars_abboip_data_metered_col1_diff  <- data.frame(diff(as.matrix(cars_abboip_data_metered_col1)))
    cars_abboip_data_metered_col2_diff  <- data.frame(diff(as.matrix(cars_abboip_data_metered_col2)))
    #Applying distance formula to the variables above
    cars_abboip_data_metered_col1_diff_sq  <- cars_abboip_data_metered_col1_diff^2
    cars_abboip_data_metered_col2_diff_sq  <- cars_abboip_data_metered_col2_diff^2
    cars_abboip_data_metered_col1and2_sum  <- cars_abboip_data_metered_col1_diff_sq + cars_abboip_data_metered_col2_diff_sq
    cars_abboip_data_metered_distance  <- sqrt(cars_abboip_data_metered_col1and2_sum)
    #Multiplying the differnce by 111kms - Conversion between lat-long to kms
    cars_abboip_data_metered_TotalDistance  <- colSums(cars_abboip_data_metered_distance)*111
    cars_abboip_data_metered_TotalDistance_int  <- ceiling(cars_abboip_data_metered_TotalDistance)
    #OUTPUT
    paste("Total distance travelled: ",cars_abboip_data_metered_TotalDistance_int,"kms")
  })
  
  output$TotalTimeMetered <- renderText({
    #Getting the column 4 values from the dataframe
    cars_abboip_data_metered_col_time  <- cbind(cars_abboip_data_metered$V4)
    #Differnce between successive rows in seconds
    cars_abboip_data_metered_col_time_diff  <- data.frame(diff(as.matrix(cars_abboip_data_metered_col_time)))
    cars_abboip_data_metered_col_time_diff[(cars_abboip_data_metered_col_time_diff < -99),]  <- -50
    cars_abboip_data_metered_col_time_diff_sum  <- sum(cars_abboip_data_metered_col_time_diff)*-1
    total_time_hours_metered  <- (cars_abboip_data_metered_col_time_diff_sum %/% 3600)
    #OUTPUT
    paste("Total time taken: ",total_time_hours_metered,"hrs")
  })
  
  output$AverageSpeedMetered <- renderText({
    avg_speed_metered  <- (cars_abboip_data_metered_TotalDistance_int %/% total_time_hours_metered)
    #OUTPUT
    paste("Average speed: ",avg_speed_metered,"kmph")
  })
  
  output$NumberOfPickups <- renderText({
    library(plyr)
    pickupNumber <- count(data.frame(embed(rev(cars_abboip_data$V3),2)))
    #pickupNumber gives us a data.frame of values
    #We are interested in soting out the (0,1)'s in the entire data.frame
    pickupNumberTotal  <- pickupNumber[[3]][2]
    #OUTPUT
    paste("Number of pickups: ",pickupNumberTotal)
  })
  
  output$DistanceVisualize <- renderPlot({
    #A visualization of the Total distance travelled metered vs empty
    distanceVisualizeSlices <- c((cars_abboip_data_TotalDistance_int-cars_abboip_data_metered_TotalDistance_int),cars_abboip_data_metered_TotalDistance_int)
    distanceVisualizeLabels <- c("Distance travelled empty","Distance travelled metered")
    pie(distanceVisualizeSlices, labels = distanceVisualizeLabels, main="Visualization of the Total Distance Travelled")
  })
  
  output$TimeVisualize <- renderPlot({
    #A visualization of the Total time taken metered vs empty
    timeVisualizeSlices <- c((total_time_hours-total_time_hours_metered),total_time_hours_metered)
    timeVisualizeLabels <- c("Time taken empty","Time taken metered")
    pie(timeVisualizeSlices, labels = timeVisualizeLabels, main="Visualization of the Total Time Taken")
  })
 
  output$AverageSpeedVisualize <- renderPlot({
    #A visualization of the variation in average speed metered vs empty
    total_distance_empty  <- cars_abboip_data_TotalDistance_int-cars_abboip_data_metered_TotalDistance_int
    total_time_empty <- total_time_hours-total_time_hours_metered
    avg_speed_empty  <- (total_distance_empty/total_time_empty)
    barplot(matrix(c(avg_speed,avg_speed_empty,avg_speed_metered), nr=3), beside=TRUE, 
            main="Variation in average speed when empty and when metered",
            col=c("aquamarine3", "coral","blue"),names.arg=c("Average speed","Average speed (empty)","Average speed (metered)"),
            ylim=range(0,(max(avg_speed,avg_speed_metered)+10)))
  })
  
  
})
