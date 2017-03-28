pollutantmean <- function(directory, pollutant, id= 1:332){

    # This programs caluculates and mean of pollutants based on data captured
    # in a number of csv files found in "specdata" directory
    
    setwd("C:/Users/mbroome/Documents/Coursera/R/Week_2/Programs/specdata")
    
    # Determine the number of monitors that will be examined
    
    read_count <- length(id)
    
    # Set the counter for the cumulative pollutants and observations to 0
    # These will be used to calculate the mean (pollutant_count / observation_count)
    
    pollutant_count <- 0
    observation_count <- 0
    
    # Setup a for loop to capture pollutant Count and number of observations for 
    # the selected monitors as identified in id
    
        for (i in id){
            
            # develop a character string to match the csv file name in the 
            # specdata directory (001.csv ... 332.csv)
            
            if (i > 99) { filler <- ""}
            if (i < 100) { filler <- "0"}
            if (i < 10) { filler <- "00"}
            file_name <- paste(filler, as.character(i), ".csv", sep="")
            
            # read the file and save to mon.data
            
            mon.data <- read.csv(file_name)
            
            #remove na rows that include "na" in the "pollutant" column
            
            mon.readings <- mon.data[!is.na(mon.data[pollutant]),]
            
            if (nrow(mon.readings) > 0){
                # sum the pollutant count for each monitor reading
                
                pollutant_count <- sum(mon.readings[pollutant]) + pollutant_count

                # count the number of observations

                observation_count <- nrow(mon.readings[pollutant]) + observation_count
                
                }
            }  
            
    # Calculate the mean which will be returned by the function
    
    mean.reading <- pollutant_count / observation_count  
    
    }
  
