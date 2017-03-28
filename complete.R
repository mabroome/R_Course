complete <- function(directory, id= 1:332){

    # This program captures the monitor number and the total number of observations made
    # by the monitor. id selects the monitors to be interogated 
    
    setwd("C:/Users/mbroome/Documents/Coursera/R/Week_2/Programs/specdata")
    
    #Initialize complete as a data frame
    
    complete = data.frame(matrix(NA, nrow=0, ncol=0))
    
    # Initialize vectors
    
    total_count <- 0
    mon_number <- 0
    
    # Determine the number of monitors that will be examined
    
    read_count <- length(id)
    
    # Internal counter within the for loop to populate vectors of total_count of observations
    # for each monitor and the associated monitor id
    
    int_count <- 1
    
    # Setup a for loop to capture observations for 
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
            
            # Remove observations that are not complete
            
            mon.data <- mon.data[complete.cases(mon.data),]

            
            # Count the number of observations for both sulfate and nitrate
            
            total_count[int_count] <- nrow(mon.data)
            
            # Associate the monitor with the total_count
            
            mon_number[int_count] <- i
            
            # Increment the internal counter by 1
            
            int_count <- int_count + 1
            
        
            }  
            
    # Combine the monitor number (mon_number) and the associated number of observations 
    # (total_count)
    
    complete <- cbind(mon_number,total_count)
    
    }
  
