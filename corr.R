corr <- function(directory, threshold = 0){

    # This function will calculate the correlation between sulfate and nitrate for 
    # data monitors that achieve a threshold of observations for each monitor
    
    setwd("C:/Users/mbroome/Documents/Coursera/R/Week_2/Programs/specdata")
    
    #Initialize complete as a data frame
    
    complete = data.frame(matrix(NA, nrow=0, ncol=0))
    
    # Initialize vectors
    
    corr <- 0
    corr_ans <- 0

    
    # Determine which monitors will be correlated based on the selected threshold
    # Utilize the complete function to build a list of monitors and the number of 
    # observations captured by the monitor 
    
    ob_by_mon <- complete("abc", id = 1:332)
    
    # Establish a vector that includes the monitors to be included in the correlation
    
    ob_by_mon <- as.data.frame(ob_by_mon)
    mon_included <- ob_by_mon[(ob_by_mon$total_count >= threshold),]
    mon_selected <- as.vector(mon_included$mon_number)
    
    mon_count <- length(mon_selected)
    
        
    # Setup a for loop to capture observations for 
    # the selected monitors as identified in id
    
        for (i in 1:mon_count){
            
            # develop a character string to match the csv file name in the 
            # specdata directory (001.csv ... 332.csv)
            
            if (mon_selected[i] > 99) { filler <- ""}
            if (mon_selected[i] < 100) { filler <- "0"}
            if (mon_selected[i] < 10) { filler <- "00"}
            file_name <- paste(filler, as.character(mon_selected[i]), ".csv", sep="")
            
            # read the file and save to mon.data
            
            mon.data <- read.csv(file_name)
            
            # Count the number of observations for both sulfate and nitrate
            
            mon.data <- mon.data[complete.cases(mon.data),]
            corr_mat_data <- cbind(mon.data$sulfate, mon.data$nitrate)
            corr_ans[i] <- cor(corr_mat_data[,1],corr_mat_data[,2])            
            }  
        corr <- corr_ans
    }
    
  
