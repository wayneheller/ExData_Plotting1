################################################################################
# Exploratory Data Analysis - John's Hopkins University | Coursera             #
# Week 1 Course Project                                                        #
# Wayne Heller                                                                 #
# 2/11/2017                                                                    #
# Assignment- to recreate plots from the UC Irvine Machine Learning Repository #
# using the Base Plotting system                                               #
#                                                                              #
# Key Assumtion: assumes data file household_power_consumption.txt is in the   #
# working directory                                                            #
################################################################################

# Returns a data.frome of Global Active Power By Frequency for the data between
# 2007-02-01 and 2007-02-02.
# supports two methods of retrieval: read entire file and subset or read subset
# using sqldf.  the later method is about 5x faster!
# takes 1 input parameter to set retrieval method
readSubsetData <- function(useSQL = TRUE) {
    
    # setup timer
    timeStamp <- Sys.time()
    if (useSQL == TRUE) {
        library(sqldf)
        
        # read data using sqldf
        dfHPC <- read.csv.sql("household_power_consumption.txt", header = TRUE, sep = ";", 
                              sql = "select * from file where Date = '1/2/2007' or Date = '2/2/2007'")
        on.exit(closeAllConnections())
        
        # create a date time column for filtering
        datetime <- strptime(paste(dfHPC$Date, dfHPC$Time), "%d/%m/%Y %H:%M:%S")
        dfHPC$datetime = datetime
    }
    
    else {
        # read data file in its entirety
        dfHPC <- read.csv("household_power_consumption.txt", header = TRUE, sep=";", na.strings = "?")
        
        # create a date time column for filtering
        datetime <- strptime(paste(dfHPC$Date, dfHPC$Time), "%d/%m/%Y %H:%M:%S")
        dfHPC$datetime = datetime
        
        # subset the data to just the dates needed
        startDateTime <- strptime("01/02/2007 00:00:00", "%d/%m/%Y %H:%M:%S")
        endDateTime <- strptime("03/02/2007 00:00:00", "%d/%m/%Y %H:%M:%S")
        dfHPC <- subset(dfHPC, dfHPC$datetime >= startDateTime & dfHPC$datetime < endDateTime)
    }
    # show time taken
    print(Sys.time() - timeStamp)
    
    return(dfHPC)
    
}

# creates a histogram of Global Active Power By Frequency for the data between
# 2007-02-01 and 2007-02-02 and creates a png file of the plot
createPlot3 <- function() {
    
    df <- readSubsetData()
    
    # close any open graphics devices
    # this clears out any existing settings
    while(length(dev.list()>1)) {
        dev.off()
    }
    
    # create the plot on the screen device
    plot(df$datetime,df$Sub_metering_1, type ="l", ylab="Energy sub metering", xlab = "")
    lines(x=df$datetime, y=df$Sub_metering_2, col= "red")
    lines(x=df$datetime, y=df$Sub_metering_3, col= "blue")
    
    # add legend
    legend("topright", legend = names(df)[7:9], col = c("black", "red", "blue"), lty = 1)
    
    # copy the screen device to the plot file and close the png device
    dev.copy(png, "plot3.png")
    dev.off()
    
}