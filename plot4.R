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
readSubsetData <- function() {
    
    # Read data from downloaded dataset
    dfHPC <- read.csv("household_power_consumption.txt", header = TRUE, sep=";", na.strings = "?")
    
    # create a date time column for filtering
    datetime <- strptime(paste(dfHPC$Date, dfHPC$Time), "%d/%m/%Y %H:%M:%S")
    dfHPC$datetime = datetime
    
    # subset the data to just the dates needed
    startDateTime <- strptime("01/02/2007 00:00:00", "%d/%m/%Y %H:%M:%S")
    endDateTime <- strptime("03/02/2007 00:00:00", "%d/%m/%Y %H:%M:%S")
    dfHPC <- subset(dfHPC, dfHPC$datetime >= startDateTime & dfHPC$datetime < endDateTime)
    
    return(dfHPC)
    
}

# creates a histogram of Global Active Power By Frequency for the data between
# 2007-02-01 and 2007-02-02 and creates a png file of the plot
createPlot4 <- function() {
    
    dfHPC <- readSubsetData()
    
    # create 2 x 2 plot area
    par(mfrow = c(2,2))
    
    # upper left quadrant
    plot(df$datetime,df$Global_active_power, type ="l", ylab="Global Active Power (kilowatts)", xlab = "")
    
    #upper right quadrant
    plot(df$datetime,df$Voltage, type ="l", ylab="Voltage", xlab = "datetime")
    
    # lower left quadrant
    plot(df$datetime,df$Sub_metering_1, type ="l", ylab="Energy sub metering", xlab = "")
    lines(x=df$datetime, y=df$Sub_metering_2, col= "red")
    lines(x=df$datetime, y=df$Sub_metering_3, col= "blue")
    
    # add legend
    legend("topright", legend = names(df)[7:9], col = c("black", "red", "blue"), lty = 1)
    
    # lower right
    #upper right quadrant
    plot(df$datetime,df$Global_reactive_power, type ="l", ylab="Global_reactive_power", xlab = "datetime")
    
    # copy the screen device to the plot file and close the png device
    dev.copy(png, "plot4.png")
    dev.off()
    
}