
#Lets read the data. 
readData <- function(file) {
  data <- read.csv(file, sep=";", stringsAsFactors=FALSE)
  data
}

#Subset the data to only have dates we neeed.
subsetData <- function(data, date1, date2) {
  
  #subset data and get a new dataframe with just dates from date1 and date2
  data <- subset(data, (data[,1] == c(date1)) | data[,1] == c(date2)) 
  
  #Return data.
  data
}

#We need weekdays
getWeekDays <- function(data) {
  weekDays <- strptime(paste(data$Date, data$Time), format='%d/%m/%Y %H:%M:%S')
  weekDays
}

#Plot histogram and save it. 
plotData <- function(data, weekDays) {
  
  #open a graphic device
  png("plot4.png")
  
  #Plot the 4 x 4 chart
  #par(mfrow) will sett the global variable for how the chart is plotted. 
  #We make it two coloums wide and with two rows.
  par(mfrow = c(2,2))
  
  #Plot the Global Active Power chart
  plot(weekDays, data$Global_active_power, type="l", xlab="", ylab="Global Active Power")
  
  #Plot the Voltage chart
  plot(weekDays, data$Voltage, type="l", xlab="", ylab="Voltage")
  
  #Plot the energy sub_metering chart. Turn of legend border and set colors.
  with(data, {
    plot(weekDays, data$Sub_metering_1, type="l", xlab="", ylab="")
    lines(weekDays, data$Sub_metering_2, type="l", xlab="", ylab="", col="red")
    lines(weekDays, data$Sub_metering_3, type="l", xlab="", ylab="Energy Sub Metering", col="blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           bty="n",
           lty=c(1,1),
           col=c("black", "red", "blue")
           )
  })
  
  #Plot the last chart. Global Reactive Power and add name to the x axis.
  plot(weekDays, data$Global_reactive_power, type="l", ylab="Global_reactive_power", xlab="datetime")
  
  
  #Finaly close the graphics device
  dev.off()
}

##Feed data from readData function into subsetData function with the dates we want. We can then work
#With a much smaler dataset.
data <- subsetData(readData("household_power_consumption.txt"), "2/2/2007", "1/2/2007")

#Now what we have data for just the days we want. We need a repsesentation of weekdays for charting. 
#Feed data into getWeekDays function to get datetimeobjects.
weekDays <- getWeekDays(data)

#Now that we have all data ready, we just need to plot it. 
plotData(data, weekDays)


