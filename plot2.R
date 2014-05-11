
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
  png("plot2.png")
  
  #Plot a chart
  plot(weekDays, data$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
  
  #Close the graphics device
  dev.off()
}

##Feed data from readData function into subsetData function with the dates we want. 
data <- subsetData(readData("household_power_consumption.txt"), "2/2/2007", "1/2/2007")
weekDays <- getWeekDays(data)

#Plot it!
plotData(data, weekDays)

