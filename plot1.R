
#Lets read the data. 
readData <- function(file) {
  data <- read.csv(file, sep=";", stringsAsFactors=FALSE)
  data
}

#Subset the data to only have dates we neeed.
subsetData <- function(data, date1, date2) {
  
  #subset data and get a new dataframe with just dates from date1 and date2
  data <- subset(data, (data[,1] == c(date1)) | data[,1] == c(date2)) 
  
  #Since we are only after "Global Active Power" we are only returning that.
  data[,3]
}

#Plot histogram and save it. 
plotData <- function(data) {
  
  #open a graphic device
  png("plot1.png")
  
  #Plot a histogram
  hist(as.numeric(data), col="red", main='Global Active Power', xlab="Global Active Power (kilowatts)")
  #title('Global Active Power', xlab="Global Active Power (kilowatts)")
  
  #Close the graphics device
  dev.off()
}

#Feed data from readData function into subsetData function with the dates we want. 
data <- subsetData(readData("household_power_consumption.txt"), "2/2/2007", "1/2/2007")

#Plot the data we recieved above
plotData(data)
