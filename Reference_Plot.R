##Set directory
currdir <- "./data"
if(!dir.exists("./data")) dir.create("./data")
setwd(currdir)


##download zip file & extract contents

downloadurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip" 
zipfile <- "Household_power_consumption.zip"
download.file(downloadurl, zipfile)
if(file.exists(zipfile)) unzip(zipfile)
 
## load date into table

data_full <- read.csv("household_power_consumption.txt", header=T, sep=';', 
                      na.strings="?", nrows=2075259, check.names=F, 
                      stringsAsFactors=F, comment.char="", quote='\"')
   View(household_power_consumption)
  
##subset for relevant data & transform date format
   HPCsubset <- subset(household_power_consumption,Date %in% c("1/2/2007","2/2/2007"))
   HPCsubset$Date <- as.Date(HPCsubset$Date, format="%d/%m/%Y")
   
##Plot 1 generate histogram & PNG
with(HPCsubset, hist(Global_active_power, main="Global Active Power", 
     xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red"))

dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()

## Plot 2 generate weekdays, plot & PNG
weekday <- paste(as.Date(HPCsubset$Date), HPCsubset$Time)
HPCsubset$weekday<- as.POSIXct(weekday)

plot(HPCsubset$Global_active_power~HPCsubset$weekday, type="l", ylab="Global 
     Active Power (kilowatts)", xlab="")

dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()

## Plot 3 generate plot & PNG
with(HPCsubset, {
  plot(Sub_metering_1~weekday, type="l", ylab="Global Active Power (kilowatts)"
       , xlab="")
  lines(Sub_metering_2~weekday,col='Red')
  lines(Sub_metering_3~weekday,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
})
 
 dev.copy(png, file="plot3.png", height=480, width=480)
 dev.off()

 ## Plot 4 divide area, generate plots & PNG
 par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
 with(HPCsubset, {
   plot(Global_active_power~weekday, type="l", 
        ylab="Global Active Power (kilowatts)", xlab="")
   plot(Voltage~weekday, type="l", 
        ylab="Voltage (volt)", xlab="")
   plot(Sub_metering_1~weekday, type="l", 
        ylab="Global Active Power (kilowatts)", xlab="")
   lines(Sub_metering_2~weekday,col='Red')
   lines(Sub_metering_3~weekday,col='Blue')
   legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
          legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
   plot(Global_reactive_power~weekday, type="l", 
        ylab="Global Rective Power (kilowatts)",xlab="")
 })
 dev.copy(png, file="plot4.png", height=480, width=480)
 dev.off()
 