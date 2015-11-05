plot4 <- function(filename, startDate = "2007-02-01", endDate = "2007-02-02") {
    ## Read full data
    data <- read.table(filename, header=TRUE, sep=";", na.strings = "?");
    
    ## Init start/end date
    start <- as.Date(startDate);
    end <- as.Date(endDate);
    
    ## Get subset
    dataSubset <- subset(data, as.Date(Date, format = "%d/%m/%Y") >= start & as.Date(Date, format = "%d/%m/%Y") <= end);
    
    ## Remove the full data buffer
    rm(data);
    
    ## Converting dates
    datetime <- paste(as.Date(dataSubset$Date, format = "%d/%m/%Y"), dataSubset$Time);
    datetimePos <- as.POSIXct(datetime);
    
    ## Setup columns,rows and margins
    par(mfrow = c(2,2), mar = c(4,4,1,1));
    
    with(dataSubset, {
        ## Plot the global active power
        plot(Global_active_power~datetimePos, type="l", ylab="Global active power (kilowatts)", xlab="");
        
        ## Plot the voltage
        plot(Voltage~datetimePos, type="l", ylab="Voltage", xlab="datetime");
        
        ## Plot energy sub metering
        plot(Sub_metering_1~datetimePos, type="l",  ylab="Energy sub metering", xlab="");
        lines(Sub_metering_2~datetimePos,col='red');
        lines(Sub_metering_3~datetimePos,col='blue');
        
        ## Add legend
        legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), pch="-", bty = "n");
        
        ## Plot the global reactive power
        plot(Global_reactive_power~datetimePos, type="l", ylab="Global reactive power", xlab="datetime");
    });
    
    ## Copy to PNG file
    dev.copy(png, file="plot4.png", width = 480, height = 480);
    
    ## Close PNG device
    dev.off();
}