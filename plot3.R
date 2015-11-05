plot3 <- function(filename, startDate = "2007-02-01", endDate = "2007-02-02") {
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
    par(mfrow = c(1,1), mar = c(4,4,1,1));
    
    ## Create plot
    with(dataSubset, {
        plot(Sub_metering_1~datetimePos, type="l",  ylab="Energy sub metering", xlab="");
        lines(Sub_metering_2~datetimePos,col='red');
        lines(Sub_metering_3~datetimePos,col='blue');
    });
    
    ## Add legend
    legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), pch="-");
    
    ## Copy to PNG file
    dev.copy(png, file="plot3.png", width = 480, height = 480);
    
    ## Close PNG device
    dev.off();
}