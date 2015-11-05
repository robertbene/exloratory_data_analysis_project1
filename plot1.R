plot1 <- function(filename, startDate = "2007-02-01", endDate = "2007-02-02") {
    data <- read.table(filename, header=TRUE, sep=";", na.strings = "?");
    start <- as.Date(startDate);
    end <- as.Date(endDate);
    
    dataSubset <- subset(data, as.Date(Date, format = "%d/%m/%Y") >= start & as.Date(Date, format = "%d/%m/%Y") <= end);
    
    title  <- "Global Active Power";
    xLabel <- "Global Active Power (kilowatts)";
    yLabel <- "Frequency";
    
    par(mar = c(4,4,2,2));
    
    png("plot1.png",  width = 480, height = 480, units = "px");
    hist(dataSubset$Global_active_power, col = "red", xlab = xLabel, ylab = yLabel, main = title);
    dev.off();
}