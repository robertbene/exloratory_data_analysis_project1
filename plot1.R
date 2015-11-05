plot1 <- function(filename, startDate = "2007-02-01", endDate = "2007-02-02") {
    ## Read full data
    data <- read.table(filename, header=TRUE, sep=";", na.strings = "?");
    
    ## Init start/end date
    start <- as.Date(startDate);
    end <- as.Date(endDate);
    
    ## Get subset
    dataSubset <- subset(data, as.Date(Date, format = "%d/%m/%Y") >= start & as.Date(Date, format = "%d/%m/%Y") <= end);
    
    ## Remove the full data buffer
    rm(data);
    
    ## Fix margins
    par(mar = c(4,4,2,2));
    
    ## Create histogram
    hist(dataSubset$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main = "Global Active Power");
    
    ## Copy to PNG file
    dev.copy(png, file="plot1.png", width = 480, height = 480);
    
    ## Close PNG device
    dev.off();
}