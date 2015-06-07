library(dplyr)

# Reads in the household power consumption data.
read_data <- function() {
    data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?") %>%
        filter(Date == "1/2/2007" | Date == "2/2/2007")
    
    cbind(
        DateTime = strptime(paste0(data$Date, " ", data$Time), "%d/%m/%Y %H:%M:%S"),
        data[,3:9]
    )
}

# Writes out a png file of the given file name and
# runs plot_fn to write to the device.
write_png <- function(file_name, plot_fn) {
    png(file_name, width = 480, height = 480)
    plot_fn()
    dev.off()
}

# Gnerates the actual plot.
generate_plot <- function(data) {
    
    metering_columns = c(
        "Sub_metering_1",
        "Sub_metering_2",
        "Sub_metering_3"
    )
    
    colors = c("black", "red", "blue")
    
    all_readings <- do.call(rbind, lapply(metering_columns, function(col) {
        data.frame(DateTime = data$DateTime, Sub_metering = data[,col])
    }))
    
    with(all_readings, plot(
        DateTime,
        Sub_metering,
        type = "n",
        ylab = "Energy sub metering",
        xlab = ""))
    
    for (i in 1:length(metering_columns)) {
        lines(data$DateTime, data[,metering_columns[i]], col = colors[i])
    }
    
    legend("topright", metering_columns, col = colors, lty = 1)
}

# Run the whole process.
write_png("plot3.png", function() {
    generate_plot(read_data())
})