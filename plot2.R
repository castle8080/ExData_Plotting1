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
    with(data, plot(
        DateTime,
        Global_active_power,
        type = "l",
        ylab = "Global Active Power (kilowatts)",
        xlab = ""))
}

# Run the whole process.
write_png("plot2.png", function() {
    generate_plot(read_data())
})