
## set working directory
setwd("~/Projects/ds200/r_code-EDA_project1")

## load libraries


library(lubridate)


##download data, read it, and save it in a variable
########################################

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
raw_data <- read.table(unz(temp, "household_power_consumption.txt"), header=TRUE, na.strings="?", sep=";")
unlink(temp)


####################################################
### create DateTime column with combined date/time converted to objects.
raw_data$DateTime <- lubridate::dmy_hms(paste(raw_data$Date, raw_data$Time))

# get the dates we need
raw_data <- raw_data %>%
  filter(year(DateTime) == 2007 & month(DateTime) == 2 & (day(DateTime) == 1 | day(DateTime) == 2)  )


summary(raw_data)

hist(raw_data$Global_active_power, main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red", cex.sub=0.8)

# copy plot to file
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()


#################
#####  Not used
################## 

####### for local testing

# raw_data <- read.table("household_power_consumption.txt", header=TRUE, na.strings="?", sep=";")


# raw_data <- read_delim(unz(temp, "household_power_consumption.txt"),na=c('?'),delim=";", col_types = list(col_date(format = "%e/%m/%Y"), col_time("%H:%M:%S"),col_double(),col_double(),col_double(),col_double(),col_double(),col_double(),col_double()  ))


# # raw_data = read_delim("household_power_consumption.txt",delim=";")
# temp <- readLines("household_power_consumption.txt")
# selected_rows = grep('(^1/2/2007|^2/2/2007)',temp)
# temp[selected_rows]
# # raw_data = read_delim(temp[selected_rows],delim=";")
# raw_data = read_table(temp[selected_rows],delim=";")

# raw_data
# unique(raw_data[selected_rows,]$Date)


# raw_data <- read_delim("household_power_consumption.txt",na=c('?'),delim=";", col_types = list(col_date(format = "%e/%m/%Y"), col_time("%H:%M:%S"),col_double(),col_double(),col_double(),col_double(),col_double(),col_double(),col_double()))