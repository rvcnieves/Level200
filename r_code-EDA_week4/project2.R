# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip

##download data, read it, and save it in a variable
########################################

# temp <- tempfile()
# download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",temp)
# 
# con <- unz(temp, filename = "Source_Classification_Code.rds",open = "rb")
# # con2 <- gzcon(con)
# pm25data <- readRDS(con)
# codeTable <- readRDS(unz(temp, "Source_Classification_Code.rds"))
# #unz(temp, "summarySCC_PM25.rds")
# unlink(temp)
# 



##### load rds files from working directory.  Was unable to download zip file and then read files, see above.
setwd("~/OneDrive - Red Ventures/MAC_cnieves/Projects/ds_Level200/Level200/r_code-EDA_week4")
code_data <- readRDS("Source_Classification_Code.rds")
# pm_data <- readRDS("summarySCC_PM25.rds")
# as.Date(as.character(pm_data$year),"%Y")

### look at the structure of the pm table
str(pm_data)

###1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

### ANSWER:  Yes. There is a decrease in emssions.  However, from 2002 to 2005, the reduction is minimal (~3%).

### per year emissions
pY_em <- aggregate(Emissions ~ year,data = pm_data,sum)
### Plot with barplot
barplot(pY_em$Emissions,names.arg = pY_em$year,col="blue", axes =TRUE,ylab = "Total PM2.5 Emissions",xlab="Year")
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()
########################
###2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (\color{red}{\verb|fips == "24510"|}fips=="24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

### ANSWER:  Yes, but as we can see in the plot, there was an increase from 2002 and 2005, before going to a lower reading in 2008.

### per year emissions and subsetting by provided county within the given city
pY_em_bc <- aggregate(Emissions ~ year,data = subset(pm_data,fips=="24510"),sum)
### Plot with barplot
barplot(pY_em_bc$Emissions,names.arg = pY_em_bc$year,col="blue", axes =TRUE,ylab = "Total PM2.5 Emissions in Baltimore City",xlab="Year")
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()
########################
###3.  Of the four types of sources indicated by the \color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

### ANSWER: 
###   Non-Road: saw a decrease in emissions from 1999-2008, but slighly increased from 2002 to 2005 before slighly going down on 2008.  This might have been due to rule changes that were implemented between 1999 and 2002 that controlled these emissions.  After these changes, no further significant improvements in emission reduction was achieved.  
###   NonPoint: saw a decrease in emissions from 1999-2008, but stalled from 2002 to 2005 before going down on 2008.  Could be due to similar scenario described in the previous item.  In addition, the city seems to have more emission of this type compared to other types.  
###   On-Road: saw a continuous decrease in emissions from 1999-2008.  
###   Point: saw an increase in emissions from 1999-2008, however, from 2005 to 2008, it had a decrease in emissions.  This sharp increase could have been associated to a new facility that contributed to the emissions.

### load library ggplot2
library(ggplot2)
### aggregate by year/type and sum total emissions per each category.
# pY_em_type <- aggregate(Emissions ~ year+type,data = pm_data,sum)
pY_em_type_bc <- aggregate(Emissions ~ year+type,data = subset(pm_data,fips=="24510"),sum)
# pY_em_type_bc$year <- as.Date(as.character(pY_em_type_bc$year), format="%Y")
### plot barplots with ggplot2 and use type for color

ggplot(pY_em_type_bc, aes(fill=type, y=Emissions, x=year)) + 
  geom_bar(position="dodge", stat="identity") 
  
# +
  # scale_x_date(date_breaks = "1 year", 
               # limits = as.Date(as.character(pY_em_type_bc$year), format="%Y"),
               # date_labels="%Y" )
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()

########################
###4.  Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

###  Answer: Emissions from Coal related sources decreased from 1999 to 2002, then it had a slighly increase from 2002 to 2005 before decreasing in 2008.  This decrease in emissions could be related to rules implemented that year to help reduce emissions or even to the reduction in economy activity after the 2008 financial crisis.

### get the rows that have Coal in their name
hasCoal <- grepl("Coal",code_data$Short.Name)

### get the scc codes that have Coal in their name
scc_coal <- code_data$SCC[hasCoal]

#### aggregate data by total emissions per year on those SCC's that have coal in their name
pY_em_Coal <- aggregate(Emissions ~ year,data = subset(pm_data,pm_data$SCC %in% scc_coal),sum)

#bar plot
barplot(pY_em_Coal$Emissions,names.arg = pY_em_Coal$year,col="blue", axes =TRUE,ylab = "Total Coal related PM2.5 Emissions",xlab="Year")
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()
########################
###5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

### ANSWER:  They changed to almost 50% between 1999 and 2002, but then continue decreasing slighty.  This might be associated to legislation that helped control emissions.

### check which codes are associated to vehicle sources
isVehicle <- grepl("Vehicle",code_data$SCC.Level.Two)
scc_vehicle <- code_data$SCC[isVehicle]
pY_em_Vehicle_bc <- aggregate(Emissions ~ year,data = subset(pm_data,pm_data$SCC %in% scc_vehicle & fips=="24510"),sum)

#bar plot
barplot(pY_em_Vehicle_bc$Emissions,names.arg = pY_em_Vehicle_bc$year,col="blue", axes =TRUE,ylab = "Total Vehicle related PM2.5 Emissions in Baltimore City",xlab="Year")
dev.copy(png, file="plot5.png", height=480, width=480)
dev.off()

########################
###6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (\color{red}{\verb|fips == "06037"|}fips=="06037"). Which city has seen greater changes over time in motor vehicle emissions?

### ANSWER: In percentages, Baltimore City had a significant change of ~52% reduction from 1999 to 2002 while LA had an increase of 17%.  In addition, the total Emissions is way higher that in Baltimore City. On the other hand, from 2005 to 2008, LA emissions decreased but stay higher than in 1999. The high emissions in LA makes it high risk city to live in.

###  get the data for Los Angeles

pY_em_Vehicle_LA <- aggregate(Emissions ~ year,data = subset(pm_data,pm_data$SCC %in% scc_vehicle & fips=="06037"),sum)

bc_la_mrg <- merge(pY_em_Vehicle_bc,pY_em_Vehicle_LA,by="year")

names(bc_la_mrg) <- c('year','em_bc',"em_la")
head(bc_la_mrg)

### impute to have Emissions in one column.
library(tidyr)
long_bc_la_mrg <- bc_la_mrg %>% gather(location,Emissions,em_bc:em_la)
long_bc_la_mrg


### looking at the plot with log(Emissions) in order to appreciate the changes between the two locations,considering they have different value ranges.
ggplot(long_bc_la_mrg, aes(fill=location, y=log(Emissions), x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "With log(Emissions)")
dev.copy(png, file="plot6.png", height=480, width=480)
dev.off()
### without the log
ggplot(long_bc_la_mrg, aes(fill=location, y=(Emissions), x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Without log(Emissions)")
dev.copy(png, file="plot7.png", height=480, width=480)
dev.off()

## calculating pct change
-(bc_la_mrg[1,]-bc_la_mrg[2,]) / bc_la_mrg[1,]
