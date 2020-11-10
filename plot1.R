
#1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?  
# Using the base plotting system, make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008.

# call relevant libraries
library(dplyr)

# load data
data <- readRDS('summarySCC_PM25.rds')

# filter and group
totpy <- data %>%
        group_by(year) %>%
        summarise(sum = sum(Emissions/1000))

# open png file and plot the graph
png(filename = "plot1.png", 
    width = 480, 
    height = 480)

plot(totpy, 
     type="b", 
     main='Total Yearly PM2.5 Emissions',
     xlab="Year", 
     ylab="PM2.5 emitted (in 1000 tons)")

with(totpy,axis(1, seq(min(year),max(year),1)))
dev.off()

