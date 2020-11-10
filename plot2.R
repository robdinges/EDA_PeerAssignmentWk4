#2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510"
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# call relevant libraries
library(dplyr)

# load data
data <- readRDS('summarySCC_PM25.rds')

# filter and group
balt <- subset(data, fips == "24510")

baltpy <- balt %>%
        group_by(year) %>%
        summarise(sum = sum(Emissions/1000))

# open png file and plot the graph
png(filename = "plot2.png", 
    width = 480, 
    height = 480)

plot(baltpy, 
     type="b", 
     main='Yearly PM2.5 Emissions Baltimore City, MD',
     xlab="Year", 
     ylab="PM2.5 emitted (in 1000 tons)")

with(baltpy,axis(1, seq(min(year),max(year),1)))
dev.off()

