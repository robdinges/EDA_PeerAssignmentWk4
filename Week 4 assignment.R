
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

#3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999–2008 
# for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 
# plotting system to make a plot answer this question.

# call relevant libraries
library(dplyr)
library(ggplot2)

# load data
data <- readRDS('summarySCC_PM25.rds')

# filter and group
balt <- subset(data, fips == "24510")

baltpy <- balt %>%
        group_by(year, type) %>%
        summarise(sum = sum(Emissions/1000))

# open png file and plot the graph
png(filename = "plot3.png", 
    width = 480, 
    height = 480)

baltpy %>% ggplot() +
        geom_line(aes(baltpy$year, baltpy$sum, col=type)) +
        labs(
                title = "Yearly PM2.5 Emissions Baltimore City, MD",
                subtitle = "by type of source",
                x = "Year",
                y = "PM2.5 emitted (in 1000 tons)"
        ) +
        scale_color_discrete(name="Type of Source") + 
        scale_x_continuous(breaks = c(baltpy$year))

dev.off()

#4
# Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999–2008?

# call relevant libraries
library(dplyr)

# load data
data <- readRDS('summarySCC_PM25.rds')
poll <- readRDS('Source_Classification_Code.rds')
mrg <- merge(data, poll, by='SCC')

ccrs <- subset(mrg, EI.Sector == "Fuel Comb - Comm/Institutional - Coal")
#ccrs <- mrg %>% filter(grepl("comb", Short.Name, ignore.case = TRUE) & 
#                         grepl("coal", Short.Name, ignore.case = TRUE))

# filter and group
ccrspy <- ccrs %>%
        group_by(year) %>%
        summarise(sum = sum(Emissions/1000))

# open png file and plot the graph
png(filename = "plot4.png", 
    width = 480, 
    height = 480)

plot(ccrspy, 
     type="b", 
     main='Yearly PM2.5 for Cole Comb related sources',
     xlab="Year", 
     ylab="PM2.5 emitted (in 1000 tons)")

with(ccrspy,axis(1, seq(min(year),max(year),1)))
dev.off()

#5
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# call relevant libraries
library(dplyr)
library(ggplot2)

# load data
data <- readRDS('summarySCC_PM25.rds')
poll <- readRDS('Source_Classification_Code.rds')
mrg <- merge(data, poll[,'Short.Name'], by='SCC')

# filter and group
mc <- mrg %>% filter(grepl("motor", Short.Name, ignore.case = TRUE) & 
                grepl("vehicle", Short.Name, ignore.case = TRUE) & 
                fips == "24510")

mcpy <- mc %>%
        group_by(year) %>%
        summarise(sum = sum(Emissions/1000))

# open png file and plot the graph
png(filename = "plot5.png", 
    width = 480, 
    height = 480)

mcpy %>% ggplot() +
        geom_line(aes(year, sum)) +
        labs(
            title = "Yearly PM2.5 Emissions Baltimore City, MD",
            subtitle = "from Motor Vehicles",
            x = "Year",
            y = "PM2.5 emitted (in 1000 tons)"
        ) +
        xlim(1999, 2008)
dev.off()

#6
# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037")
# Which city has seen greater changes over time in motor vehicle emissions?

# call relevant libraries
library(dplyr)
library(ggplot2)

# load data
data <- readRDS('summarySCC_PM25.rds')
poll <- readRDS('Source_Classification_Code.rds')
mrg <- merge(data, poll[,c('Short.Name','SSC')], by='SCC')

# filter on motor and vehicle in combination with the two areas
mc <- mrg %>% filter(grepl("motor", Short.Name, ignore.case = TRUE) & 
                grepl("vehicle", Short.Name, ignore.case = TRUE) &  
                    (fips == "24510" | fips == "06037"))

mcpy <- mc %>%
    group_by(year, fips) %>%
    summarise(sum = sum(Emissions/1000))

# open png file and plot the graph
png(filename = "plot6.png", 
    width = 480, 
    height = 480)

mcpy %>% ggplot() +
    geom_line(aes(year, sum, col=fips)) +
    labs(
        title = "Yearly PM2.5 Emissions from Motor Vehicles",
        subtitle = "Baltimore City, MD (24510) and Los Angeles County, CA (06037)",
        x = "Year",
        y = "PM2.5 emitted (in 1000 tons)"
    ) +
    scale_color_discrete(name="Fips code") + 
    xlim(1999, 2008)
dev.off()


