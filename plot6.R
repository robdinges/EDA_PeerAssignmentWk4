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


