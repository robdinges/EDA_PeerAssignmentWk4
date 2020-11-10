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

