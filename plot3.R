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

