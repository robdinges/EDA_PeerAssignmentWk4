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

