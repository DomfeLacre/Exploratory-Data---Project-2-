

setwd("~/R/Exploratory Data - week 4 project")


library(dplyr)
library(ggplot2)


# Read the data -----------------------------------------------------------

SCC <- readRDS(file = "Source_Classification_Code.rds")
NEI <- readRDS(file = "summarySCC_PM25.rds")

dir()

str(NEI)
summary(NEI)
dim(NEI)

table(NEI$fips)
table(NEI$SCC)
table(NEI$Pollutant)
table(NEI$Emissions)
table(NEI$type)
table(NEI$year)


str(NEI$Emissions)
str(NEI$year)

# Obtain total emmissions by year -----------------------------------------

##Individual tables containing emissions by year
Emm1999 <- NEI %>%
     filter(year == 1999) %>%
     mutate(sum(Emissions))

Emm2002 <- NEI %>%
     filter(year == 2002) %>%
     mutate(sum(Emissions))

Emm2005 <- NEI %>%
     filter(year == 2005) %>%
     mutate(sum(Emissions))

Emm2008 <- NEI %>%
     filter(year == 2008) %>%
     mutate(sum(Emissions))






# Identify four sources of data -------------------------------------------


subset24510 <- NEI[NEI$fips == "24510",]

Emm24510 <- aggregate(Emissions ~ year + type, subset24510, sum)








# Graph the data ----------------------------------------------------------



png('plot3.png')
plot3 <- ggplot(Emm1, aes(year, Emissions)) 
plot3 <- plot3 + geom_line() + 
     xlab("year") + 
     ylab(expression('Total PM'[2.5]*" Emissions")) + 
     ggtitle('Emissions, Baltimore City, Maryland,  24510 (1999 to 2008)')
plot(plot3)
print(plot3)
dev.off()














