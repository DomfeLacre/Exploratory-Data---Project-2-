

setwd("~/R/Exploratory Data - week 4 project")


library(dplyr)
library(ggplot2)
library(data.table)


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


subset24510 <- NEI[fips == "24510",]








# Graph the data ----------------------------------------------------------


png('plot3.png')

ggplot(subset24510, aes(factor(year), Emissions, fill = type)) +
        geom_bar(stat = "identity") + 
        theme_bw() + guides(fill=FALSE)+
        facet_grid(.~type,scales = "free",space="free") + 
        labs(x="year", y=expression("Total PM"[2.5]*" Emissions")) + 
        labs(title=expression("Emissions, Baltimore City, MD 24510 (1999-2008)"))



dev.off()
