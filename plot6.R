

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






# Merge data, capture zipcode and identify motor vehicle data -------------------------------------------


Merged_Data <- merge(NEI, SCC, "SCC")

names(Merged_Data)



subset_On_Road_zips <- Merged_Data[(Merged_Data$fips=="24510" | Merged_Data$fips=="06037")
                               & Merged_Data$type=="ON-ROAD",  ]


On_Roads_Zips_Data <- aggregate(Emissions ~ year + fips, subset_On_Road_zips, sum)

On_Roads_Zips_Data$fips [On_Roads_Zips_Data$fips == "24510"] <- "Baltimore, Maryland"

On_Roads_Zips_Data$fips [On_Roads_Zips_Data$fips == "06037"] <- "Los Angeles, California"



##change year as a factor
On_Roads_Zips_Data$year <- as.factor(On_Roads_Zips_Data$year)





# Graph the data ----------------------------------------------------------

options(scipen=999)  # turn-off scientific notation like 1e+48
theme_set(theme_bw())  # pre-set the bw theme.


png('plot6.png')
plot6 <- ggplot(On_Roads_Zips_Data, aes(year, Emissions)) 
plot6 <- plot6 + 
     facet_grid(.~fips) +
     geom_bar(stat = "identity") + 
     xlab("Year") + 
     ylab(expression('Total PM'[2.5]*" Emissions")) + 
     ggtitle('Emissions from motor vehicle in zipcodes 24510 & 06037  (1999 to 2008)')
plot(plot6)
print(plot6)

dev.off()














