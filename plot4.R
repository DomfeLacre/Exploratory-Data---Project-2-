
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






# Merge data and identify coal data -------------------------------------------


Merged_Data <- merge(NEI, SCC, "SCC")

names(Merged_Data)

coal_data  <- grepl("coal", Merged_Data$Short.Name, ignore.case=TRUE) 
subsetMerged_Data <- Merged_Data[coal_data, ] 



Merged_Data_Coal <- aggregate(Emissions ~ year, subsetMerged_Data, sum)

str(Merged_Data_Coal)

Merged_Data_Coal$year <- as.factor(Merged_Data_Coal$year)


# Graph the data ----------------------------------------------------------

options(scipen=999)  # turn-off scientific notation like 1e+48
theme_set(theme_bw())  # pre-set the bw theme.


png('plot4.png')
plot4 <- ggplot(Merged_Data_Coal, aes(year, Emissions)) 
plot4 <- plot4 + geom_bar(stat = "identity") + 
     xlab("Year") + 
     ylab(expression('Total PM'[2.5]*" Emissions")) + 
     ggtitle('Emissions from coal (1999 to 2008)')
plot(plot4)
print(plot4)

dev.off()














