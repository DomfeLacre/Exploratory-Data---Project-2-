
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


##table contains only the year and emission totals (4 rows and 2 columns)
Emm24510 <- NEI %>% 
          filter(fips == "24510") %>%
          group_by(year) %>% 
          summarise(Total_Emm = sum(Emissions))

###or

subsetNEI <- NEI[NEI$fips == "24510",]

Emm1 <- aggregate(Emissions ~ year, subsetNEI, sum)





# Graph the data ----------------------------------------------------------


png('plot2.png')
barplot(Emm24510$Total_Emm, Emm24510$year, 
        xlab = "Years", ylab = "Total Emissions",
        main = "Baltimore over the Years")
dev.off()




