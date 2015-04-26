library(RCurl)
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
f <- file.path('data', 'FUCI_HAR_dataset.zip')
download.file(url, f, method = 'curl')

library(RCurl)
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv'
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
f <- file.path('data', 'GDP.csv')
download.file(url, f, method = 'curl')

library(data.table)
dtGDP <- data.table(read.csv('data/GDP.csv', skip = 4, stringsAsFactors = FALSE))
dtGDP <- dtGDP[X != ""]
setnames(dtGDP, c("X", "X.1", "X.3", "X.4"), c("CountryCode", "rankingGDP", "Long.Name", "gdp"))
gdp <- as.numeric(gsub(",","",dtGDP$gdp))

url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
f <- file.path('data', 'FEDSTATS.csv')
download.file(url, f, method = 'curl')

dtFED <- data.table(read.csv(f, stringsAsFactors = FALSE))
dt <- merge(dtGDP, dtFED, all = TRUE, by = c("CountryCode"))
isFiscalYearEnd <- grepl("fiscal year end", tolower(dt$Special.Notes))
isJune <- grepl("june", tolower(dt$Special.Notes))
table(isFiscalYearEnd, isJune)

require(quantmod)
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

addmargins(table(year(sampleTimes), weekdays(sampleTimes)))

url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
f <- file.path('data', 'NEI.zip')
download.file(url, f, method = 'curl')

NEI <- readRDS('data/summarySCC_PM25.rds')
SCC <- readRDS('data/Source_Classification_Code.rds')
emissionAgg <- aggregate(Emissions ~ year,NEI, sum)

BaltimoreNEI <- NEI[NEI$fips == 24510,]
emissionAgg <- aggregate(Emissions ~ year,BatimoreNEI, sum)
barplot((emissionAgg$Emissions),
        names.arg = emissionAgg$year, 
        xlab = "Year", 
        ylab = "PM2.5 Emissions (Tons)", 
        main = "Total PM2.5 Emissions from Baltimore City")


library(ggplot2)

BaltimoreNEI <- NEI[NEI$fips == 24510,]
emissionAgg <- aggregate(Emissions ~ year,BatimoreNEI, sum)

ggp <- ggplot(BaltimoreNEI,aes(factor(year),Emissions,fill=type)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))

print(ggp)

# Subset coal combustion related NEI data
combustionRelated <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coalRelated <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
coalCombustion <- (combustionRelated & coalRelated)
combustionSCC <- SCC[coalCombustion,]$SCC
combustionNEI <- NEI[NEI$SCC %in% combustionSCC,]

barplot((emissionAgg$Emissions)/10^5,
        names.arg = emissionAgg$year, 
        xlab = "Year", 
        ylab = "PM2.5 Emissions (10^5 Tons)", 
        main = "PM2.5 Emissions from Coal Combustion in US")

# subset for motor vehicles
vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]

# get for Baltimore city
baltimoreVehiclesNEI <- vehiclesNEI[vehiclesNEI$fips==24510,]
emissionAgg <- aggregate(Emissions ~ year,baltimoreVehiclesNEI, sum)

barplot((emissionAgg$Emissions),
        names.arg = emissionAgg$year, 
        xlab = "Year", 
        ylab = "PM2.5 Emissions (Tons)", 
        main = "PM2.5 Emissions from Vehicle Emission in Baltimore City")


library(ggplot2)
ggp <- ggplot(baltimoreVehiclesNEI,aes(factor(year),Emissions)) +
  geom_bar(stat="identity",fill="grey",width=0.75) +
  theme_bw() +  guides(fill=FALSE) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))

print(ggp)

vehiclesBaltimoreNEI <- vehiclesNEI[vehiclesNEI$fips == 24510,]
vehiclesBaltimoreNEI$city <- "Baltimore City"
vehiclesLANEI <- vehiclesNEI[vehiclesNEI$fips=="06037",]
vehiclesLANEI$city <- "Los Angeles County"
bothNEI <- rbind(vehiclesBaltimoreNEI,vehiclesLANEI)

library(ggplot2)

ggp <- ggplot(bothNEI, aes(x=factor(year), y=Emissions, fill=city)) +
  geom_bar(aes(fill=year),stat="identity") +
  facet_grid(scales="free", space="free", .~city) +
  guides(fill=FALSE) + theme_bw() +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))

print(ggp)