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
