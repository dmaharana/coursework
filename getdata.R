library(RCurl)
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
f <- file.path('data', 'FUCI_HAR_dataset.zip')
download.file(url, f, method = 'curl')