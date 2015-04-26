######################################################################################
# load required libraries
######################################################################################
library(data.table)
library(reshape2)

dataPath <- file.path('data', 'UCI HAR Dataset')

######################################################################################
# read subject files
######################################################################################
dtSubjectTrain <- fread(file.path(dataPath, "train", "subject_train.txt"))
dtSubjectTest  <- fread(file.path(dataPath, "test" , "subject_test.txt" ))

######################################################################################
# read label or activity files
######################################################################################
dtActivityTrain <- fread(file.path(dataPath, "train", "y_train.txt"))
dtActivityTest  <- fread(file.path(dataPath, "test" , "y_test.txt" ))

######################################################################################
# read data files
######################################################################################
dtTrain <- data.table(read.table(file.path(dataPath, "train", "X_train.txt")))
dtTest <- data.table(read.table(file.path(dataPath, "test", "X_test.txt")))

######################################################################################
# merge data tables
######################################################################################
dtSubject <- rbind(dtSubjectTest, dtSubjectTrain)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

######################################################################################
# merge coloumns
######################################################################################
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

######################################################################################
# set key
######################################################################################
setkey(dt, subject, activityNum)

######################################################################################
# extract only the mean and standard deviation
######################################################################################
dtFeatures <- fread(file.path(dataPath, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

######################################################################################
# match the column names with that of dt
######################################################################################
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

######################################################################################
# subset with variable names
######################################################################################
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

######################################################################################
# add descriptive names to the activities
######################################################################################
dtActivityNames <- fread(file.path(dataPath, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

######################################################################################
# merge activity labels
######################################################################################
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)

######################################################################################
# add activityName as a key
######################################################################################
setkey(dt, subject, activityNum, activityName)

######################################################################################
# melt the data table
######################################################################################
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

######################################################################################
# merge activity name
######################################################################################
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

######################################################################################
# create columns for factors
######################################################################################
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

######################################################################################
# Seperate features from featureName using the helper function grepthis.
######################################################################################

grepthis <- function (regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

######################################################################################
# Check to make sure all possible combinations of feature are accounted for by all possible combinations of the factor class variables.
######################################################################################

r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

######################################################################################
# Create a tidy data set
######################################################################################
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

######################################################################################
# Create a tidy data set
######################################################################################

write.table(dtTidy, 'data/out.txt', row.names = FALSE)

# make code book
#library(knitr)
#knit("makeCodebook.Rmd", output="codebook.md", encoding="ISO8859-1", quiet=TRUE)
#markdownToHTML("codebook.md", "codebook.html")