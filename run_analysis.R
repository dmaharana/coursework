library(data.table)
pathIn <- file.path('data', 'UCI HAR Dataset')
dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest  <- fread(file.path(pathIn, "test" , "subject_test.txt" ))

dtActivityTrain <- fread(file.path(pathIn, "train", "y_train.txt"))
dtActivityTest  <- fread(file.path(pathIn, "test" , "y_test.txt" ))

dtTrain <- data.table(read.table(file.path(pathIn, "train", "X_train.txt")))
dtTest <- data.table(read.table(file.path(pathIn, "test", "X_test.txt")))

dtSubject <- rbind(dtSubjectTest, dtSubjectTrain)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

setkey(dt, subject, activityNum)

dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

