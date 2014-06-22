run_analysis.R
==============

```r
###############################################################################
## Script Name  : run_analysis.R                                             ##
## Author Name  : Vignesh Parameswaran                                       ##
## Date Created : 2014-06-22                                                 ##
## Version      : 1.0                                                        ##
## Description  : This script works with "UCI Human Activity Recognition     ##
##                Using Smartphones" Data Set. This code                     ##
##                1. Merges the training and the test sets to create one     ##
##                   data set                                                ##
##                2. Extracts only the measurements on the mean and standard ##
##                   deviation for each measurement                          ##
##                3. Uses descriptive activity names to name the activities  ##
##                   in the data set                                         ##
##                4. Appropriately labels the data set with descriptive      ##
##                   variable names                                          ##
##                5. Creates a second, independent tidy data set with the    ##
##                   average of each variable for each activity and each     ##
##                   subject                                                 ##
###############################################################################

run_analysis <- function() {
  
  ## Check for Packages
  source("pkgCheck.R")
  message(Sys.time(), " Checking for necessary packages")
  pkgCheck("bitops")
  pkgCheck("RCurl")
  pkgCheck("plyr")
  pkgCheck("reshape2")
  pkgCheck("data.table")
  message(Sys.time(), " Success!")
  
  ## Set Path and download files
  message(Sys.time(), " Preparing the environment")
  currentPath <- getwd()
  tidyData <- file.path(currentPath, "HARUsingSmartphones.txt")
  if(file.exists("HARUsingSmartphones.txt")) stop("HARUsingSmartphones.txt exists")
  
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  file <- "Dataset.zip"
  if (!file.exists(file)) {
    message(Sys.time(), "   Downloading file")
    download.file(url, file.path(currentPath, file), quiet = TRUE)
    message(Sys.time(), "   Download completed")
  }
  else {
    message(Sys.time(), "   File already downloaded")
  }
  
  ## Unzip the files
  if (!file.exists("UCI HAR Dataset")) {
    message(Sys.time(), "   Unzip Started")
    unzip(file)
    message(Sys.time(), "   Unzip Completed")
  }
  else {
    message(Sys.time(), "   File already unzipped")
  }
  
  ## Set 'UCI HAR Dataset' as Input path
  UCIPath <- file.path(currentPath, "UCI HAR Dataset")
  
  ## Read Subject files
  message(Sys.time(), "   Reading Subject files")
  dataSubjectTrain <- fread(file.path(UCIPath, "train", "subject_train.txt"))
  dataSubjectTest  <- fread(file.path(UCIPath, "test" , "subject_test.txt" ))
  
  ## Read Activity files
  message(Sys.time(), "   Reading Activity files")
  dataActivityTrain <- fread(file.path(UCIPath, "train", "Y_train.txt"))
  dataActivityTest  <- fread(file.path(UCIPath, "test" , "Y_test.txt" ))
  
  ## Read Data files
  message(Sys.time(), "   Reading Data files")
  dataTrain <- data.table(read.table(file.path(UCIPath, "train", "X_train.txt")))
  dataTest  <- data.table(read.table(file.path(UCIPath, "test" , "X_test.txt" )))
  message(Sys.time(), " Success!")
  
  ## Segment 1 - Merge the training and the test sets to create one data set
  ## -----------------------------------------------------------------------
  message(Sys.time(), " Merge the training and the test sets")
  
  ## Bind the data tables
  message(Sys.time(), "   Creating Data Table")
  dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
  setnames(dataSubject, "V1", "subject")
  dataActivity <- rbind(dataActivityTrain, dataActivityTest)
  setnames(dataActivity, "V1", "activityNum")
  data <- rbind(dataTrain, dataTest)
  
  ## Merge columns
  dataSubject <- cbind(dataSubject, dataActivity)
  data <- cbind(dataSubject, data)
  
  ## Set key
  setkey(data, subject, activityNum)
  message(Sys.time(), "   Data Table Created")
  message(Sys.time(), " Success!")
  
  ## Segment 2 - Extract only the measurements on the mean and standard deviation
  ## ----------------------------------------------------------------------------
  message(Sys.time(), " Extract the mean and standard deviation")
  
  ## Extract only the mean and standard deviation
  message(Sys.time(), "   Extracting mean and standard deviation")
  dataFeatures <- fread(file.path(UCIPath, "features.txt"))
  setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
  
  ## Subset only measurements for the mean and standard deviation
  message(Sys.time(), "   Subseting mean and standard deviation")
  dataFeatures <- dataFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
  
  ## Convert the column numbers to a vector of variable names
  message(Sys.time(), "   Converting column numbers")
  dataFeatures$featureCode <- dataFeatures[, paste0("V", featureNum)]
  
  ## Subset variables using variable names
  message(Sys.time(), "   Subset variables using variable names")
  select <- c(key(data), dataFeatures$featureCode)
  data <- data[, select, with=FALSE]
  message(Sys.time(), " Success!")
  
  ## Segment 3 - Use descriptive activity names
  ## ------------------------------------------
  message(Sys.time(), " Use descriptive activity names")
  
  ## Read activity_labels.txt
  dataActivityNames <- fread(file.path(UCIPath, "activity_labels.txt"))
  setnames(dataActivityNames, names(dataActivityNames), c("activityNum", "activityName"))
  message(Sys.time(), " Success!")
  
  ## Segment 4 - Label with descriptive activity names
  ## -------------------------------------------------
  message(Sys.time(), " Label with descriptive activity names")
  
  ## Merge activity labels and set key
  message(Sys.time(), "   Merge activity labels and set key")
  data <- merge(data, dataActivityNames, by="activityNum", all.x=TRUE)
  setkey(data, subject, activityNum, activityName)

  ## Melt the data table to reshape it
  message(Sys.time(), "   Melt the data table")
  data <- data.table(melt(data, key(data), variable.name="featureCode"))

  ## Merge activity name
  message(Sys.time(), "   Merge activity name")
  data <- merge(data, dataFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

  ## Create `activity`  and `feature`
  message(Sys.time(), "   Create activity and feature variables")
  data$activity <- factor(data$activityName)
  data$feature <- factor(data$featureName)
  
  ## Features with 1 category
  message(Sys.time(), "   Features with 1 category")
  data$featJerk <- factor(grepl("Jerk", data$feature), labels=c(NA, "Jerk"))
  data$featMagnitude <- factor(grepl("Mag", data$feature), labels=c(NA, "Magnitude"))
  
  ## Features with 2 categories
  message(Sys.time(), "   Features with 2 categories")
  n <- 2
  y <- matrix(seq(1, n), nrow=n)
  x <- matrix(c(grepl("^t", data$feature), grepl("^f", data$feature)), ncol=nrow(y))
  data$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
  x <- matrix(c(grepl("Acc", data$feature), grepl("Gyro", data$feature)), ncol=nrow(y))
  data$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
  x <- matrix(c(grepl("BodyAcc", data$feature), grepl("GravityAcc", data$feature)), ncol=nrow(y))
  data$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
  x <- matrix(c(grepl("mean()", data$feature), grepl("std()", data$feature)), ncol=nrow(y))
  data$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
  
  ## Features with 3 categories
  message(Sys.time(), "   Features with 3 categories")
  n <- 3
  y <- matrix(seq(1, n), nrow=n)
  x <- matrix(c(grepl("-X", data$feature), grepl("-Y", data$feature), grepl("-Z", data$feature)), ncol=nrow(y))
  data$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))
  
  ## Check to make sure all possible combinations of `feature` are accounted for by all possible combinations of the factor class variables.
  message(Sys.time(), "   Confirm Features")
  r1 <- nrow(data[, .N, by=c("feature")])
  r2 <- nrow(data[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
  r1 == r2

  message(Sys.time(), " Success!")
  
  ## Segment 5 - Create a tidy data set
  ## ----------------------------------
  message(Sys.time(), " Create a tidy data set")
  
  setkey(data, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
  dataTidy <- data[, list(count = .N, average = mean(value)), by=key(data)]
  write.table(dataTidy, tidyData, quote=FALSE, sep="\t", row.names=FALSE)
  message(Sys.time(), "   Tidy data set created")
  message(Sys.time(), " Success!")
}
```
