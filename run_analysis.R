#STEP 1
#You need to download the data that is in a zipped folder

#STEP 2
#Unzip the folder

#STEP 3
#Put the UCI HAR Dataset folder
#In your R working directory

#STEP 4
#You need to install the data.table package.
#If you do not have use
#install.packages("data.table")

#STEP 5 
#Run the following R program

library(data.table)

subject_testDATAFRAME  <-  read.table('./UCI HAR Dataset/test/subject_test.txt',   header = FALSE, sep = ' ')
y_testDATAFRAME        <-  read.table('./UCI HAR Dataset/test/y_test.txt'      ,   header = FALSE, sep = ' ')
X_testDATAFRAME        <-  read.table('./UCI HAR Dataset/test/X_test.txt'      ,   header = FALSE, sep = '' )

#We combine the previews dataframes in one.
testDATAFRAME <-  data.frame(subject_testDATAFRAME, y_testDATAFRAME, X_testDATAFRAME)

subject_trainDATAFRAME <-  read.table('./UCI HAR Dataset/train/subject_train.txt', header = FALSE, sep = ' ')
y_trainDATAFRAME       <-  read.table('./UCI HAR Dataset/train/y_train.txt'      , header = FALSE, sep = ' ')
X_trainDATAFRAME       <-  read.table('./UCI HAR Dataset/train/X_train.txt'      , header = FALSE, sep = '' )

#We combine the previews dataframes in one.
trainDATAFRAME         <-  data.frame(subject_trainDATAFRAME, y_trainDATAFRAME, X_trainDATAFRAME)

#STEP 6
#We combine the testDATAFRAME and the trainDATAFRAME 

completeDATAFRAME <- rbind( testDATAFRAME, trainDATAFRAME)

featuresDATAFRAME     <-  read.table('./UCI HAR Dataset/features.txt'          , header = FALSE, sep = ' ')
featuresDATAFRAME[,2] <-  as.character(featuresDATAFRAME[,2])

#STEP 7 
#We change the names of the columns of the completeDATAFRAME
names(completeDATAFRAME) <- c('subject', 'activities', featuresDATAFRAME[,2])
View(completeDATAFRAME)


#STEP 8
# We reduce the dataframe 
# Using only the COLUMNS that are related to
# means or standard deviations
# To do that we find the names of the columns that
# have the substrings
# mean and std
# Finally we created a new dataframe only with those columns
meanCOLUMNS <- grep('mean', names(completeDATAFRAME))
standarddeviationCOLUMNS  <- grep('std',  names(completeDATAFRAME))
reducedDATAFRAME <- completeDATAFRAME[,c(1, 2, meanCOLUMNS, standarddeviationCOLUMNS)]


#STEP 9
# We change the rows of the activities column
# reducedDATAFRAME has the numbers 1, 2, 3, 4, 5, etc
# We will change those numbers by the names of the activities
# The names of the activies are in the file
# activity_labels.txt 
newDATAFRAME <- reducedDATAFRAME
View(newDATAFRAME)
activityDATAFRAME <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
View(activityDATAFRAME)
activityNAMES <- as.character(activityDATAFRAME[,2])
View(activityNAMES)

#This is a very important trick
# of the R PROGRAMMING LANGUAGE
#It is faster than others programming languages
#Instead of looping through the names
#We use the following trick:

reducedDATAFRAME[,2] <- activityNAMES[reducedDATAFRAME[,2]]
View(reducedDATAFRAME)

#STEP 10
# We need to change the names of the columns of our dataframe named reducedDATAFRAMES
# using the information that is 
# in the file
# features_info.txt
# the complete location is './UCI HAR Dataset/features_info.txt'
#
#
# We need to use the gsub() function to make changes in the names of the columns
# gsub() changes substrings in a string
# basically works like 
# replace "a" by "b" in the string called "c"

oldCOLUMNSnames <- names(reducedDATAFRAME)
View(oldCOLUMNSnames)

newCOLUMNSnames <- oldCOLUMNSnames
newCOLUMNSnames <- gsub("[(]", "", newCOLUMNSnames)
newCOLUMNSnames <- gsub("[)]", "", newCOLUMNSnames)
newCOLUMNSnames <- gsub("^f", "Frequency", newCOLUMNSnames)
newCOLUMNSnames <- gsub("^t", "Time", newCOLUMNSnames)
newCOLUMNSnames <- gsub("Acc", "Accelerometer", newCOLUMNSnames)
newCOLUMNSnames <- gsub("BodyBody", "Body", newCOLUMNSnames)
newCOLUMNSnames <- gsub("Gyro", "Gyroscope", newCOLUMNSnames)
newCOLUMNSnames <- gsub("Mag", "Magnitude", newCOLUMNSnames)
newCOLUMNSnames <- gsub("meanFreq", "mean_frequency", newCOLUMNSnames)
newCOLUMNSnames <- gsub("std", "standard_deviation", newCOLUMNSnames)
View(newCOLUMNSnames)

names(reducedDATAFRAME) <- newCOLUMNSnames
View(reducedDATAFRAME)

# STEP 11
# We create a new dataframe
# Finding the mean of the numeric variables
# We need to use the aggregate function
# After that we create a file with that dataframe
# Using the write.table function

dataBYsubjectDATAFRAME <- aggregate(reducedDATAFRAME[,3:81], 
by = list(activities = reducedDATAFRAME$activities, subject = reducedDATAFRAME$subject),
FUN = mean, 
na.rm = TRUE)
View(dataBYsubjectDATAFRAME)

write.table(x = dataBYsubjectDATAFRAME, 
file = "dataBYsubject.txt", 
row.names = FALSE)

#STEP 12
#We repeat the previous step
#But now we will not classify by subject
#We will combine all the data

summaryDATAFRAME <- aggregate(reducedDATAFRAME[,3:81], 
by = list(activities = reducedDATAFRAME$activities),
FUN = mean, 
na.rm = TRUE)
View(summaryDATAFRAME)

write.table(x = summaryDATAFRAME, 
file = "summaryDATAFRAME.txt", 
row.names = FALSE)
