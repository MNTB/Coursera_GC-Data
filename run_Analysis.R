##Getting and Cleaning Data: Course Project

library("dplyr",lib.loc="~/R/win-library/3.1")
library("tidyr",lib.loc="~/R/win-library/3.1")

#1. Create a folder for raw data
if(!file.exists("./RawData")){dir.create("./RawData")}

#2. Download zipped file (if it doesn't exist already), change name
 

fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                        
if(!file.exists("./ProjectData.zip")){download.file(fileUrl, destfile = "./ProjectData.zip")}

#3. unzip the zipped file into the Raw Data folder and reset Raw Data folder as working directory

if(!exists("rawData")){rawData<-unzip("./ProjectData.zip", exdir="./RawData")}

setwd("./RawData")

#4. use list.files() to see the directory, and reset if necessary

list.files()
setwd("./UCI HAR Dataset")

#5. set working directory to test

setwd("./test")

#6. read in the test files

testTable<-read.table("X_test.txt")

#7. go up a directory, then set the working directory to train

setwd("../")
setwd("./train")

#8. read in the train files

trainTable<-read.table("X_train.txt")


#9 Need to name variables;Go back up one directory to get the "features.txt" file with the activity codes in it. 

setwd("../")
names<-readLines("features.txt")

colnames(trainTable)<-names
colnames(testTable)<-names


#10. #need to append new variables for subject and activity.  Read in text data with new column names

setwd("./train")
tblsub<-read.table("subject_train.txt", col.names = "Subject")
tblact<-read.table("y_train.txt", col.names = "Activity")

setwd("../")
setwd("./test")

tblsub2<-read.table("subject_test.txt", col.names = "Subject")
tblact2<-read.table("y_test.txt", col.names = "Activity")

#11. apppend new variables to each respective data frame


trainTable[,562]<-tblsub
trainTable[,563]<-tblact
testTable[,562]<-tblsub2
testTable[,563]<-tblact2


#13.create a new long dataset with both test and train data

bigTable<-rbind(testTable, trainTable)

#14.subset merged dataset by column titles containing mean() or sd()

bT2<-bigTable[, grep("mean()", colnames(bigTable))]
bT3<-bigTable[, grep("std()", colnames(bigTable))]

#15. append "Subject" and "Activity" columns again

bT4<-as.data.frame(bigTable$Subject)
bT5<-as.data.frame(bigTable$Activity)

#15A. rename last two columns
colnames(bT4)<-"Subject"
colnames(bT5)<-"Activity"

#16. create new dataframe with bT2 to bT5
data<-cbind(bT2, bT3, bT4, bT5)

#17.make a for loop, to get means, grouped first by subject, and then activity

for (i in 1:30){
        Subs<-subset(data, subset =(Subject == i))
        meanByAct<-aggregate(x= Subs, by = list(Subs$Activity), FUN = mean)
        if(i==1){finalframe<<-meanByAct}
        else{finalframe<-rbind(finalframe,meanByAct)}
}

head(finalframe,10)

#18. Make a for loop, to change the Activity numbers to their associated descriptions

for(i in 1:nrow(finalframe)){
        if(finalframe[i,82]==1){finalframe[i,82]<-"WALKING"}
        else if(finalframe[i,82]==2){finalframe[i,82]<-"WALKING_UPSTAIRS"}
        else if(finalframe[i,82]==3){finalframe[i,82]<-"WALKING_DOWNSTAIRS"}
        else if(finalframe[i,82]==4){finalframe[i,82]<-"SITTING"}
        else if(finalframe[i,82]==5){finalframe[i,82]<-"STANDING"}
        else {finalframe[i,82]<-"LAYING"}
                      
}

head(finalframe,10)

#19.Aggregating data seems to have added an extra column.  Remove.

tidydata<-finalframe[,-(1)]


#20.create a new name vector
Titles<-names(tidydata)

#21.use gsub to rename columns more "descriptively"
##21a. first replace X, Y, Z with X-axis, etc.
TitlesX<-gsub("()-X", "X-axis", Titles)
TitlesY<-gsub("()-Y", "Y-axis", TitlesX)
TitlesZ<-gsub("()-Z", "Z-axis", TitlesY)

#21b. remove brackets, change std to SD
Titles2<-gsub("-std()", " SD", TitlesZ)
Titles2a<-gsub("\\()"," ",Titles2)

#21c. replace 't' and 'f' with more descriptive titles, and remove digits.
Titles3<-gsub(" t", " Average Mean Timed ", Titles2a)
Titles4<-gsub("-mean()", "", Titles3)
Titles5<-gsub(" f", " Average Mean Fourier ", Titles4)
Titles6<-gsub("[[:digit:]]", "", Titles5)

#22. Finally, replace column names with new Titles6 vector.
colnames(tidydata)<-Titles6

#23.Write data to text file with write.table
write.table(tidydata, file = "tidydata.txt", row.name = FALSE)


