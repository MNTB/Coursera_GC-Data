# ReadMe
MNTB  
###Getting and Cleaning Data: Course Project

####Here are the steps that were taken in the script to analyze the data and create the final data set


1. Create a folder for raw data

2. Download zipped file (if it doesn't exist already), change name
 
3. unzip the zipped file into a new Raw Data folder (if it doesn't already exist, this step will creat it) and reset Raw Data folder as working directory

4. use list.files() to see the directory, and reset to UCI HAR Dataset if necessary

5. set working directory to test

6. read in the test files using read.table, assign to a variable

7. go up a directory, then set the working directory to the 'train' folder

8. read in the train files using read.table, assign to a variable

9 Need to name activity variables;Go back up one directory to get the "features.txt" file with the activity codes in it. 

10. need to append new variables for subject and activity.  Read in text data with new column names (Subject, Activity)

11. apppend new variables to each respective data frame

13.create a new long dataset with both test and train data using rbind

14.subset merged dataset by column titles containing mean() or sd() - with grep


15. append "Subject" and "Activity" columns again to each subset

   - 15a. rename last two columns

16. create new dataframe with subsets created in step #14.

17.make a for loop with if statements, to get means, grouped first by subject, and then activity

18. Make a for loop with if statements, to change the Activity numbers to their associated descriptions

   - First I looked at the file "features.txt" in step #9.

   - in this step I'm assigning the descriptive activity names (e.g. WALKING, STANDING, etc.)

19.Aggregating data seems to have added an extra column.  Remove.

20.create a new name vector using colnames function

21.use gsub to rename columns more "descriptively".  This is a stepwise process, where, based on the README from the original date, I am replacing "t" with Timed, "f" with Fourier, "mean" with Averaged, "x, Y, or Z" with X-axis, etc., and "std" with SD.  So wherease the original variable name might be "tGravityAcc-std()-X" the new descriptive title is "Average Timed GravityAcc SD X-axis""

  - 21a. first replace X, Y, Z with X-axis, etc.

  - 21b. remove brackets, change std to SD

  - 21c. replace 't' and 'f' with more descriptive titles, and remove digits.

22. Finally, replace column names with new final name vector.

23. Write data to text file with write.table

24. To view data in R, use: variable.name<-read.table(tidydata, header = TRUE). 


