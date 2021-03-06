# CodeBook
MNTB  

For this project, the training and test sets were merged to create one dataset. he subjects (numbered 1-30) and their associated activies (numbered 1-6) were appended to this dataset.

 A new smaller subset of this data was created, selecting only values that were means (mean()), or standard deviations (std()).  The whole dataset was summarized to obtain the mean values for all observations.
 
The numbered activities (1-6), were relabelled with their descriptive titles:

+ 1=WALKING
+ 2=WALKING UPSTAIRS
+ 3=WALKING DOWNSTAIRS
+ 4=SITTING
+ 5=STANDING
+ 6=LAYING

And finally, the variable names were made more descriptive:

This is a stepwise process, where, based on the "features_info.txt" from the original data, "t" was replaced with Average Mean Timed, "f" with Average Mean Fourier, "x, Y, or Z" with X-axis, etc., and "std" with SD.  So wherease the original variable name might be "tGravityAcc-std()-X" the new descriptive title is "Average Mean Timed GravityAcc SD X-axis"

The final dataset is by definition tidy, with one variable per column. To confirm this, use the read.table instruction in the ReadMe file (#24) to get a better view of the data.

