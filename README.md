# GetData_Proj2
Project 2 for the Getting and Cleaning Data course
---
title: "README"
author: "Torbj�rn"
date: "Sunday, January 25, 2015"
output: html_document
---
==================================================================
Torbj�rn Kraft
Getting and Cleaning Data - Project 2
==================================================================

Background from the source of the data:
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

Study Design
===================================================================
The R-script selected 3 body and gyro measures out of all 561 variables in the total souce data and calculated the means of these 3 variables for every person/subject and activity.

The provided R-script downloads the source file and unzip them into a directory tree. Reads and combines in the training and test files into one singel dataset - named:Dataset. Then 3 measurements are selected:
- BodyAccJerkMean
- BodyGyroMean
- BodyGyroJerkMean
And a tidy dataset are created and the means of all subject and activity are calulated and stored in the dataset - named:Tidydata. The Tidydataset is written out to a text file: TidyData.txt
No parameters needed to the R-script you need the computer to be connected to Internet.

Further and more detailed instructions and comments for the execution of the R-script are included in the R-script code.

For each of these measurements a mean is calculates for all subjects/persons and on all activities. In total 180 observations.

The dataset includes the following files:
=========================================

- 'README.txt'    : This file
- 'Tidydata.txt'  : Text file containing all data
- 'CodeBook.txt'  : Discribing the data in the TidyData.txt and the TidyData set in the R environment
- 'run_analysis.R': The R script that perfomes the TidyData dataset i R and exports the TidyData.txt file

==================================================================


Source:
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
www.smartlab.ws
==================================================================

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
