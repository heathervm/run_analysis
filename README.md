# run_analysis
##for Getting and Cleaning Data Course Project
run_analysis works by first reading the train data and test data sets into R. 
Next it adds in the columns with the activity codes and participant IDs. 
The script then substitutes the activity codes with human readable activity names. 

* sitting = sitting
* laying = laying down 
* walking = walking
* up = walking upstairs
* down = walking downstairs

The data is subsetted to find columns with "-mean" or "-median" in the name. 
rbind is used to add the train and test datasets together to create one dataset. 
The means for each subject and activity are individually calculated using dplyr
the means for each subject are assembled in a dataframe

##variables associated with this dataset are as follows: 
The participant numbers are encoded as follows: 
* one = 1
* two = 2
* three = 3
* four = 4
* five = 6
* six = 6
* sep = 7
* oct = 8
* non = 9
* ten = 10 
* oneone = 11
* onetwo = 12
* onethree = 13
* onefour = 14
* onefive = 15
* onesix = 16
* onesep = 7
* oneoct = 18 
* onenon = 19
* twozero = 20
* twoone = 21
* twotwo = 22
* twothree = 23
* twofour = 24
* twofive = 25
* twosix = 26
* twosep = 27
* twooct = 28
* twonon = 29
* threezero = 30
