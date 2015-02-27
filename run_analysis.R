run_analysis <- function(){  
  require(dplyr)
  traindata <- read.table("./UCI HAR Dataset/train/X_train.txt")
  testdata <- read.table("./UCI HAR Dataset/test/X_test.txt")
  newnames <- readLines("./UCI HAR Dataset/features.txt")
  variables <- c(newnames)
  colnames(traindata) <- variables
  colnames(testdata) <- variables
  submean_train <- traindata[,grep("-mean", colnames(traindata))]
  submedian_train <- traindata[,grep("-median", colnames(traindata))]
  traindata_sub <- cbind(submean_train, submedian_train)
  submean_test <- testdata[,grep("-mean", colnames(testdata))]
  submedian_test <- testdata[,grep("-median", colnames(testdata))]
  testdata_sub <- cbind(submean_test, submedian_test)
  
  a_train <- readLines("/Users/heathermurray/Downloads/UCI HAR Dataset/train/y_train.txt")
  a_train <- c(a_train)
  b_train <- gsub("1", "walking", a_train)
  c_train <- gsub("2", "walking_upstairs", b_train)
  d_train <- gsub("3", "walking_downstairs", c_train)
  e_train <- gsub("4", "sitting", d_train)
  f_train <- gsub("5", "standing", e_train)
  g_train <- gsub("6", "laying", f_train)   
  traindata_sub$activity <- g_train
  
  a_test <- readLines("/Users/heathermurray/Downloads/UCI HAR Dataset/test/y_test.txt")
  a_test <- c(a_test)
  b_test <- gsub("1", "walking", a_test)
  c_test <- gsub("2", "walking_upstairs", b_test)
  d_test <- gsub("3", "walking_downstairs", c_test)
  e_test <- gsub("4", "sitting", d_test)
  f_test <- gsub("5", "standing", e_test)
  g_test <- gsub("6", "laying", f_test)   
  testdata_sub$activity <- g_test
  
  participantstrain <- readLines("/Users/heathermurray/Downloads/UCI HAR Dataset/train/subject_train.txt")
  traindata_sub$participants <- participantstrain
  ##return(traindata_sub)
  
  participantstest <- readLines("/Users/heathermurray/Downloads/UCI HAR Dataset/test/subject_test.txt")
  testdata_sub$participants <- participantstest
  ##return(testdata_sub)
  
  mergedset <- rbind(traindata_sub, testdata_sub)
  
  one_sitting <-  filter(mergedset, activity == "sitting" , participants == "1")
  cols_one_sitting <- select(one_sitting, -(activity:participants))
  means_one_sitting <- colMeans(cols_one_sitting)
  
  
  one_laying <-  filter(mergedset, activity == "laying", participants == "1")
  cols_one_laying <- select(one_laying, -(activity:participants))
  means_one_laying <- colMeans(cols_one_laying)
  
  one_standing <- filter(mergedset, activity == "standing", participants == "1")
  cols_one_standing <- select(one_standing, -(activity:participants))
  means_one_standing <- colMeans(cols_one_standing)
  
  one_walking <- filter(mergedset, activity == "walking", participants == "1")
  cols_one_walking <- select(one_walking, -(activity:participants))
  means_one_walking <- colMeans(cols_one_walking)
  
  one_up <- filter(mergedset, activity == "walking_upstairs", participants == "1")
  cols_one_up <- select(one_up, -(activity:participants))
  means_one_up <- colMeans(cols_one_up)
  
  one_down <- filter(mergedset, activity == "walking_downstairs", participants == "1")
  cols_one_down <- select(one_down, -(activity:participants))
  means_one_down <- colMeans(cols_one_down)
 
  means_one <- rbind(means_one_walking, means_one_standing, means_one_sitting, means_one_laying, means_one_up, means_one_down)

  
  two_sitting <-  filter(mergedset, activity == "sitting", participants == "2")
  cols_two_sitting <- select(two_sitting, -(activity:participants))
  means_two_sitting <- colMeans(cols_two_sitting)
  
  two_laying <-  filter(mergedset, activity == "laying", participants == "2")
  cols_two_laying <- select(two_laying, -(activity:participants))
  means_two_laying <- colMeans(cols_two_laying)
  
  two_standing <- filter(mergedset, activity == "standing", participants == "2")
  cols_two_standing <- select(two_standing, -(activity:participants))
  means_two_standing <- colMeans(cols_two_standing)
  
  two_walking <- filter(mergedset, activity == "walking", participants == "2")
  cols_two_walking <- select(two_walking, -(activity:participants))
  means_two_walking <- colMeans(cols_two_walking)
  
  two_up <- filter(mergedset, activity == "walking_upstairs", participants == "2")
  cols_two_up <- select(two_up, -(activity:participants))
  means_two_up <- colMeans(cols_two_up)
  
  two_down <- filter(mergedset, activity == "walking_downstairs", participants == "2")
  cols_two_down <- select(two_down, -(activity:participants))
  means_two_down <- colMeans(cols_two_down)
  
  means_two <- rbind(means_two_walking, means_two_standing, means_two_sitting, means_two_laying, means_two_up, means_two_down)

  
  three_sitting <-  filter(mergedset, activity == "sitting", participants == "3")
  cols_three_sitting <- select(three_sitting, -(activity:participants))
  means_three_sitting <- colMeans(cols_three_sitting)
  
  three_laying <-  filter(mergedset, activity == "laying", participants == "3")
  cols_three_laying <- select(three_laying, -(activity:participants))
  means_three_laying <- colMeans(cols_three_laying)
  
  
  three_standing <- filter(mergedset, activity == "standing", participants == "3")
  cols_three_standing <- select(three_standing, -(activity:participants))
  means_three_standing <- colMeans(cols_three_standing)
  
  
  three_walking <- filter(mergedset, activity == "walking", participants == "3")
  cols_three_walking <- select(three_walking, -(activity:participants))
  means_three_walking <- colMeans(cols_three_walking)
  
  three_up <- filter(mergedset, activity == "walking_upstairs", participants == "3")
  cols_three_up <- select(three_up, -(activity:participants))
  means_three_up <- colMeans(cols_three_up)
  
  three_down <- filter(mergedset, activity == "walking_downstairs", participants == "3")
  cols_three_down <- select(three_down, -(activity:participants))
  means_three_down <- colMeans(cols_three_down)
  means_three <- rbind(means_three_walking, means_three_standing, means_three_sitting, means_three_laying, means_three_up, means_three_down)

  four_sitting <-  filter(mergedset, activity == "sitting", participants == "4")
  cols_four_sitting <- select(four_sitting, -(activity:participants))
  means_four_sitting <- colMeans(cols_four_sitting)
  
  four_laying <-  filter(mergedset, activity == "laying", participants == "4")
  cols_four_laying <- select(four_laying, -(activity:participants))
  means_four_laying <- colMeans(cols_four_laying)
  
  four_standing <- filter(mergedset, activity == "standing", participants == "4")
  cols_four_standing <- select(four_standing, -(activity:participants))
  means_four_standing <- colMeans(cols_four_standing)
  
  four_walking <- filter(mergedset, activity == "walking", participants == "4")
  cols_four_walking <- select(four_walking, -(activity:participants))
  means_four_walking <- colMeans(cols_four_walking
                                 )
  four_up <- filter(mergedset, activity == "walking_upstairs", participants == "4")
  cols_four_up <- select(four_up, -(activity:participants))
  means_four_up <- colMeans(cols_four_up)
  
  four_down <- filter(mergedset, activity == "walking_downstairs", participants == "4")
  cols_four_down <- select(four_down, -(activity:participants))
  means_four_down <- colMeans(cols_four_down)
  means_four <- rbind(means_four_walking, means_four_standing, means_four_sitting, means_four_laying, means_four_up, means_four_down)
  
  five_sitting <-  filter(mergedset, activity == "sitting", participants == "5")
  cols_five_sitting <- select(five_sitting, -(activity:participants))
  means_five_sitting <- colMeans(cols_five_sitting)
  
  five_laying <-  filter(mergedset, activity == "laying", participants == "5")
  cols_five_laying <- select(five_laying, -(activity:participants))
  means_five_laying <- colMeans(cols_five_laying)
  
  five_standing <- filter(mergedset, activity == "standing", participants == "5")
  cols_five_standing <- select(five_standing, -(activity:participants))
  means_five_standing <- colMeans(cols_five_standing)
  
  
  five_walking <- filter(mergedset, activity == "walking", participants == "5")
  cols_five_walking <- select(five_walking, -(activity:participants))
  means_five_walking <- colMeans(cols_five_walking)
  
  five_up <- filter(mergedset, activity == "walking_upstairs", participants == "5")
  cols_five_up <- select(five_up, -(activity:participants))
  means_five_up <- colMeans(cols_five_up)
  
  five_down <- filter(mergedset, activity == "walking_downstairs", participants == "5")
  cols_five_down <- select(five_down, -(activity:participants))
  means_five_down <- colMeans(cols_five_down)
  means_five <- rbind(means_five_walking, means_five_standing, means_five_sitting, means_five_laying, means_five_up, means_five_down)
  
  six_sitting <-  filter(mergedset, activity == "sitting", participants == "6")
  cols_six_sitting <- select(six_sitting, -(activity:participants))
  means_six_sitting <- colMeans(cols_six_sitting)
  
  
  six_laying <-  filter(mergedset, activity == "laying", participants == "6")
  cols_six_laying <- select(six_laying, -(activity:participants))
  means_six_laying <- colMeans(cols_six_laying)
  
  six_standing <- filter(mergedset, activity == "standing", participants == "6")
  cols_six_standing <- select(six_standing, -(activity:participants))
  means_six_standing <- colMeans(cols_six_standing)
  
  six_walking <- filter(mergedset, activity == "walking", participants == "6")
  cols_six_walking <- select(six_walking, -(activity:participants))
  means_six_walking <- colMeans(cols_six_walking)
  
  six_up <- filter(mergedset, activity == "walking_upstairs", participants == "6")
  cols_six_up <- select(six_up, -(activity:participants))
  means_six_up <- colMeans(cols_six_up)
  
  six_down <- filter(mergedset, activity == "walking_downstairs", participants == "6")
  cols_six_down <- select(six_down, -(activity:participants))
  means_six_down <- colMeans(cols_six_down)
  means_six <- rbind(means_six_walking, means_six_standing, means_six_sitting, means_six_laying, means_six_up, means_six_down)
  
  sev_sitting <-  filter(mergedset, activity == "sitting", participants == "7")
  cols_sev_sitting <- select(sev_sitting, -(activity:participants))
  means_sev_sitting <- colMeans(cols_sev_sitting)
  
  sev_laying <-  filter(mergedset, activity == "laying", participants == "7")
  cols_sev_laying <- select(sev_laying, -(activity:participants))
  means_sev_laying <- colMeans(cols_sev_laying)
  
  sev_standing <- filter(mergedset, activity == "standing", participants == "7")
  cols_sev_standing <- select(sev_standing, -(activity:participants))
  means_sev_standing <- colMeans(cols_sev_standing)
  
  sev_walking <- filter(mergedset, activity == "walking", participants == "7")
  cols_sev_walking <- select(sev_walking, -(activity:participants))
  means_sev_walking <- colMeans(cols_sev_walking)
  
  sev_up <- filter(mergedset, activity == "walking_upstairs", participants == "7")
  cols_sev_up <- select(sev_up, -(activity:participants))
  means_sev_up <- colMeans(cols_sev_up)
  
  sev_down <- filter(mergedset, activity == "walking_downstairs", participants == "7")
  cols_sev_down <- select(sev_down, -(activity:participants))
  means_sev_down <- colMeans(cols_sev_down)
  means_sev <- rbind(means_sev_walking, means_sev_standing, means_sev_sitting, means_sev_laying, means_sev_up, means_sev_down)
  
  
  oct_sitting <-  filter(mergedset, activity == "sitting", participants == "8")
  cols_oct_sitting <- select(oct_sitting, -(activity:participants))
  means_oct_sitting <- colMeans(cols_oct_sitting)
  
  oct_laying <-  filter(mergedset, activity == "laying", participants == "8")
  cols_oct_laying <- select(oct_laying, -(activity:participants))
  means_oct_laying <- colMeans(cols_oct_laying)
  
  oct_standing <- filter(mergedset, activity == "standing", participants == "8")
  cols_oct_standing <- select(oct_standing, -(activity:participants))
  means_oct_standing <- colMeans(cols_oct_standing)
  
  oct_walking <- filter(mergedset, activity == "walking", participants == "8")
  cols_oct_walking <- select(oct_walking, -(activity:participants))
  means_oct_walking <- colMeans(cols_oct_walking)
  
  oct_up <- filter(mergedset, activity == "walking_upstairs", participants == "8")
  cols_oct_up <- select(oct_up, -(activity:participants))
  means_oct_up <- colMeans(cols_oct_up)
  
  oct_down <- filter(mergedset, activity == "walking_downstairs", participants == "8")
  cols_oct_down <- select(oct_down, -(activity:participants))
  means_oct_down <- colMeans(cols_oct_down)
  means_oct <- rbind(means_oct_walking, means_oct_standing, means_oct_sitting, means_oct_laying, means_oct_up, means_oct_down)
  
  
  non_sitting <-  filter(mergedset, activity == "sitting", participants == "9")
  cols_non_sitting <- select(non_sitting, -(activity:participants))
  means_non_sitting <- colMeans(cols_non_sitting)
  
  non_laying <-  filter(mergedset, activity == "laying", participants == "9")
  cols_non_laying <- select(non_laying, -(activity:participants))
  means_non_laying <- colMeans(cols_non_laying)
  
  non_standing <- filter(mergedset, activity == "standing", participants == "9")
  cols_non_standing <- select(non_standing, -(activity:participants))
  means_non_standing <- colMeans(cols_non_standing)
  
  non_walking <- filter(mergedset, activity == "walking", participants == "9")
  cols_non_walking <- select(non_walking, -(activity:participants))
  means_non_walking <- colMeans(cols_non_walking)
  
  non_up <- filter(mergedset, activity == "walking_upstairs", participants == "9")
  cols_non_up <- select(non_up, -(activity:participants))
  means_non_up <- colMeans(cols_non_up)
  
  non_down <- filter(mergedset, activity == "walking_downstairs", participants == "9")
  cols_non_down <- select(non_down, -(activity:participants))
  means_non_down <- colMeans(cols_non_down)
  means_non <- rbind(means_non_walking, means_non_standing, means_non_sitting, means_non_laying, means_non_up, means_non_down)
  
  
  ten_sitting <-  filter(mergedset, activity == "sitting", participants == "10")
  cols_ten_sitting <- select(ten_sitting, -(activity:participants))
  means_ten_sitting <- colMeans(cols_ten_sitting)
  
  ten_laying <-  filter(mergedset, activity == "laying", participants == "10")
  cols_ten_laying <- select(ten_laying, -(activity:participants))
  means_ten_laying <- colMeans(cols_ten_laying)
  
  ten_standing <- filter(mergedset, activity == "standing", participants == "10")
  cols_ten_standing <- select(ten_standing, -(activity:participants))
  means_ten_standing <- colMeans(cols_ten_standing)
  
  ten_walking <- filter(mergedset, activity == "walking", participants == "10")
  cols_ten_walking <- select(ten_walking, -(activity:participants))
  means_ten_walking <- colMeans(cols_ten_walking)
  
  ten_up <- filter(mergedset, activity == "walking_upstairs", participants == "10")
  cols_ten_up <- select(ten_up, -(activity:participants))
  means_ten_up <- colMeans(cols_ten_up)
  
  ten_down <- filter(mergedset, activity == "walking_downstairs", participants == "10")
  cols_ten_down <- select(ten_down, -(activity:participants))
  means_ten_down <- colMeans(cols_ten_down)
  means_ten <- rbind(means_ten_walking, means_ten_standing, means_ten_sitting, means_ten_laying, means_ten_up, means_ten_down)
  
  
  oneone_laying <-  filter(mergedset, activity == "laying", participants == "11")
  cols_oneone_laying <- select(oneone_laying, -(activity:participants))
  means_oneone_laying <- colMeans(cols_oneone_laying)
  
  oneone_standing <- filter(mergedset, activity == "standing", participants == "11")
  cols_oneone_standing <- select(oneone_standing, -(activity:participants))
  means_oneone_standing <- colMeans(cols_oneone_standing)
  
  oneone_walking <- filter(mergedset, activity == "walking", participants == "11")
  cols_oneone_walking <- select(oneone_walking, -(activity:participants))
  means_oneone_walking <- colMeans(cols_oneone_walking)
  
  oneone_sitting <-  filter(mergedset, activity == "sitting" , participants == "11")
  cols_oneone_sitting <- select(oneone_sitting, -(activity:participants))
  means_oneone_sitting <- colMeans(cols_oneone_sitting)
  
  oneone_up <- filter(mergedset, activity == "walking_upstairs", participants == "11")
  cols_oneone_up <- select(oneone_up, -(activity:participants))
  means_oneone_up <- colMeans(cols_oneone_up)
  
  oneone_down <- filter(mergedset, activity == "walking_downstairs", participants == "11")
  cols_oneone_down <- select(oneone_down, -(activity:participants))
  means_oneone_down <- colMeans(cols_oneone_down)
  
  means_oneone <- rbind(means_oneone_walking, means_oneone_standing, means_oneone_sitting, means_oneone_laying means_oneone_up, means_oneone_down)
  
  
  onetwo_sitting <-  filter(mergedset, activity == "sitting", participants == "12")
  cols_onetwo_sitting <- select(onetwo_sitting, -(activity:participants))
  means_onetwo_sitting <- colMeans(cols_onetwo_sitting)
  
  onetwo_laying <-  filter(mergedset, activity == "laying", participants == "12")
  cols_onetwo_laying <- select(onetwo_laying, -(activity:participants))
  means_onetwo_laying <- colMeans(cols_onetwo_laying)
  
  onetwo_standing <- filter(mergedset, activity == "standing", participants == "12")
  cols_onetwo_standing <- select(onetwo_standing, -(activity:participants))
  means_onetwo_standing <- colMeans(cols_onetwo_standing)
  
  onetwo_walking <- filter(mergedset, activity == "walking", participants == "12")
  cols_onetwo_walking <- select(onetwo_walking, -(activity:participants))
  means_onetwo_walking <- colMeans(cols_onetwo_walking)
  
  onetwo_up <- filter(mergedset, activity == "walking_upstairs", participants == "12")
  cols_onetwo_up <- select(onetwo_up, -(activity:participants))
  means_onetwo_up <- colMeans(cols_onetwo_up)
  
  onetwo_down <- filter(mergedset, activity == "walking_downstairs", participants == "12")
  cols_onetwo_down <- select(onetwo_down, -(activity:participants))
  means_onetwo_down <- colMeans(cols_onetwo_down)
  
  means_onetwo <- rbind(means_onetwo_walking, means_onetwo_standing, means_onetwo_sitting, means_onetwo_laying, means_onetwo_up, means_onetwo_down)
  
  
  onethree_sitting <-  filter(mergedset, activity == "sitting", participants == "13")
  cols_onethree_sitting <- select(onethree_sitting, -(activity:participants))
  means_onethree_sitting <- colMeans(cols_onethree_sitting)
  
  onethree_laying <-  filter(mergedset, activity == "laying", participants == "13")
  cols_onethree_laying <- select(onethree_laying, -(activity:participants))
  means_onethree_laying <- colMeans(cols_onethree_laying)
  
  
  onethree_standing <- filter(mergedset, activity == "standing", participants == "13")
  cols_onethree_standing <- select(onethree_standing, -(activity:participants))
  means_onethree_standing <- colMeans(cols_onethree_standing)
  
  
  onethree_walking <- filter(mergedset, activity == "walking", participants == "13")
  cols_onethree_walking <- select(onethree_walking, -(activity:participants))
  means_onethree_walking <- colMeans(cols_onethree_walking)
  
  onethree_up <- filter(mergedset, activity == "walking_upstairs", participants == "13")
  cols_onethree_up <- select(onethree_up, -(activity:participants))
  means_onethree_up <- colMeans(cols_onethree_up)
  
  onethree_down <- filter(mergedset, activity == "walking_downstairs", participants == "13")
  cols_onethree_down <- select(onethree_down, -(activity:participants))
  means_onethree_down <- colMeans(cols_onethree_down)
  means_onethree <- rbind(means_onethree_walking, means_onethree_standing, means_onethree_sitting, means_onethree_laying, means_onethree_up, means_onethree_down)
  
  onefour_sitting <-  filter(mergedset, activity == "sitting", participants == "14")
  cols_onefour_sitting <- select(onefour_sitting, -(activity:participants))
  means_onefour_sitting <- colMeans(cols_onefour_sitting)
  
  onefour_laying <-  filter(mergedset, activity == "laying", participants == "14")
  cols_onefour_laying <- select(onefour_laying, -(activity:participants))
  means_onefour_laying <- colMeans(cols_onefour_laying)
  
  onefour_standing <- filter(mergedset, activity == "standing", participants == "14")
  cols_onefour_standing <- select(onefour_standing, -(activity:participants))
  means_onefour_standing <- colMeans(cols_onefour_standing)
  
  onefour_walking <- filter(mergedset, activity == "walking", participants == "14")
  cols_onefour_walking <- select(four_walking, -(activity:participants))
  means_onefour_walking <- colMeans(cols_onefour_walking)
  
  onefour_up <- filter(mergedset, activity == "walking_upstairs", participants == "14")
  cols_onefour_up <- select(onefour_up, -(activity:participants))
  means_onefour_up <- colMeans(cols_onefour_up)
  
  onefour_down <- filter(mergedset, activity == "walking_downstairs", participants == "14")
  cols_onefour_down <- select(onefour_down, -(activity:participants))
  means_onefour_down <- colMeans(cols_onefour_down)
  means_onefour <- rbind(means_onefour_walking, means_onefour_standing, means_onefour_sitting, means_onefour_laying, means_onefour_up, means_onefour_down)
  
  onefive_sitting <-  filter(mergedset, activity == "sitting", participants == "15")
  cols_onefive_sitting <- select(onefive_sitting, -(activity:participants))
  means_onefive_sitting <- colMeans(cols_onefive_sitting)
  
  onefive_laying <-  filter(mergedset, activity == "laying", participants == "15")
  cols_onefive_laying <- select(five_laying, -(activity:participants))
  means_onefive_laying <- colMeans(cols_onefive_laying)
  
  onefive_standing <- filter(mergedset, activity == "standing", participants == "15")
  cols_onefive_standing <- select(onefive_standing, -(activity:participants))
  means_onefive_standing <- colMeans(cols_onefive_standing)
  
  
  onefive_walking <- filter(mergedset, activity == "walking", participants == "15")
  cols_onefive_walking <- select(onefive_walking, -(activity:participants))
  means_onefive_walking <- colMeans(cols_onefive_walking)
  
  onefive_up <- filter(mergedset, activity == "walking_upstairs", participants == "15")
  cols_onefive_up <- select(onefive_up, -(activity:participants))
  means_onefive_up <- colMeans(cols_onefive_up)
  
  onefive_down <- filter(mergedset, activity == "walking_downstairs", participants == "15")
  cols_onefive_down <- select(onefive_down, -(activity:participants))
  means_onefive_down <- colMeans(cols_onefive_down)
  
  means_onefive <- rbind(means_onefive_walking, means_onefive_standing, means_onefive_sitting, means_onefive_laying, means_onefive_up, means_onefive_down)
  
  onesix_sitting <-  filter(mergedset, activity == "sitting", participants == "16")
  cols_onesix_sitting <- select(onesix_sitting, -(activity:participants))
  means_onesix_sitting <- colMeans(cols_onesix_sitting)
  
  
  onesix_laying <-  filter(mergedset, activity == "laying", participants == "16")
  cols_onesix_laying <- select(onesix_laying, -(activity:participants))
  means_onesix_laying <- colMeans(cols_onesix_laying)
  
  onesix_standing <- filter(mergedset, activity == "standing", participants == "16")
  cols_onesix_standing <- select(onesix_standing, -(activity:participants))
  means_onesix_standing <- colMeans(cols_onesix_standing)
  
  onesix_walking <- filter(mergedset, activity == "walking", participants == "16")
  cols_onesix_walking <- select(onesix_walking, -(activity:participants))
  means_onesix_walking <- colMeans(cols_onesix_walking)
  
  onesix_up <- filter(mergedset, activity == "walking_upstairs", participants == "16")
  cols_onesix_up <- select(onesix_up, -(activity:participants))
  means_onesix_up <- colMeans(cols_onesix_up)
  
  onesix_down <- filter(mergedset, activity == "walking_downstairs", participants == "16")
  cols_onesix_down <- select(onesix_down, -(activity:participants))
  means_onesix_down <- colMeans(cols_onesix_down)
 
  means_onesix <- rbind(means_onesix_walking, means_onesix_standing, means_onesix_sitting, means_onesix_laying, means_onesix_up, means_onesix_down)
  
  
  onesev_sitting <-  filter(mergedset, activity == "sitting", participants == "17")
  cols_onesev_sitting <- select(onesev_sitting, -(activity:participants))
  means_onesev_sitting <- colMeans(cols_onesev_sitting)
  
  onesev_laying <-  filter(mergedset, activity == "laying", participants == "17")
  cols_onesev_laying <- select(onesev_laying, -(activity:participants))
  means_onesev_laying <- colMeans(cols_onesev_laying)
  
  onesev_standing <- filter(mergedset, activity == "standing", participants == "17")
  cols_onesev_standing <- select(onesev_standing, -(activity:participants))
  means_onesev_standing <- colMeans(cols_onesev_standing)
  
  onesev_walking <- filter(mergedset, activity == "walking", participants == "17")
  cols_onesev_walking <- select(onesev_walking, -(activity:participants))
  means_onesev_walking <- colMeans(cols_onesev_walking)
  
  onesev_up <- filter(mergedset, activity == "walking_upstairs", participants == "17")
  cols_onesev_up <- select(onesev_up, -(activity:participants))
  means_onesev_up <- colMeans(cols_onesev_up)
  
  onesev_down <- filter(mergedset, activity == "walking_downstairs", participants == "17")
  cols_onesev_down <- select(onesev_down, -(activity:participants))
  means_onesev_down <- colMeans(cols_onesev_down)
  means_onesev <- rbind(means_onesev_walking, means_onesev_standing, means_onesev_sitting, means_onesev_laying, means_onesev_up, means_onesev_down)
  
  
  oneoct_sitting <-  filter(mergedset, activity == "sitting", participants == "18")
  cols_oneoct_sitting <- select(oneoct_sitting, -(activity:participants))
  means_oneoct_sitting <- colMeans(cols_oneoct_sitting)
  
  oneoct_laying <-  filter(mergedset, activity == "laying", participants == "18")
  cols_oneoct_laying <- select(oneoct_laying, -(activity:participants))
  means_oneoct_laying <- colMeans(cols_oneoct_laying)
  
  oneoct_standing <- filter(mergedset, activity == "standing", participants == "18")
  cols_oneoct_standing <- select(oneoct_standing, -(activity:participants))
  means_oneoct_standing <- colMeans(cols_oneoct_standing)
  
  oneoct_walking <- filter(mergedset, activity == "walking", participants == "18")
  cols_oneoct_walking <- select(oneoct_walking, -(activity:participants))
  means_oneoct_walking <- colMeans(cols_oneoct_walking)
  
  oneoct_up <- filter(mergedset, activity == "walking_upstairs", participants == "18")
  cols_oneoct_up <- select(oct_up, -(activity:participants))
  means_oneoct_up <- colMeans(cols_oneoct_up)
  
  oneoct_down <- filter(mergedset, activity == "walking_downstairs", participants == "18")
  cols_oneoct_down <- select(oneoct_down, -(activity:participants))
  means_oneoct_down <- colMeans(cols_oneoct_down)
  
  means_oneoct <- rbind(means_oneoct_walking, means_onesev_standing, means_oneoct_sitting, means_oneoct_laying, means_oneoct_up, means_oneoct_down)
  
  onenon_sitting <-  filter(mergedset, activity == "sitting", participants == "19")
  cols_onenon_sitting <- select(onenon_sitting, -(activity:participants))
  means_onenon_sitting <- colMeans(cols_onenon_sitting)
  
  onenon_laying <-  filter(mergedset, activity == "laying", participants == "19")
  cols_onenon_laying <- select(non_laying, -(activity:participants))
  means_onenon_laying <- colMeans(cols_onenon_laying)
  
  onenon_standing <- filter(mergedset, activity == "standing", participants == "19")
  cols_onenon_standing <- select(onenon_standing, -(activity:participants))
  means_onenon_standing <- colMeans(cols_onenon_standing)
  
  onenon_walking <- filter(mergedset, activity == "walking", participants == "19")
  cols_onenon_walking <- select(onenon_walking, -(activity:participants))
  means_onenon_walking <- colMeans(cols_onenon_walking)
  
  onenon_up <- filter(mergedset, activity == "walking_upstairs", participants == "19")
  cols_onenon_up <- select(non_up, -(activity:participants))
  means_onenon_up <- colMeans(cols_onenon_up)
  
  onenon_down <- filter(mergedset, activity == "walking_downstairs", participants == "19")
  cols_onenon_down <- select(onenon_down, -(activity:participants))
  means_onenon_down <- colMeans(cols_onenon_down)
  means_onenon <- rbind(means_onenon_walking, means_onenon_standing, means_onenon_sitting, means_onenon_laying, means_onenon_up, means_onenon_down)
  
  twozero_sitting <-  filter(mergedset, activity == "sitting", participants == "20")
  cols_twozero_sitting <- select(twozero_sitting, -(activity:participants))
  means_twozero_sitting <- colMeans(cols_twozero_sitting)
  
  twozero_laying <-  filter(mergedset, activity == "laying", participants == "20")
  cols_twozero_laying <- select(twozero_laying, -(activity:participants))
  means_twozero_laying <- colMeans(cols_twozero_laying)
  
  twozero_standing <- filter(mergedset, activity == "standing", participants == "20")
  cols_twozero_standing <- select(twozero_standing, -(activity:participants))
  means_twozero_standing <- colMeans(cols_twozero_standing)
  
  twozero_walking <- filter(mergedset, activity == "walking", participants == "20")
  cols_twozero_walking <- select(twozero_walking, -(activity:participants))
  means_twozero_walking <- colMeans(cols_twozero_walking)
  
  twozero_up <- filter(mergedset, activity == "walking_upstairs", participants == "20")
  cols_twozero_up <- select(twozero_up, -(activity:participants))
  means_twozero_up <- colMeans(cols_twozero_up)
  
  twozero_down <- filter(mergedset, activity == "walking_downstairs", participants == "20")
  cols_twozero_down <- select(twozero_down, -(activity:participants))
  means_twozero_down <- colMeans(cols_twozero_down)
  
  means_twozero <- rbind(means_twozero_walking, means_twozero_standing, means_twozero_sitting, means_twozero_laying, means_twozero_up, means_twozero_down)
  
  twoone_sitting <-  filter(mergedset, activity == "sitting", participants == "21")
  cols_twoone_sitting <- select(two_sitting, -(activity:participants))
  means_twoone_sitting <- colMeans(cols_twoone_sitting)
  
  twoone_laying <-  filter(mergedset, activity == "laying", participants == "21")
  cols_twoone_laying <- select(twoone_laying, -(activity:participants))
  means_twoone_laying <- colMeans(cols_twoone_laying)
  
  twoone_standing <- filter(mergedset, activity == "standing", participants == "21")
  cols_twoone_standing <- select(twoone_standing, -(activity:participants))
  means_twoone_standing <- colMeans(cols_twoone_standing)
  
  twoone_walking <- filter(mergedset, activity == "walking", participants == "21")
  cols_twoone_walking <- select(twoone_walking, -(activity:participants))
  means_twoone_walking <- colMeans(cols_twoone_walking)
  
  twoone_up <- filter(mergedset, activity == "walking_upstairs", participants == "21")
  cols_twoone_up <- select(twoone_up, -(activity:participants))
  means_twoone_up <- colMeans(cols_twoone_up)
  
  twoone_down <- filter(mergedset, activity == "walking_downstairs", participants == "21")
  cols_twoone_down <- select(twoone_down, -(activity:participants))
  means_twoone_down <- colMeans(cols_twoone_down)
  
  means_twoone <- rbind(means_twoone_walking, means_twoone_standing, means_twoone_sitting, means_twoone_laying, means_twoone_up, means_twoone_down)
  
  
  twotwo_sitting <-  filter(mergedset, activity == "sitting", participants == "22")
  cols_twotwo_sitting <- select(twotwo_sitting, -(activity:participants))
  means_twotwo_sitting <- colMeans(cols_twotwo_sitting)
  
  twotwo_laying <-  filter(mergedset, activity == "laying", participants == "22")
  cols_twotwo_laying <- select(twotwo_laying, -(activity:participants))
  means_twotwo_laying <- colMeans(cols_twotwo_laying)
  
  twotwo_standing <- filter(mergedset, activity == "standing", participants == "22")
  cols_two_standing <- select(two_standing, -(activity:participants))
  means_twotwo_standing <- colMeans(cols_twotwo_standing)
  
  twotwo_walking <- filter(mergedset, activity == "walking", participants == "22")
  cols_twotwo_walking <- select(twotwo_walking, -(activity:participants))
  means_twotwo_walking <- colMeans(cols_twotwo_walking)
  
  twotwo_up <- filter(mergedset, activity == "walking_upstairs", participants == "22")
  cols_twotwo_up <- select(twotwo_up, -(activity:participants))
  means_twotwo_up <- colMeans(cols_twotwo_up)
  
  twotwo_down <- filter(mergedset, activity == "walking_downstairs", participants == "22")
  cols_twotwo_down <- select(twotwo_down, -(activity:participants))
  means_twotwo_down <- colMeans(cols_twotwo_down)
  
  means_twotwo <- rbind(means_twotwo_walking, means_twotwo_standing, means_twotwo_sitting, means_twotwo_laying, means_twotwo_up, means_twotwo_down)
  
  
  twothree_sitting <-  filter(mergedset, activity == "sitting", participants == "23")
  cols_twothree_sitting <- select(twothree_sitting, -(activity:participants))
  means_twothree_sitting <- colMeans(cols_twothree_sitting)
  
  twothree_laying <-  filter(mergedset, activity == "laying", participants == "23")
  cols_twothree_laying <- select(twothree_laying, -(activity:participants))
  means_twothree_laying <- colMeans(cols_twothree_laying)
  
  twothree_standing <- filter(mergedset, activity == "standing", participants == "23")
  cols_twothree_standing <- select(twothree_standing, -(activity:participants))
  means_twothree_standing <- colMeans(cols_twothree_standing)
  
  twothree_walking <- filter(mergedset, activity == "walking", participants == "23")
  cols_twothree_walking <- select(twothree_walking, -(activity:participants))
  means_twothree_walking <- colMeans(cols_twothree_walking)
  
  twothree_up <- filter(mergedset, activity == "walking_upstairs", participants == "23")
  cols_twothree_up <- select(twothree_up, -(activity:participants))
  means_twothree_up <- colMeans(cols_twothree_up)
  
  twothree_down <- filter(mergedset, activity == "walking_downstairs", participants == "23")
  cols_twothree_down <- select(twothree_down, -(activity:participants))
  means_twothree_down <- colMeans(cols_twothree_down)
  
  means_twothree <- rbind(means_twothree_walking, means_twothree_standing, means_twothree_sitting, means_twothree_laying, means_twothree_up, means_twothree_down)
  
  
  twofour_sitting <-  filter(mergedset, activity == "sitting", participants == "24")
  cols_twofour_sitting <- select(twofour_sitting, -(activity:participants))
  means_twofour_sitting <- colMeans(cols_twofour_sitting)
  
  twofour_laying <-  filter(mergedset, activity == "laying", participants == "24")
  cols_twofour_laying <- select(twofour_laying, -(activity:participants))
  means_twofour_laying <- colMeans(cols_twofour_laying)
  
  twofour_standing <- filter(mergedset, activity == "standing", participants == "24")
  cols_twofour_standing <- select(twofour_standing, -(activity:participants))
  means_twofour_standing <- colMeans(cols_twofour_standing)
  
  twofour_walking <- filter(mergedset, activity == "walking", participants == "24")
  cols_twofour_walking <- select(twofour_walking, -(activity:participants))
  means_twofour_walking <- colMeans(cols_twofour_walking)
  
  twofour_up <- filter(mergedset, activity == "walking_upstairs", participants == "24")
  cols_twofour_up <- select(two_up, -(activity:participants))
  means_twofour_up <- colMeans(cols_twofour_up)
  
  twofour_down <- filter(mergedset, activity == "walking_downstairs", participants == "24")
  cols_twofour_down <- select(two_down, -(activity:participants))
  means_two_down <- colMeans(cols_twofour_down)
  
  means_twofour <- rbind(means_twofour_walking, means_twofour_standing, means_twofour_sitting, means_twofour_laying, means_twofour_up, means_twofour_down)
  
  
  twofive_sitting <-  filter(mergedset, activity == "sitting", participants == "25")
  cols_twofive_sitting <- select(twofive_sitting, -(activity:participants))
  means_twofive_sitting <- colMeans(cols_twofive_sitting)
  
  twofive_laying <-  filter(mergedset, activity == "laying", participants == "25")
  cols_twofive_laying <- select(twofive_laying, -(activity:participants))
  means_twofive_laying <- colMeans(cols_twofive_laying)
  
  twofive_standing <- filter(mergedset, activity == "standing", participants == "25")
  cols_twofive_standing <- select(twofive_standing, -(activity:participants))
  means_twofive_standing <- colMeans(cols_twofive_standing)
  
  twofive_walking <- filter(mergedset, activity == "walking", participants == "25")
  cols_twofive_walking <- select(twofive_walking, -(activity:participants))
  means_twofive_walking <- colMeans(cols_twofive_walking)
  
  twofive_up <- filter(mergedset, activity == "walking_upstairs", participants == "25")
  cols_twofive_up <- select(twofive_up, -(activity:participants))
  means_twofive_up <- colMeans(cols_twofive_up)
  
  twofive_down <- filter(mergedset, activity == "walking_downstairs", participants == "25")
  cols_twofive_down <- select(twofive_down, -(activity:participants))
  means_twofive_down <- colMeans(cols_twofive_down)
  
  means_twofive <- rbind(means_twofive_walking, means_twofive_standing, means_twofive_sitting, means_twofive_laying, means_twofive_up, means_twofive_down)
  
  
  twosix_sitting <-  filter(mergedset, activity == "sitting", participants == "26")
  cols_twosix_sitting <- select(twosix_sitting, -(activity:participants))
  means_twosix_sitting <- colMeans(cols_twosix_sitting)
  
  twosix_laying <-  filter(mergedset, activity == "laying", participants == "26")
  cols_twosix_laying <- select(twosix_laying, -(activity:participants))
  means_twosix_laying <- colMeans(cols_twosix_laying)
  
  twosix_standing <- filter(mergedset, activity == "standing", participants == "26")
  cols_two_standing <- select(twosix_standing, -(activity:participants))
  means_two_standing <- colMeans(cols_twosix_standing)
  
  twosix_walking <- filter(mergedset, activity == "walking", participants == "26")
  cols_twosix_walking <- select(twosix_walking, -(activity:participants))
  means_twosix_walking <- colMeans(cols_twosix_walking)
  
  twosix_up <- filter(mergedset, activity == "walking_upstairs", participants == "26")
  cols_twosix_up <- select(twosix_up, -(activity:participants))
  means_twosix_up <- colMeans(cols_twosix_up)
  
  twosix_down <- filter(mergedset, activity == "walking_downstairs", participants == "26")
  cols_twosix_down <- select(twosix_down, -(activity:participants))
  means_twosix_down <- colMeans(cols_twosix_down)
  
  means_twosix <- rbind(means_twosix_walking, means_twosix_standing, means_twosix_sitting, means_twosix_laying, means_twosix_up, means_twosix_down)
  
  twosev_sitting <-  filter(mergedset, activity == "sitting", participants == "27")
  cols_twosev_sitting <- select(twosev_sitting, -(activity:participants))
  means_twosev_sitting <- colMeans(cols_twosev_sitting)
  
  twosev_laying <-  filter(mergedset, activity == "laying", participants == "27")
  cols_twosev_laying <- select(two_laying, -(activity:participants))
  means_twosev_laying <- colMeans(cols_twosev_laying)
  
  twosev_standing <- filter(mergedset, activity == "standing", participants == "27")
  cols_twosev_standing <- select(twosev_standing, -(activity:participants))
  means_twosev_standing <- colMeans(cols_twosev_standing)
  
  twosev_walking <- filter(mergedset, activity == "walking", participants == "27")
  cols_twosev_walking <- select(twosev_walking, -(activity:participants))
  means_twosev_walking <- colMeans(cols_twosev_walking)
  
  twosev_up <- filter(mergedset, activity == "walking_upstairs", participants == "27")
  cols_twosev_up <- select(twosev_up, -(activity:participants))
  means_twosev_up <- colMeans(cols_twosev_up)
  
  twosev_down <- filter(mergedset, activity == "walking_downstairs", participants == "27")
  cols_twosev_down <- select(twosev_down, -(activity:participants))
  means_twosev_down <- colMeans(cols_twosev_down)
  
  means_twosev <- rbind(means_twosev_walking, means_twosev_standing, means_twosev_sitting, means_twosev_laying, means_twosev_up, means_twosev_down)
  
  
  twooct_sitting <-  filter(mergedset, activity == "sitting", participants == "28")
  cols_twooct_sitting <- select(twooct_sitting, -(activity:participants))
  means_twooct_sitting <- colMeans(cols_twooct_sitting)
  
  twooct_laying <-  filter(mergedset, activity == "laying", participants == "28")
  cols_twooct_laying <- select(twooct_laying, -(activity:participants))
  means_twooct_laying <- colMeans(cols_twooct_laying)
  
  twooct_standing <- filter(mergedset, activity == "standing", participants == "28")
  cols_twooct_standing <- select(twooct_standing, -(activity:participants))
  means_twooct_standing <- colMeans(cols_twooct_standing)
  
  twooct_walking <- filter(mergedset, activity == "walking", participants == "28")
  cols_twooct_walking <- select(twooct_walking, -(activity:participants))
  means_twooct_walking <- colMeans(cols_twooct_walking)
  
  twooct_up <- filter(mergedset, activity == "walking_upstairs", participants == "28")
  cols_twooct_up <- select(twooct_up, -(activity:participants))
  means_two_up <- colMeans(cols_twooct_up)
  
  twooct_down <- filter(mergedset, activity == "walking_downstairs", participants == "28")
  cols_twooct_down <- select(twooct_down, -(activity:participants))
  means_twooct_down <- colMeans(cols_twooct_down)
  
  means_twooct <- rbind(means_twooct_walking, means_twooct_standing, means_twooct_sitting, means_twooct_laying, means_twooct_up, means_twooct_down)
  
  
  twonon_sitting <-  filter(mergedset, activity == "sitting", participants == "29")
  cols_twonon_sitting <- select(twonon_sitting, -(activity:participants))
  means_twonon_sitting <- colMeans(cols_twonon_sitting)
  
  twonon_laying <-  filter(mergedset, activity == "laying", participants == "29")
  cols_twonon_laying <- select(twonon_laying, -(activity:participants))
  means_twonon_laying <- colMeans(cols_twonon_laying)
  
  twonon_standing <- filter(mergedset, activity == "standing", participants == "29")
  cols_twonon_standing <- select(twonon_standing, -(activity:participants))
  means_twonon_standing <- colMeans(cols_twonon_standing)
  
  twonon_walking <- filter(mergedset, activity == "walking", participants == "29")
  cols_twonon_walking <- select(twonon_walking, -(activity:participants))
  means_twonon_walking <- colMeans(cols_twonon_walking)
  
  twonon_up <- filter(mergedset, activity == "walking_upstairs", participants == "29")
  cols_twonon_up <- select(twonon_up, -(activity:participants))
  means_twonon_up <- colMeans(cols_twonon_up)
  
  twonon_down <- filter(mergedset, activity == "walking_downstairs", participants == "29")
  cols_twonon_down <- select(twonon_down, -(activity:participants))
  means_twonon_down <- colMeans(cols_twonon_down)
  
  means_twonon <- rbind(means_twonon_walking, means_twonon_standing, means_twonon_sitting, means_twonon_laying, means_twonon_up, means_twonon_down)
  
  threezero_sitting <-  filter(mergedset, activity == "sitting", participants == "30")
  cols_threezero_sitting <- select(threezero_sitting, -(activity:participants))
  means_threezero_sitting <- colMeans(cols_threezero_sitting)
  
  threezero_laying <-  filter(mergedset, activity == "laying", participants == "30")
  cols_threezero_laying <- select(threezero_laying, -(activity:participants))
  means_threezero_laying <- colMeans(cols_threezero_laying)
  
  
  threezero_standing <- filter(mergedset, activity == "standing", participants == "30")
  cols_threezero_standing <- select(threezero_standing, -(activity:participants))
  means_threezero_standing <- colMeans(cols_threezero_standing)
  
  
  threezero_walking <- filter(mergedset, activity == "walking", participants == "30")
  cols_threezero_walking <- select(threezero_walking, -(activity:participants))
  means_threezero_walking <- colMeans(cols_threezero_walking)
  
  threezero_up <- filter(mergedset, activity == "walking_upstairs", participants == "30")
  cols_threezero_up <- select(threezero_up, -(activity:participants))
  means_threezero_up <- colMeans(cols_threezero_up)
  
  threezero_down <- filter(mergedset, activity == "walking_downstairs", participants == "30")
  cols_threezero_down <- select(threezero_down, -(activity:participants))
  means_threezero_down <- colMeans(cols_threezero_down)
  means_threezero <- rbind(means_threezero_walking, means_threezero_standing, means_threezero_sitting, means_threezero_laying, means_threezero_up, meanszero_three_down)
  print(means_threezero)

