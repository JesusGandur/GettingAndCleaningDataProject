file <- "data.zip"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data_path <- "UCI HAR Dataset"
result_folder <- "results"

# PACKAGES INSTALLED
library(plyr)
if(!is.element("plyr", installed.packages()[,1])){
  print("Installing packages")
  install.packages("plyr")
}

# DATA ZIP DOWNLOADED
if(!file.exists(file)){
  
#DOWNLOAD DATA FILE
print("downloading Data")
download.file(url,file, mode = "wb")
  
}

if(!file.exists(result_folder)){
  print("Creating result folder")
  dir.create(result_folder)
} 

# READ TABLE FORM ZIP
getTable <- function (filename,cols = NULL){
  
  print(paste("Getting table:", filename))
  
  f <- unz(file, paste(data_path,filename,sep="/"))
  
  data <- data.frame()
  
  if(is.null(cols)){
    data <- read.table(f,sep="",stringsAsFactors=F)
  } else {
    data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
  }
  
  
  data
  
}

# COMPLETE DATA SET
getData <- function(type, features){
  
  print(paste("Getting data", type))
  
  subject_data <- getTable(paste(type,"/","subject_",type,".txt",sep=""),"id")
  y_data <- getTable(paste(type,"/","y_",type,".txt",sep=""),"activity")    
  x_data <- getTable(paste(type,"/","X_",type,".txt",sep=""),features$V2) 
  
  return (cbind(subject_data,y_data,x_data)) 
}

# SAVE IN RESULTS FOLDER
saveResult <- function (data,name){
  
  print(paste("Saving data", name))
  
  file <- paste(result_folder, "/", name,".csv" ,sep="")
  write.csv(data,file)
}

# FEATURES USED FOR COL NAMES
features <- getTable("features.txt")
train <- getData("train",features)
test <- getData("test",features)

# 1. Merges the training and the test sets to create one data set.
data <- rbind(train, test)
data <- arrange(data, id)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
dataset1 <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveResult(dataset1,"dataset1")

# 3 & 4. Uses descriptive activity names to name the activities in the data set & Appropriately labels the data set with descriptive variable names. 
activity_labels <- getTable("activity_labels.txt")
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
dataset2 <- ddply(dataset1, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })

# ADDING NAMES
colnames(dataset2)[-c(1:2)] <- paste(colnames(dataset2)[-c(1:2)], "_mean", sep="")
# SAVE TIDY DATA
saveResult(dataset2,"dataset2")