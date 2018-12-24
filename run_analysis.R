setwd('C:/Users/MRDG/Documents/Rfiles/UCI HAR Dataset');

features <- read.table('./features.txt',header=FALSE);

activityLabel <- read.table('./activity_labels.txt',header=FALSE); 
colnames(activityLabel) <- c("activityName","activityType");
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); 
colnames(subjectTrain) <- "subjectName";
xTrain <- read.table('./train/x_train.txt',header=FALSE); colnames(xTrain) <- 
  features[,2];
yTrain <- read.table('./train/y_train.txt',header=FALSE); colnames(yTrain) <- 
  "activityName";

trainingSet = cbind(yTrain,subjectTrain,xTrain);
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); 
colnames(subjectTest) <- "subjectName";
xTest <- read.table('./test/x_test.txt',header=FALSE); colnames(xTest) <- 
  features[,2];
yTest <- read.table('./test/y_test.txt',header=FALSE); colnames(yTest) <- 
  "activityName";


testSet = cbind(yTest,subjectTest,xTest);
MergedDataSet = rbind(trainingSet,testSet);
columns <- colnames(MergedDataSet);


vector <- (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) &
             !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | 
             grepl("-std..",columns) & !grepl("-std()..-",columns));

MergedDataSet <- MergedDataSet[vector==TRUE];
MergedDataSet <- merge(MergedDataSet,activityLabel,by='activityName',all.x=TRUE);
MergedDataSet$activityName <-activityLabel[,2][match(MergedDataSet$activityName, activityLabel[,1])] 
columns <- colnames(MergedDataSet);

for (i in 1:length(columns)) 
{
  columns[i] <- gsub("\\()","",columns[i])
  columns[i] <- gsub("-std$","StdDev",columns[i])
  columns[i] <- gsub("-mean","Mean",columns[i])
  columns[i] <- gsub("^(t)","time",columns[i])
  columns[i] <- gsub("^(f)","freq",columns[i])
  columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
  columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
  columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
  columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
  columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
  columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
  columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
};

colnames(MergedDataSet) <- columns;
MergedDataSet <- MergedDataSet[,names(MergedDataSet) != 'activityType'];

tidyData <- aggregate(MergedDataSet[,names(MergedDataSet) 
                                    != c('activityName','subjectName')],by=list
                      (activityName=MergedDataSet$activityName,
                        subjectName=MergedDataSet$subjectName),mean);

write.table(tidyData, './TidyData.txt',row.names=FALSE,sep='\t')