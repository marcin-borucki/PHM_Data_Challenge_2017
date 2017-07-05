rm(list = ls())
library(dplyr)
library(ggplot2)

train<-read.csv("training.csv",sep=",",header=TRUE)

train$file<-paste("training/Experiment-",train$ExperimentID,".csv",sep="")

train_data<-read.csv(train$file[1], sep=",", header = TRUE)

train_data$Payload<-train$Payload[1]
train_data$Speed<-train$Speed[1]
train_data$Track<-train$Track[1]
train_data$State<-train$State[1]
train_data$lp<-seq.int(nrow(train_data))
train_data$ID<-train$ExperimentID[1]


for(i in 2:nrow(train)){
  
  tmp<-read.csv(train$file[i], sep=",", header = TRUE)
  
  tmp$Payload<-train$Payload[i]
  tmp$Speed<-train$Speed[i]
  tmp$Track<-train$Track[i]
  tmp$State<-train$State[i]  
  tmp$lp<-seq.int(nrow(tmp))
  tmp$ID<-train$ExperimentID[i]
  
  train_data<-rbind(train_data,tmp)
  
} 

