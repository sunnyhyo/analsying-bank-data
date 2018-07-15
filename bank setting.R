library(ggplot2)
library(gridExtra)

setwd("C:/Users/HS/Documents/GitHub/analsying-bank-data")

bank<-read.csv("bank-additional-full.csv", header = TRUE)
head(bank)
names(bank)



#####
#data set 
#traing, test set 나누기
#75% 
sam_size <- floor(0.75 * nrow(bank))

#set the see dto make your partition 
set.seed(1023)
train_bank <- sample(seq_len(nrow(bank), size = sam_size))

train <- bank[train_bank, ]
test <- bank[-train_bank, ]
test_ydel <- bank[-train_bank, -21]


######
#데이터셋 불러오기


