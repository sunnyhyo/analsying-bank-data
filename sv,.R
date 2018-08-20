#set library
library(DT)          # For Data Tables
library(lattice)     # The lattice add-on of Trellis graphics for R
library(knitr)       # For Dynamic Report Generation in R 
library(gplots)      # Various R Programming Tools for Plotting Data
library(ggplot2)     # An Implementation of the Grammar of Graphics 
library(ClustOfVar)  # Clustering of variables 
library(ape)         # Analyses of Phylogenetics and Evolution (as.phylo) 
library(Information) # Data Exploration with Information Theory (Weight-of-Evidence and Information Value)
library(ROCR)        # Model Performance and ROC curve
library(caret)       # Classification and Regression Training -  for any machine learning algorithms
library(rpart)       # Recursive partitioning for classification, regression and survival trees
library(rpart.utils) # Tools for parsing and manipulating rpart objects, including generating machine readable rules
library(rpart.plot)  # Plot 'rpart' Models: An Enhanced Version of 'plot.rpart'
library(randomForest)# Leo Breiman and Cutler's Random Forests for Classification and Regression 
library(party)       # A computational toolbox for recursive partitioning - Conditional inference Trees
library(bnlearn)     # Bayesian Network Structure Learning, Parameter Learning and Inference
library(DAAG)        # Data Analysis and Graphics Data and Functions
library(vcd)         # Visualizing Categorical Data
library(kernlab)     # Support Vector Machine

# Function 1: Create function to calculate percent distribution for factors
pct <- function(x){
  tbl <- table(x)
  tbl_pct <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(tbl_pct) <- c('Count','Percentage')
  kable(tbl_pct)
}
#table(cdata$good_bad_21)
#kable(cdata$good_bad_21)
#pct(cdata$good_bad_21)
# Function 2: Own function to calculate IV, WOE and Eefficiency 
gbpct <- function(x, y=cdata$good_bad_21){
  mt <- as.matrix(table(as.factor(x), as.factor(y))) # x -> independent variable(vector), y->dependent variable(vector)
  Total <- mt[,1] + mt[,2]                          # Total observations
  Total_Pct <- round(Total/sum(mt)*100, 2)          # Total PCT
  Bad_pct <- round((mt[,1]/sum(mt[,1]))*100, 2)     # PCT of BAd or event or response
  Good_pct <- round((mt[,2]/sum(mt[,2]))*100, 2)   # PCT of Good or non-event
  Bad_Rate <- round((mt[,1]/(mt[,1]+mt[,2]))*100, 2) # Bad rate or response rate
  grp_score <- round((Good_pct/(Good_pct + Bad_pct))*10, 2) # score for each group
  WOE <- round(log(Good_pct/Bad_pct)*10, 2)      # Weight of Evidence for each group
  g_b_comp <- ifelse(mt[,1] == mt[,2], 0, 1)
  IV <- ifelse(g_b_comp == 0, 0, (Good_pct - Bad_pct)*(WOE/10)) # Information value for each group
  Efficiency <- abs(Good_pct - Bad_pct)/2                       # Efficiency for each group
  otb<-as.data.frame(cbind(mt, Good_pct,  Bad_pct,  Total, 
                           Total_Pct,  Bad_Rate, grp_score, 
                           WOE, IV, Efficiency ))
  otb$Names <- rownames(otb)
  rownames(otb) <- NULL
  otb[,c(12,2,1,3:11)] # return IV table
}
#mt <- as.matrix(table(as.factor(cdata$chk_ac_status_1), as.factor(cdata$good_bad_21)))
#mt
#sum(mt)
# Function 3: Normalize using Range
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


######
setwd("C:/Users/HS/Documents/GitHub/analsying-bank-data")

  
######
#1.2 The data
cdata<-read.table("german.data.txt", h=F, sep="")
# Update column Names
colnames(cdata) <- c("chk_ac_status_1",
                     "duration_month_2", "credit_history_3", "purpose_4",
                     "credit_amount_5","savings_ac_bond_6","p_employment_since_7", 
                       "instalment_pct_8", "personal_status_9","other_debtors_or_grantors_10", 
                       "present_residence_since_11","property_type_12","age_in_yrs_13",
                       "other_instalment_type_14", "housing_type_15", 
                       "number_cards_this_bank_16","job_17","no_people_liable_for_mntnance_18",
                       "telephone_19", "foreign_worker_20", 
                       "good_bad_21")
cdata[1,1]<-"A11"
#head(cdata, 100)
  
  
################
#2. data analysis and variavle creation
#2.0 Modify Variable types
cdata$duration_month_2  <- as.numeric(cdata$duration_month_2)
cdata$credit_amount_5   <-  as.numeric(cdata$credit_amount_5 )
cdata$instalment_pct_8 <-  as.numeric(cdata$instalment_pct_8)
cdata$present_residence_since_11 <-  as.numeric(cdata$present_residence_since_11)
cdata$age_in_yrs_13        <-  as.numeric(cdata$age_in_yrs_13)
cdata$number_cards_this_bank_16    <-  as.numeric(cdata$number_cards_this_bank_16)
cdata$no_people_liable_for_mntnance_18 <-  as.numeric(cdata$no_people_liable_for_mntnance_18)

#2.1 Good-Bad and Univariate Analysis:
#2.1.1 Analyse good_bad
cdata$good_bad_21<-as.factor(ifelse(cdata$good_bad_21 == 1, "Good", "Bad"))
pct(cdata$good_bad_21)

################

# require library(Information) 
cdata$good_bad_21<-as.numeric(ifelse(cdata$good_bad_21 == "Good", 0, 1))
IV <- Information::create_infotables(data=cdata, NULL, y="good_bad_21", 10)
IV$Summary$IV <- round(IV$Summary$IV*100,2)

IV$Tables

kable(IV$Summary)

cdata$good_bad_21<-as.factor(ifelse(cdata$good_bad_21 == 0, "Good", "Bad"))

var_list_1 <- IV$Summary[IV$Summary$IV > 2,] # 15 variables
cdata_reduced_1 <- cdata[, c(var_list_1$Variable,"good_bad_21")] #16 variables


#############

# Step 1: Subset quantitative and qualitative variables X.quanti and X.quali

factors <- sapply(cdata_reduced_1, is.factor)
#subset Qualitative variables 
vars_quali <- cdata_reduced_1[,factors]
#vars_quali$good_bad_21<-vars_quali$good_bad_21[drop=TRUE] # remove empty factors
str(vars_quali)


#subset Quantitative variables 
vars_quanti <- cdata_reduced_1[,!factors]
str(vars_quanti)


############subset

keep<- c(1:8,12,13,21)
cdata_reduced_2 <- cdata[,keep]
str(cdata_reduced_2)

div_part <- sort(sample(nrow(cdata_reduced_2), nrow(cdata_reduced_2)*.6))

#select training sample 
train<-cdata_reduced_2[div_part,] # 70% here
pct(train$good_bad_21)

# put remaining into test sample
test<-cdata_reduced_2[-div_part,] # rest of the 30% data goes here
pct(test$good_bad_21)


# Required library(caret)
# considering good_bad variable as strata

pct(cdata_reduced_2$good_bad_21)


div_part_1 <- createDataPartition(y = cdata_reduced_2$good_bad_21, p = 0.7, list = F)

# Training Sample
train_1 <- cdata_reduced_2[div_part_1,] # 70% here
pct(train_1$good_bad_21)

# Test Sample
test_1 <- cdata_reduced_2[-div_part_1,] # rest of the 30% data goes here
pct(test_1$good_bad_21)


save(train_1, file="train_1.csv")
save(test_1, file="test_1.csv")

# For neural network we would need contious data
# Sampling for Neural Network - It can be used for other modeling as well
#div_part_2 <- createDataPartition(y = cdatanum[,25], p = 0.7, list = F)

# Training Sample for Neural Network
#train_num <- cdatanum[div_part_2,] # 70% here

# Test Sample for Neural Network
#test_num <- cdatanum[-div_part_2,] # rest of the 30% data goes here

# Save for the future
#save(train_num, file="train_num.RData")
#save(test_num, file="test_num.RData")





###########################
##########SVM#############

library(kernlab) #for SVM
#str(train_1)
#class(train_1)
#dim(train_1)

# Basic Model
m7_1 <- ksvm(good_bad_21 ~ ., data = train_1, kernel = "vanilladot")

m7_1_pred <- predict(m7_1, test_1[,1:10], type="response")
head(m7_1_pred)


# Model accuracy:
table(m7_1_pred, test_1$good_bad_21)

#agreement
m7_1_accuracy  <- (m7_1_pred == test_1$good_bad_21)
pct(m7_1_accuracy)

# Compute at the prediction scores
m7_1_score <- predict(m7_1,test_1, type="decision")
m7_1_pred <- prediction(m7_1_score, test_1$good_bad_21)

# Plot ROC curve
m7_1_perf <- performance(m7_1_pred, measure = "tpr", x.measure = "fpr")
plot(m7_1_perf, colorize=TRUE, lwd=2, main="m7_1 SVM:Plot ROC curve - Vanilladot")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

# Plot precision/recall curve
m7_1_perf_precision <- performance(m7_1_pred, measure = "prec", x.measure = "rec")
plot(m7_1_perf_precision, main="m7_1 SVM:Plot precision/recall curve")

# Plot accuracy as function of threshold
m7_1_perf_acc <- performance(m7_1_pred, measure = "acc")
plot(m7_1_perf_acc, main="m7_1 SVM:Plot accuracy as function of threshold")


# Model Performance

#KS & AUC m7_1
m7_1_AUROC <- round(performance(m7_1_pred, measure = "auc")@y.values[[1]]*100, 2)
m7_1_KS <- round(max(attr(m7_1_perf,'y.values')[[1]]-attr(m7_1_perf,'x.values')[[1]])*100, 2)
m7_1_Gini <- (2*m7_1_AUROC - 100)
cat("AUROC: ",m7_1_AUROC,"\tKS: ", m7_1_KS, "\tGini:", m7_1_Gini, "\n")











  
  
  
  
  