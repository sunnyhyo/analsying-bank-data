library(ggplot2)
library(gridExtra)

setwd("C:/Users/HS/Documents/GitHub/analsying-bank-data")

bank<-read.csv("bank-additional-full.csv", header = TRUE)
head(bank)
names(bank)

######
#https://rstudio-pubs-static.s3.amazonaws.com/225209_df0130c5a0614790b6365676b9372c07.html
install.packages("DT");install.packages("lattice");

###
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
# Following libraries we have load for model 8 and model 9
#library(neuralnet)  # Neural Network 
#library(lars)   # For Least Angle Regression, Lasso and Forward Stagewise
#library(glmnet) # Lasso and Elastic-Net Regularized Generalized Linear Models


# Function 1: Create function to calculate percent distribution for factors
pct <- function(x){
  tbl <- table(x)
  tbl_pct <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(tbl_pct) <- c('Count','Percentage')
  kable(tbl_pct)
}



# pct(cdata$good_bad_21)

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

# Function 3: Normalize using Range

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}



#1.2  the data
cdata<-bank
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

kable(as.data.frame(colnames(cdata)))



