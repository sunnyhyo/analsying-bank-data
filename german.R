setwd("C:/Users/HS/Documents/GitHub/analsying-bank-data")


######
#https://rstudio-pubs-static.s3.amazonaws.com/225209_df0130c5a0614790b6365676b9372c07.html
install.packages("DT");install.packages("lattice");install.packages("knitr");install.packages("gplots")
install.packages("ggplot2");install.packages("Clust0fVar");install.packages("ape");install.packages("Information")
install.packages("ROCR");install.packages("caret");install.packages("rpart");install.packages("rpart.utils")
install.packages("rpart.plot");install.packages("randomForest");install.packages("party");install.packages("bnlearn")
install.packages("DAAG");install.packages("vcd");install.packages("kernlab");install.packages("Clustofvar")
######
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


##########################
# User Defined Function
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
head(cdata)
# Read a numeric copy: Numeric data for Neural network & Lasso
cdatanum<-read.table("german.data-numeric.txt", h=F, sep="") 
cdatanum <- as.data.frame(sapply(cdatanum, as.numeric ))


##############
#1.3 Get first hand feeling of data
head(cdata)
kable(as.data.frame(colnames(cdata)))
str(cdata)
summary(cdata)

# library(knitr) # required for kable() function

# kable(head(cdata, 5))
# kable(tail(cdata, 5))

# library(DT) # Data Table
DT::datatable(cdata[1:100,]) # First 100 observations


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


op<-par(mfrow=c(1,2), new=TRUE)
plot(as.numeric(cdata$good_bad_21), ylab="Good-Bad", xlab="n", main="Good ~ Bad")
hist(as.numeric(cdata$good_bad_21), breaks=2, 
     xlab="Good(1) and Bad(2)", col="blue")

par(op)
par(mfrow=c(1,1))
#2.2 Univariate and bivariate Analysis
#2.2.0 Weight of Evidence(WOE), Information Value(IV) and Efficiency
#2.2.1 Checking account status

# Attribute 1:  (qualitative)
#-----------------------------------------------------------
# Checking account status

#          Status of existing checking account
#                A11 :      ... <    0 DM
#          A12 : 0 <= ... <  200 DM
#          A13 :      ... >= 200 DM /
#            salary assignments for at least 1 year
#                A14 : no checking account


A1 <- gbpct(cdata$chk_ac_status_1)
A1
op1<-par(mfrow=c(1,2), new=TRUE)
plot(cdata$chk_ac_status_1, cdata$good_bad_21, 
     ylab="Good-Bad", xlab="category", 
     main="Checking Account Status ~ Good-Bad ")

barplot(A1$WOE, col="brown", names.arg=c(A1$Levels), 
        main="Score:Checking Account Status",
        xlab="Category",
        ylab="WOE"
)

par(op1)

kable(A1, caption = 'Checking Account Status ~ Good-Bad')

#2.2.2 Loan Duration
# Attribute 2:  (numerical)
#-----------------------------------------------------------
# Loan Duration (Tenure) in Month

summary(cdata$duration_month_2)

op2<-par(mfrow=c(1,2))
boxplot(cdata$duration_month_2, ylab="Loan Duration(Month)", main="Boxplot:Loan Duration")

plot(cdata$duration_month_2, cdata$good_bad_21, 
     ylab="Good-Bad", xlab="Loan Duration(Month)",
     main="Loan Duration ~ Good-Bad ")

plot(as.factor(cdata$duration_month_2), cdata$good_bad_21, 
     ylab="Good-Bad", xlab="Loan Duration(Month)",
     main="Loan Duration(Before Groupping)")

# Create some groups from contious variables
cdata$duration_month_2 <-as.factor(ifelse(cdata$duration_month_2<=6,'00-06',
                                          ifelse(cdata$duration_month_2<=12,'06-12',
                                                 ifelse(cdata$duration_month_2<=24,'12-24', 
                                                        ifelse(cdata$duration_month_2<=30,'24-30',
                                                               ifelse(cdata$duration_month_2<=36,'30-36',
                                                                      ifelse(cdata$duration_month_2<=42,'36-42','42+')))))))

plot(cdata$duration_month_2, cdata$good_bad_21,
     main="Loan Duration(after grouping) ",
     xlab="Loan Duration (Month)",
     ylab="Good-Bad")

par(op2)

A2<-gbpct(cdata$duration_month_2)

barplot(A2$WOE, col="brown", names.arg=c(A2$Levels),
        main="Loan Duration",
        xlab="Duration(Months)",
        ylab="WOE"
)
kable(A2, caption = 'Loan Duration ~ Good-Bad')

#2.2.3 Credit History
# Attribute 3:  (qualitative)
#-----------------------------------------------------------
# Credit History

#         A30 : no credits taken/
#           all credits paid back duly
#               A31 : all credits at this bank paid back duly
#         A32 : existing credits paid back duly till now
#               A33 : delay in paying off in the past
#         A34 : critical account/
#           other credits existing (not at this bank)

# Combine few groups together based on WOE and bad rates
cdata$credit_history_3<-as.factor(ifelse(cdata$credit_history_3 == "A30", "01.A30",
                                         ifelse(cdata$credit_history_3 == "A31","02.A31",
                                                ifelse(cdata$credit_history_3 == "A32","03.A32.A33",
                                                       ifelse(cdata$credit_history_3 == "A33","03.A32.A33",
                                                              "04.A34")))))

op3<-par(mfrow=c(1,2))
plot(cdata$credit_history_3, cdata$good_bad_21, 
     main = "Credit History ~ Good-Bad",
     xlab = "Credit History",
     ylab = "Good-Bad")

plot(cdata$credit_history_3, cdata$good_bad_21, 
     main = "Credit History(After Groupping) ~ Good-Bad ",
     xlab = "Credit History",
     ylab = "Good-Bad")

par(op3)

A3<-gbpct(cdata$credit_history_3)

barplot(A3$WOE, col="brown", names.arg=c(A3$Levels),
        main="Credit History",
        xlab="Credit History",
        ylab="WOE"
)
kable(A3, caption = "Credit History~ Good-Bad")

#2.2.4 Purpose of the loan
# Attribute 4:  (qualitative)
#-----------------------------------------------------------
# Purpose of the loan

# 
#         A40 : car (new)
#         A41 : car (used)
#         A42 : furniture/equipment
#         A43 : radio/television
#         A44 : domestic appliances
#         A45 : repairs
#         A46 : education
#         A47 : (vacation - does not exist?)
#         A48 : retraining
#         A49 : business
#         A410 : others


A4<-gbpct(cdata$purpose_4)


op4<-par(mfrow=c(1,2))
plot(cdata$purpose_4, cdata$good_bad_21, 
     main="Purpose of Loan~ Good-Bad ",
     xlab="Purpose",
     ylab="Good-Bad")

barplot(A4$WOE, col="brown", names.arg=c(A4$Levels),
        main="Purpose of Loan",
        xlab="Category",
        ylab="WOE")

par(op4)

kable(A4, caption = "Purpose of Loan~ Good-Bad")

#2.2.5 Credit Amount
# Attribute 5:  (numerical)
#-----------------------------------------------------------
# Credit (Loan) Amount

cdata$credit_amount_5 <- as.double(cdata$credit_amount_5)
summary(cdata$credit_amount_5)

boxplot(cdata$credit_amount_5)

# Create groups based on their distribution
cdata$credit_amount_5<-as.factor(ifelse(cdata$credit_amount_5<=1400,'0-1400',
                                        ifelse(cdata$credit_amount_5<=2500,'1400-2500',
                                               ifelse(cdata$credit_amount_5<=3500,'2500-3500', 
                                                      ifelse(cdata$credit_amount_5<=4500,'3500-4500',
                                                             ifelse(cdata$credit_amount_5<=5500,'4500-5500','5500+'))))))


A5<-gbpct(cdata$credit_amount_5)



plot(cdata$credit_amount_5, cdata$good_bad_21, 
     main="Credit Ammount (After Grouping) ~ Good-Bad",
     xlab="Amount",
     ylab="Good-Bad")


barplot(A5$WOE, col="brown", names.arg=c(A5$Levels),
        main="Credit Ammount",
        xlab="Amount",
        ylab="WOE")

kable(A5, caption = "Credit Ammount ~ Good-Bad")

#2.2.6 Savings account/bonds
# Attibute 6:  (qualitative)
#-----------------------------------------------------------
# Savings account/bonds

#         A61 :          ... <  100 DM
#         A62 :   100 <= ... <  500 DM
#         A63 :   500 <= ... < 1000 DM
#         A64 :          .. >= 1000 DM
#               A65 :   unknown/ no savings account

A6<-gbpct(cdata$savings_ac_bond_6)


plot(cdata$savings_ac_bond_6, cdata$good_bad_21, 
     main="Savings account/bonds ~ Good-Bad",
     xlab="Savings/Bonds",
     ylab="Good-Bad")


barplot(A6$WOE, col="brown", names.arg=c(A6$Levels),
        main="Savings account/bonds",
        xlab="Category",
        ylab="WOE")

kable(A6, caption = "Savings account/bonds ~ Good-Bad" )

#2.2.7 Present employment since
# Attribute 7:  (qualitative)
#-----------------------------------------------------------
# Present employment since

# A71 : unemployed
# A72 :       ... < 1 year
# A73 : 1  <= ... < 4 years
# A74 : 4  <= ... < 7 years
# A75 :       .. >= 7 years

A7<-gbpct(cdata$p_employment_since_7)

op7<-par(mfrow=c(1,2))
plot(cdata$p_employment_since_7, cdata$good_bad_21,
     main="Present employment since ~ Good-Bad",
     xlab="Employment in Years",
     ylab="Good-Bad")

barplot(A7$WOE, col="brown", names.arg=c(A7$Levels),
        main="Present employment",
        xlab="Category",
        ylab="WOE")

par(op7)

kable(A7, caption ="Present employment since ~ Good-Bad")

#2.2.8 instalment rate in percentage of disposable income
# Attribute 8:  (numerical)
#-----------------------------------------------------------
# instalment rate in percentage of disposable income

summary(cdata$instalment_pct_8)

op8<-par(mfrow=c(1,2))
boxplot(cdata$instalment_pct_8)
histogram(cdata$instalment_pct_8,
          main = "instalment rate in percentage of disposable income",
          xlab = "instalment percent",
          ylab = "Percent Population")
par(op8)

A8<-gbpct(cdata$instalment_pct_8)

op8_1<-par(mfrow=c(1,2))
plot(as.factor(cdata$instalment_pct_8), cdata$good_bad_21,
     main="instalment rate in percentage of disposable income ~ Good-Bad",
     xlab="Percent",
     ylab="Good-Bad")

barplot(A8$WOE, col="brown", names.arg=c(A8$Levels),
        main="instalment rate",
        xlab="Percent",
        ylab="WOE")

par(op8_1)

kable(A8, caption = "instalment rate in percentage of disposable income ~ Good-Bad")


#2.2.9 Personal status and sex
# Attribute 9:  (qualitative)
#-----------------------------------------------------------
# Personal status and sex - you may not use for some country due to regulations

#         A91 : male   : divorced/separated
#         A92 : female : divorced/separated/married
#               A93 : male   : single
#         A94 : male   : married/widowed
#         A95 : female : single

A9<-gbpct(cdata$personal_status_9)

op9<-par(mfrow=c(1,2))
plot(cdata$personal_status_9, cdata$good_bad_21, 
     main=" Personal status",
     xlab=" Personal status",
     ylab="Good-Bad")


barplot(A9$WOE, col="brown", names.arg=c(A9$Levels),
        main="Personal status",
        xlab="Category",
        ylab="WOE")

par(op9)

kable(A9, caption =  "Personal status ~ Good-Bad")


#2.2.10 Other debtors / guarantors
# Attribute 10: (qualitative)      
#-----------------------------------------------------------
# Other debtors / guarantors

#         A101 : none
#         A102 : co-applicant
#         A103 : guarantor

A10<-gbpct(cdata$other_debtors_or_grantors_10)

op10<-par(mfrow=c(1,2))

plot(cdata$other_debtors_or_grantors_10, cdata$good_bad_21, 
     main="Other debtors / guarantors",
     xlab="Category",
     ylab="Good-Bad")

barplot(A10$WOE, col="brown", names.arg=c(A10$Levels),
        main="Other debtors / guarantors",
        xlab="Category",
        ylab="WOE")

par(op10)

kable(A10, caption = "Other debtors / guarantors ~ Good-Bad")

#2.2.11 Present residence since
# Attribute 11: (numerical)
#-----------------------------------------------------------
# Present residence since
summary(cdata$present_residence_since_11)

A11<-gbpct(cdata$present_residence_since_11)

op11<-par(mfrow=c(1,2))
histogram(cdata$present_residence_since_11,
          main="Present Residence~ Good-Bad",
          xlab="Present residence Since", 
          ylab="Percent Population")

barplot(A11$WOE, col="brown", names.arg=c(A11$Levels),
        main="Present Residence",
        xlab="Category",
        ylab="WOE")
par(op11)

kable(A11, caption = "Present Residence~ Good-Bad")


2.2.12 Property Type
# Attribute 12: (qualitative)
#-----------------------------------------------------------
# Property
#         A121 : real estate
#         A122 : if not A121 : building society savings agreement/
#                  life insurance
#               A123 : if not A121/A122 : car or other, not in attribute 6
#         A124 : unknown / no property

A12 <- gbpct(cdata$property_type_12)

op12 <- par(mfrow = c(1,2))
plot(cdata$property_type_12, cdata$good_bad_21, 
     main = "Property Type",
     xlab="Type",
     ylab="Good-Bad")         

barplot(A12$WOE, col="brown", names.arg=c(A12$Levels),
        main="Property Type",
        xlab="Category",
        ylab="WOE")

par(op12)

kable(A12,  caption = "Property Type")

#2.2.13 Age in Years
# Attribute 13: (numerical)
#-----------------------------------------------------------
# Age in Years

summary(cdata$age_in_yrs_13)

op13 <- par(mfrow = c(1,2))
boxplot(cdata$age_in_yrs_13)

plot(as.factor(cdata$age_in_yrs_13),  cdata$good_bad_21,
     main = "Age",
     xlab = "Age in Years",
     ylab = "Good-Bad")

par(op13)

# Group AGE - Coarse Classing (after some iterations in fine classing stage)
cdata$age_in_yrs_13 <- as.factor(ifelse(cdata$age_in_yrs_13<=25, '0-25',
                                        ifelse(cdata$age_in_yrs_13<=30, '25-30',
                                               ifelse(cdata$age_in_yrs_13<=35, '30-35', 
                                                      ifelse(cdata$age_in_yrs_13<=40, '35-40', 
                                                             ifelse(cdata$age_in_yrs_13<=45, '40-45', 
                                                                    ifelse(cdata$age_in_yrs_13<=50, '45-50',
                                                                           ifelse(cdata$age_in_yrs_13<=60, '50-60',
                                                                                  '60+'))))))))


A13<-gbpct(cdata$age_in_yrs_13)

op13_1<-par(mfrow=c(1,2))
plot(as.factor(cdata$age_in_yrs_13),  cdata$good_bad_21, 
     main="Age (After Grouping)",
     xlab="Other instalment plans",
     ylab="Good-Bad")


barplot(A13$WOE, col="brown", names.arg=c(A13$Levels),
        main="Age",
        xlab="Category",
        ylab="WOE")


par(op13_1)

kable(A13,  caption = "Age (After Grouping) ~ Good-Bad")

#2.2.14 Other instalment plans
# Attribute 14: (qualitative)
#-----------------------------------------------------------
#         Other instalment plans 
#         A141 : bank
#         A142 : stores
#         A143 : none

A14<-gbpct(cdata$other_instalment_type_14)

op14<-par(mfrow=c(1,2))

plot(cdata$other_instalment_type_14, cdata$good_bad_21, 
     main="Other instalment plans ~ Good-Bad",
     xlab="Other instalment plans",
     ylab="Good-Bad")

barplot(A14$WOE, col="brown", names.arg=c(A14$Levels),
        main="Other instalment plans",
        xlab="Category",
        ylab="WOE")

#2.3
var_list_1 <- IV$Summary[IV$Summary$IV > 2,] # 15 variables
cdata_reduced_1 <- cdata[, c(var_list_1$Variable,"good_bad_21")] #16 variables

#2.4
# Step 1: Subset quantitative and qualitative variables X.quanti and X.quali

factors <- sapply(cdata_reduced_1, is.factor)
#subset Qualitative variables 
vars_quali <- cdata_reduced_1[,factors]
#vars_quali$good_bad_21<-vars_quali$good_bad_21[drop=TRUE] # remove empty factors
str(vars_quali)

#subset Quantitative variables 
vars_quanti <- cdata_reduced_1[,!factors]
str(vars_quanti)

#2.4.1 Hierarchical Clustering of Variables
#Step 2: Hierarchical Clustering of Variables
# requires library(ClustOfVar)
# Need help type ?hclustvar on R console

tree <- hclustvar(X.quanti=vars_quanti,X.quali=vars_quali[,-c(12)])
plot(tree, main="variable clustering")
rect.hclust(tree, k=10,  border = 1:10)

summary(tree)


# Phylogenetic trees
# require library("ape")
plot(as.phylo(tree), type = "fan",
     tip.color = hsv(runif(15, 0.65,  0.95), 1, 1, 0.7),
     edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), 
     edge.width = runif(20,  0.5, 3), use.edge.length = TRUE, col = "gray80")

summary.phylo(as.phylo(tree))

stab<-stability(tree,B=50) # Bootstrap 50 times

# plot(stab,main="Stability of the partitions")
boxplot(stab$matCR)

part<-cutreevar(tree,10)
print(part)

summary(part)


#2.4.2
# kmeans for variable clustering
# requires library(ClustOfVar)
# Need help type ?kmeansvar on R console

kfit<-kmeansvar(X.quanti = vars_quanti, X.quali = vars_quali[,-c(12)], init=10,
                iter.max = 150, nstart = 1, matsim = TRUE)
summary(kfit)

plot(cbind(vars_quanti, vars_quali), as.factor(kfit$cluster))

kfit$E


#2.5 Subset data -2: Based on Variable Clustering
keep<- c(1:8,12,13,21)
cdata_reduced_2 <- cdata[,keep]
str(cdata_reduced_2)

#2.6 Random Sampling (Train and Test)
#2.6.1
div_part <- sort(sample(nrow(cdata_reduced_2), nrow(cdata_reduced_2)*.6))

#select training sample 
train<-cdata_reduced_2[div_part,] # 70% here
pct(train$good_bad_21)
# put remaining into test sample
test<-cdata_reduced_2[-div_part,] # rest of the 30% data goes here
pct(test$good_bad_21)


#2.6.2
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

save(train_1, file="train_1.RData")
save(test_1, file="test_1.RData")

# For neural network we would need contious data
# Sampling for Neural Network - It can be used for other modeling as well
div_part_2 <- createDataPartition(y = cdatanum[,25], p = 0.7, list = F)

# Training Sample for Neural Network
train_num <- cdatanum[div_part_2,] # 70% here

# Test Sample for Neural Network
test_num <- cdatanum[-div_part_2,] # rest of the 30% data goes here

# Save for the future
save(train_num, file="train_num.RData")
save(test_num, file="test_num.RData")

#3 Model Selection and Development
#3.1 Logistic Regression





