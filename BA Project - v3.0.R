
# ------------------------------- 1. Data --------------------------------------

# Loading Required Packages
library(dplyr)
library(Hmisc)
library(naniar)
library(mice)
library(ggplot2)
library(caTools)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(ROCR)
library(randomForest)
library(gains)

# Loading Dataset
df <- read.csv('Train_Dataset.csv', na.strings = '')
str(df)

# ---------------------- 2. Exploratory Data Analysis --------------------------

# ------------------------- 2.1 Descriptive Statistics -------------------------

# Specifying Numerical Variables
cols <- c(2, 7:9, 17:21, 30, 36)
df[cols] <- lapply(df[cols], as.numeric)
str(df)

describe(df)

# -------------------------- 2.2 Univariate Analysis ----------------------------

# Default Variable
# Bar chart
ggplot(df, aes(x= Default)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = 1.6) +
  labs(y = "Percent", fill="Default") +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_discrete( labels = c('No', 'Yes')) +
  ggtitle("Default Variable")

# ----------------------------- 3. Data Preprocessing ---------------------------

# --------------------------------- 3.1 Outliers -----------------------------

# Outlier Function
outliers <- function(var) {
  boxplot(var, plot = FALSE)$out
}

# Counting Outliers
nrow(subset(df, Client_Income %in% outliers(df$Client_Income)))
nrow(subset(df, Child_Count %in% outliers(df$Child_Count)))
nrow(subset(df, Credit_Amount %in% outliers(df$Credit_Amount)))
nrow(subset(df, Loan_Annuity %in% outliers(df$Loan_Annuity)))
nrow(subset(df, Employed_Days %in% outliers(df$Employed_Days)))
nrow(subset(df, Registration_Days %in% outliers(df$Registration_Days)))
nrow(subset(df, ID_Days %in% outliers(df$ID_Days)))
nrow(subset(df, Client_Family_Members %in% outliers(df$Client_Family_Members)))
nrow(subset(df, Score_Source_2 %in% outliers(df$Score_Source_2)))
nrow(subset(df, Credit_Bureau %in% outliers(df$Credit_Bureau)))

# ---------------------------- 3.2 Missing Values ------------------------------

# Rows without NAs
dim(na.omit(df))

# Summary of Missing Values
miss_var_summary(df)

# Missing Values
gg_miss_var(df)

# Preprocessing Function
dataset_prep <- function(dataset1, omit = F) {
  # Specifying Numerical Variables
  # cols <- c(2, 7:9, 17:21, 30, 36)
  # dataset[cols] <- lapply(dataset[cols], as.numeric)
  
  dataset <- dataset1
  
  dataset$Employed_Days[dataset$Employed_Days %in% outliers(dataset$Employed_Days)] <- NA
  
  temp <- c('Businessman', 'Maternity leave', 'Student', 'Unemployed')
  dataset$Client_Income_Type[dataset$Client_Income_Type %in% temp] <- 'Other'
  
  temp <- c('Business Entity Type 1', 'Business Entity Type 2', 'Business Entity Type 3')
  dataset$Type_Organization[dataset$Type_Organization %in% temp] <- 'Business Entity'
  temp <- c('Industry: type 1', 'Industry: type 2', 'Industry: type 3', 'Industry: type 4',
            'Industry: type 5', 'Industry: type 6', 'Industry: type 7', 'Industry: type 8',
            'Industry: type 9', 'Industry: type 10', 'Industry: type 11',
            'Industry: type 12', 'Industry: type 13')
  dataset$Type_Organization[dataset$Type_Organization %in% temp] <- 'Industry'
  temp <- c('Trade: type 1', 'Trade: type 2', 'Trade: type 3', 'Trade: type 4',
            'Trade: type 5', 'Trade: type 6', 'Trade: type 7')
  dataset$Type_Organization[dataset$Type_Organization %in% temp] <- 'Trade'
  temp <- c('Transport: type 1', 'Transport: type 2', 'Transport: type 3', 'Transport: type 4')
  dataset$Type_Organization[dataset$Type_Organization %in% temp] <- 'Transport'
  dataset$Type_Organization[is.na(dataset$Type_Organization)] <- 'XNA'
  
  if (omit == F) {
    dataset$Client_Income[dataset$Client_Income > 100000] = 100000
    dataset$Child_Count[dataset$Child_Count > 4] = 4
    dataset$Client_Family_Members[dataset$Client_Family_Members > 6] = 6
    dataset$Credit_Bureau[dataset$Credit_Bureau > 10] = 10
    dataset$Score_Source_2[dataset$Score_Source_2 %in% outliers(dataset$Score_Source_2)] <- NA
    dataset$Accompany_Client[dataset$Accompany_Client == '##'] <- NA
    dataset$Client_Gender[dataset$Client_Gender == 'XNA'] <- NA   
  } else {
    dataset <- subset(dataset, !(Client_Income %in% outliers(dataset$Client_Income)) | is.na(Client_Income))
    dataset <- subset(dataset, !(Child_Count %in% outliers(dataset$Child_Count)) | is.na(Child_Count))
    dataset <- subset(dataset, !(Credit_Amount %in% outliers(dataset$Credit_Amount)) | is.na(Credit_Amount))
    dataset <- subset(dataset, !(Loan_Annuity %in% outliers(dataset$Loan_Annuity)) | is.na(Loan_Annuity))
    dataset <- subset(dataset, !(Registration_Days %in% outliers(dataset$Registration_Days)) | is.na(Registration_Days))
    dataset <- subset(dataset, !(ID_Days %in% outliers(dataset$ID_Days)) | is.na(ID_Days))
    dataset <- subset(dataset, !(Client_Family_Members %in% outliers(dataset$Client_Family_Members)) | is.na(Client_Family_Members))
    dataset <- subset(dataset, !(Score_Source_2 %in% outliers(dataset$Score_Source_2)) | is.na(Score_Source_2))
    dataset <- subset(dataset, Accompany_Client != '##' | is.na(Accompany_Client))
    dataset <- subset(dataset, Client_Gender != 'XNA' | is.na(Client_Gender))
  }
  
  # Specifying Categorical Variables
  cols = c(3:7, 10:16, 23:29, 31:33, 39, 40 )
  dataset[cols] <- lapply(dataset[cols], factor)
  
  na_dim = dim(na.omit(dataset))
  print(paste("The Number of Complete rows is", na_dim[1]))
  dataset
}


df_norm <- dataset_prep(df)
df_omit <- dataset_prep(df, omit = T)


# ------------------------------ 3.3 Imputation --------------------------------

# Warning: The Imputation Process will takes several minutes

# Select Dataset to impute
# 
# df <- df_norm
# 
# df <- df_omit
# 
# 
# met <- c('pmm', 'logreg', 'logreg', 'logreg', 'logreg', 'pmm', 'pmm', 'pmm', 'polyreg',
#          'polyreg', 'polyreg', 'polyreg', 'logreg', 'logreg', 'polyreg', 'pmm', 'pmm',
#          'pmm', 'pmm', 'pmm', 'pmm', '', '', 'polyreg', 'pmm', 'pmm', 'pmm', 'pmm',
#          '', '', 'pmm', 'pmm', 'pmm', 'pmm', 'pmm', 'pmm')
# imputed_Data <- mice(df[, c(2:22, 24:32, 34:39)], m = 1, method = met, seed = 500)
# 
# summary(imputed_Data)
# 
# imputed_Data <- complete(imputed_Data)
# dim(na.omit(imputed_Data))
# 
# describe(imputed_Data)
# 
# df[, c(2:22, 24:32, 34:39)] <- imputed_Data
# # 
# # 
# write.csv(df, 'Train_Imputed_norm.csv', row.names = F)
# 
# write.csv(df, 'Train_Imputed_omit.csv', row.names = F)

rm(df)
rm(df_norm)
rm(df_omit)

# ----------------------------- 4. Visualization -----------------------------

#Loading Dataset
df <- read.csv('Train_Imputed_norm.csv', stringsAsFactors = T)
df$Default <- factor(df$Default)

# 01. Association of Default and Client_Income_Type
# Bar chart
ggplot(df, aes(x= Default,  group=Client_Income_Type)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = 1.6) +
  labs(y = "Percent", fill="Default") +
  facet_grid(~Client_Income_Type) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_discrete( labels = c('No', 'Yes')) +
  ggtitle("Association of Default and Client_Income_Type")

# 02. Association of Default and Client_Education
# Bar chart
ggplot(df, aes(x= Default,  group=Client_Education)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = 1.6) +
  labs(y = "Percent", fill="Default") +
  facet_grid(~Client_Education) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_discrete( labels = c('No', 'Yes')) +
  ggtitle("Association of Default and Client_Education")

# 03. Association of Default and Client_Gender
# Bar chart
ggplot(df, aes(x= Default,  group=Client_Gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = 1.6) +
  labs(y = "Percent", fill="Default") +
  facet_grid(~Client_Gender) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_discrete( labels = c('No', 'Yes')) +
  ggtitle("Association of Default and Client_Gender")

# 04. Association of Default and Loan_Contract_Type
# Bar chart
ggplot(df, aes(x= Default,  group=Loan_Contract_Type)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = 1.6) +
  labs(y = "Percent", fill="Default") +
  facet_grid(~Loan_Contract_Type) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_discrete( labels = c('No', 'Yes')) +
  ggtitle("Association of Default and Loan_Contract_Type")

# 05. Association of Default and Client_Housing_Type
# Bar chart
ggplot(df, aes(x= Default,  group=Client_Housing_Type)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = 1.6) +
  labs(y = "Percent", fill="Default") +
  facet_grid(~Client_Housing_Type) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_discrete( labels = c('No', 'Yes')) +
  ggtitle("Association of Default and Client_Housing_Type")

# 06. Association of Default and Age_Days
#Boxplot
temp <- subset(df, !(df$Age_Days %in% outliers(df$Age_Days)))

ggplot(temp, aes(x = Default, y = Age_Days)) + 
  geom_boxplot(fill = 'lightblue', outlier.shape = 4) + 
  coord_flip() +
  ggtitle("Association of Default and Age_Days")

# 07. Association of Default and Employed_Days
#Boxplot
temp <- subset(df, !(df$Employed_Days %in% outliers(df$Employed_Days)))

ggplot(temp, aes(x = Default, y = Employed_Days)) + 
  geom_boxplot(fill = 'lightblue', outlier.shape = 4) + 
  coord_flip() +
  ggtitle("Association of Default and Employed_Days")

# 08. Association of Default and Registration_Days
#Boxplot
temp <- subset(df, !(df$Registration_Days %in% outliers(df$Registration_Days)))

ggplot(temp, aes(x = Default, y = Registration_Days)) + 
  geom_boxplot(fill = 'lightblue', outlier.shape = 4) + 
  coord_flip() +
  ggtitle("Association of Default and Registration_Days")

# 09. Association of Default and ID_Days
#Boxplot
temp <- subset(df, !(df$ID_Days %in% outliers(df$ID_Days)))

ggplot(temp, aes(x = Default, y = ID_Days)) + 
  geom_boxplot(fill = 'lightblue', outlier.shape = 4) + 
  coord_flip() +
  ggtitle("Association of Default and ID_Days")

# 10. Association of Default and Own_House_Age
#Boxplot
temp <- subset(df, !(df$Own_House_Age %in% outliers(df$Own_House_Age)))

ggplot(df, aes(x = Default, y = Own_House_Age)) + 
  geom_boxplot(fill = 'lightblue', outlier.shape = 4) + 
  coord_flip() +
  ggtitle("Association of Default and Own_House_Age")

# 11. Association of Default and Cleint_City_Rating
# Workphone_Working
ggplot(df, aes(x= Default,  group=Cleint_City_Rating)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = 1.6) +
  labs(y = "Percent", fill="Default") +
  facet_grid(~Cleint_City_Rating) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_discrete( labels = c('No', 'Yes')) +
  ggtitle("Association of Default and Cleint_City_Rating")

# 12. Association of Default and Client_Permanent_Match_Tag
# Application_Process_Day
ggplot(df, aes(x= Default,  group=Client_Permanent_Match_Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = 1.6) +
  labs(y = "Percent", fill="Default") +
  facet_grid(~Client_Permanent_Match_Tag) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_discrete( labels = c('No', 'Yes')) +
  ggtitle("Association of Default and Client_Permanent_Match_Tag")

# 13. Association of Default and Score_Source_1
#Boxplot
temp <- subset(df, !(df$Score_Source_1 %in% outliers(df$Score_Source_1)))

ggplot(temp, aes(x = Default, y = Score_Source_1)) + 
  geom_boxplot(fill = 'lightblue', outlier.shape = 4) + 
  coord_flip() +
  ggtitle("Association of Default and Score_Source_1")

# 14. Association of Default and Score_Source_2
#Boxplot
temp <- subset(df, !(df$Score_Source_2 %in% outliers(df$Score_Source_2)))

ggplot(temp, aes(x = Default, y = Score_Source_2)) + 
  geom_boxplot(fill = 'lightblue', outlier.shape = 4) + 
  coord_flip() +
  ggtitle("Association of Default and Score_Source_2")

# 15. Association of Default and Score_Source_3
#Boxplot
temp <- subset(df, !(df$Score_Source_3 %in% outliers(df$Score_Source_3)))

ggplot(temp, aes(x = Default, y = Score_Source_3)) + 
  geom_boxplot(fill = 'lightblue', outlier.shape = 4) + 
  coord_flip() +
  ggtitle("Association of Default and Score_Source_3")

# 16. Association of Default and Social_Circle_Default
#Boxplot
temp <- subset(df, !(df$Social_Circle_Default %in% outliers(df$Social_Circle_Default)))

ggplot(temp, aes(x = Default, y = Social_Circle_Default)) + 
  geom_boxplot(fill = 'lightblue', outlier.shape = 4) + 
  coord_flip() +
  ggtitle("Association of Default and Social_Circle_Default")

# 17. Association of Default and Phone_Change

#Boxplot
temp <- subset(df, !(df$Phone_Change %in% outliers(df$Phone_Change)))

ggplot(temp, aes(x = Default, y = Phone_Change)) + 
  geom_boxplot(fill = 'lightblue', outlier.shape = 4) + 
  coord_flip() +
  ggtitle("Association of Default and Phone_Change")

rm(temp)

# ---------------------------- 5. Hypothesis Testing ---------------------------

# 01. Default and Client_Education
n1 <- sum(df$Client_Education == "Post Grad")
n2 <- sum(df$Client_Education == "Graduation")
win1 <- sum(df$Client_Education == "Post Grad" & df$Default == 1)
win2 <- sum(df$Client_Education == "Graduation" & df$Default == 1)
prop.test(x=c(win1, win2), n=c(n1,n2), alternative = "two.sided", correct = F)


# 02. Default and Client_Gender
G_D <- subset(df, Client_Gender %in% c("Male", "Female"))
tabG_D <- table(G_D$Client_Gender, G_D$Default)
tabG_D
chisq.test(tabG_D, correct = FALSE)


# 03. client_income
xbar <- mean(df$Client_Income)
se <- 2/sqrt(nrow(df))
ztest <- (xbar-16000)/se
qnorm(0.95)
pnorm(ztest, lower.tail = F)

t.test(df$Client_Income, mu=16000, alternative = "greater")

power.t.test(n=1000, delta=0.2, sd=1.98, sig.level = 0.1, type="one.sample", alternative = "one.sided")


# 04. confidence interval client_income
sd <- sd(df$Client_Income)
n=1000
sample_xbar <- vector("numeric", n)
for (i in 1:n) {
  index <- sample(1:nrow(df), 30)
  my_sample <- df[index, ]
  xbar <- mean(df$Client_Income)
  sample_xbar[i] <- xbar
}
qqnorm(sample_xbar)
qqline(sample_xbar)

qqnorm(df$Client_Income)
qqline(df$Client_Income)

Q <- qnorm(1-0.05/2)
se <- sd/sqrt(30)
interval <- c(xbar-Q*se , xbar+Q*se)
interval


# 05. Default and Client_Housing_Type
n1 <- sum(df$Client_Housing_Type == "Rental")
n2 <- sum(df$Client_Housing_Type %in% c("Family", "Home", "Municipal", "Office", "Shared"))
win1 <- sum(df$Client_Housing_Type == "Rental" & df$Default == 1)
win2 <- sum(df$Client_Housing_Type %in% c("Family", "Home", "Municipal", "Office", "Shared") & df$Default == 1)
prop.test(x=c(win1, win2), n=c(n1,n2), alternative = "two.sided", correct = F)


# 06. Default and Age_Days
n1 <- sum(df$Age_Days >= "15000")
n2 <- sum(df$Age_Days < "15000")
win1 <- sum(df$Age_Days >= "15000" & df$Default == 0)
win2 <- sum(df$Age_Days < "15000" & df$Default == 0)
prop.test(x=c(win1, win2), n=c(n1,n2), alternative = "two.sided", correct = F)


# 07. Default and Cleint_City_Rating
n1 <- sum(df$Cleint_City_Rating == "1")
n2 <- sum(df$Cleint_City_Rating == "2")
win1 <- sum(df$Cleint_City_Rating == "1" & df$Default == 1)
win2 <- sum(df$Cleint_City_Rating == "2" & df$Default == 1)
prop.test(x=c(win1, win2), n=c(n1,n2), alternative = "two.sided", correct = F)

n1 <- sum(df$Cleint_City_Rating == "1")
n2 <- sum(df$Cleint_City_Rating == "3")
win1 <- sum(df$Cleint_City_Rating == "1" & df$Default == 1)
win2 <- sum(df$Cleint_City_Rating == "3" & df$Default == 1)
prop.test(x=c(win1, win2), n=c(n1,n2), alternative = "two.sided", correct = F)


# ------------------------- 6. Prediction Models -------------------------------

#Loading Dataset
df <- read.csv('Train_Imputed_norm.csv', stringsAsFactors = T)
df$Default <- factor(df$Default)

# Split dataset to train and test Sets
set.seed(500)
spt <- sample.split(df$Default, SplitRatio = 0.8)
train <- subset(df, spt == T)
test  <- subset(df, spt == F)
rm(spt)

# -------------------------- 6.1 Logistic Regression ---------------------------

lr_model = glm(Default ~ . -ID -Mobile_Tag, data = train, family = 'binomial')
summary(lr_model)

lr_model = glm(Default ~ . -ID - Mobile_Tag -Bike_Owned -Active_Loan -Application_Process_Hour
               -Child_Count -Type_Organization -Client_Occupation
               -Population_Region_Relative -Registration_Days -Client_Contact_Work_Tag
               -Accompany_Client -Client_Marital_Status -Credit_Bureau
               -Client_Family_Members -Homephone_Tag
               , data = train, family = 'binomial' )
summary(lr_model)

# Model Optimization
pred <- predict(lr_model, type = 'response')
pred_class <- factor(ifelse(pred >= 0.08, 1, 0))
confusionMatrix(pred_class, train$Default)

# ROC Curve for train dataset
pred_roc <- prediction(predictions = pred, labels = train$Default)
perf_roc <- performance(pred_roc, 'tpr', 'fpr')
plot(perf_roc, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(0.2, 1))
mtext(side = 3, text="Logistic Regression ROC Curve for train data")

auc <- performance(pred_roc, "auc")
auc <- as.numeric(auc@y.values)
auc

# Model Performance
pred <- predict(lr_model, newdata = test, type = 'response')
pred_class <- factor(ifelse(pred >= 0.08, 1, 0))
confusionMatrix(pred_class, test$Default)

# ROC Curve for test Dataset
pred_roc <- prediction(predictions = pred, labels = test$Default)
perf_roc <- performance(pred_roc, 'tpr', 'fpr')
plot(perf_roc, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(0.2, 1))
mtext(side = 3, text="Logistic Regression ROC Curve for test data")

auc <- performance(pred_roc, "auc")
auc <- as.numeric(auc@y.values)
auc

# ---------------------------- 6.2 Decision Tree -------------------------------

# Specifying Num. and Cat. Variables
num_cols <- c('Client_Income', 'Credit_Amount', 'Loan_Annuity', 'Population_Region_Relative',
              'Age_Days', 'Employed_Days', 'Registration_Days', 'ID_Days', 'Own_House_Age',
              'Application_Process_Hour', 'Score_Source_1', 'Score_Source_2', 'Score_Source_3',
              'Social_Circle_Default', 'Phone_Change', 'Credit_Bureau', 'Child_Count', 'Client_Family_Members')

cat_cols <- c('Car_Owned', 'Bike_Owned', 'Active_Loan', 'House_Own', 'Accompany_Client',
              'Client_Income_Type', 'Client_Education', 'Client_Marital_Status',
              'Client_Gender', 'Loan_Contract_Type', 'Client_Housing_Type', 'Mobile_Tag',
              'Homephone_Tag', 'Workphone_Working', 'Client_Occupation', 'Cleint_City_Rating',
              'Client_Permanent_Match_Tag', 'Client_Contact_Work_Tag', 'Type_Organization',
              'Application_Process_Day')

# Normalize Num. Variables
cols <- num_cols
pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))

train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

# Converting Cat. Variables to Factor
train[cat_cols] <- lapply(train[cat_cols], factor)
test[cat_cols] <- lapply(test[cat_cols], factor)


# Fitting the Model
t_model = rpart(Default ~ . -ID -Mobile_Tag, data = train, method = 'class', maxdepth = 6, cp = -1)
prp(t_model)

# Model Optimization
pred <- predict(t_model, type = 'prob')
pred <- pred[,2]
pred_class <- factor(ifelse(pred >= 0.08, 1, 0))
confusionMatrix(pred_class, train$Default)

# ROC Curve for train dataset
pred_roc <- prediction(predictions = pred, labels = train$Default)
perf_roc <- performance(pred_roc, 'tpr', 'fpr')
plot(perf_roc, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(0.2, 1))
mtext(side = 3, text="Decision Tree ROC Curve for train data")

auc <- performance(pred_roc, "auc")
auc <- as.numeric(auc@y.values)
auc

# Model Performance
pred <- predict(t_model, newdata = test, type = 'prob')
pred <- pred[,2]
pred_class <- factor(ifelse(pred >= 0.08, 1, 0))
confusionMatrix(pred_class, test$Default)

# ROC Curve for test dataset
pred_roc <- prediction(predictions = pred, labels = test$Default)
perf_roc <- performance(pred_roc, 'tpr', 'fpr')
plot(perf_roc, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(0.2, 1))
mtext(side = 3, text="Decision Tree ROC Curve for test data")

auc <- performance(pred_roc, "auc")
auc <- as.numeric(auc@y.values)
auc

# --------------------------- 6.3 Random Forest Model --------------------------

rf_model <- randomForest(Default ~ . -ID -Mobile_Tag
                         , data = train, ntree = 500)
rf_model

varImpPlot(rf_model, scale = TRUE)

rf_model <- randomForest(Default ~ . -ID -Mobile_Tag -House_Own -Bike_Owned -Active_Loan
                         -Car_Owned -Workphone_Working
                         , data = train, ntree = 500)
rf_model

# Model Optimization
pred <- predict(rf_model, type = 'prob')
pred <- pred[,2]
pred_class <- factor(ifelse(pred >= 0.11, 1, 0))
confusionMatrix(pred_class, train$Default)

# ROC Curve for train dataset
pred_roc <- prediction(predictions = pred, labels = train$Default)
perf_roc <- performance(pred_roc, 'tpr', 'fpr')
plot(perf_roc, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(0.2, 1))
mtext(side = 3, text="Random Forest ROC Curve for train data")

auc <- performance(pred_roc, "auc")
auc <- as.numeric(auc@y.values)
auc

# Model Performance
pred <- predict(rf_model, newdata = test, type = 'prob')
pred <- pred[,2]
pred_class <- factor(ifelse(pred >= 0.11, 1, 0))
confusionMatrix(pred_class, test$Default)

# ROC Curve for test dataset
pred_roc <- prediction(predictions = pred, labels = test$Default)
perf_roc <- performance(pred_roc, 'tpr', 'fpr')
plot(perf_roc, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(0.2, 1))
mtext(side = 3, text="Random Forest ROC Curve for test data")

auc <- performance(pred_roc, "auc")
auc <- as.numeric(auc@y.values)
auc

varImpPlot(rf_model, scale = TRUE)

# ----------------------------- 7. Comparison ----------------------------------

# ROC Comparison


# Model Performance
pred <- predict(lr_model, newdata = test, type = 'response')
pred_roc <- prediction(predictions = pred, labels = test$Default)
perf_roc <- performance(pred_roc, 'tpr', 'fpr')
plot(perf_roc, col = 'red')

pred <- predict(t_model, newdata = test, type = 'prob')
pred <- pred[,2]
pred_roc <- prediction(predictions = pred, labels = test$Default)
perf_roc <- performance(pred_roc, 'tpr', 'fpr')
plot(perf_roc, col = 'green', add = T)

pred <- predict(rf_model, newdata = test, type = 'prob')
pred <- pred[,2]
pred_roc <- prediction(predictions = pred, labels = test$Default)
perf_roc <- performance(pred_roc, 'tpr', 'fpr')
plot(perf_roc, col = 'blue', add = T)

legend("topleft", legend = c('Logistic Regression', 'Decision Tree', 'Random Forest'),
       col = c('red', 'green', 'blue'), lty = 1:2, cex=0.8)
mtext(side = 3, text="ROC Chart Comparison")
