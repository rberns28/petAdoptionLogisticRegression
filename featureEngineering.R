#### Data Profiling ###
setwd("~/Fordham/Statistical Programming with R/Final Project")
base <- read.csv(file = 'shelterdata.csv',sep = ",",header = T,na.strings="")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(VIM)
library(mice)
library(caret)
library(mlbench)

#--------------- Building Features ---------------#

# Build Date Features
base$DateTime = as.POSIXct(strptime(base$DateTime, "%m/%d/%Y %H:%M"))

# No Name Indicator
base<-mutate(base, NoNameInd = as.factor(ifelse(is.na(Name), 1, 0)))

# Days Old
base$numericPart <- gsub("[^0-9]", "", base$AgeuponOutcome)
base <- base %>%
  mutate(
    daysOld = case_when(
      gsub("[0-9]|[[:space:]]", "", base$AgeuponOutcome)=='year' ~ (as.numeric(gsub("[^0-9]", "", base$AgeuponOutcome))*365),
      gsub("[0-9]|[[:space:]]", "", base$AgeuponOutcome)=='years' ~ (as.numeric(gsub("[^0-9]", "", base$AgeuponOutcome))*365),
      gsub("[0-9]|[[:space:]]", "", base$AgeuponOutcome)=='weeks' ~ (as.numeric(gsub("[^0-9]", "", base$AgeuponOutcome))*7),
      gsub("[0-9]|[[:space:]]", "", base$AgeuponOutcome)=='month' ~ (as.numeric(gsub("[^0-9]", "", base$AgeuponOutcome))*30),
      gsub("[0-9]|[[:space:]]", "", base$AgeuponOutcome)=='months' ~ (as.numeric(gsub("[^0-9]", "", base$AgeuponOutcome))*30),
      gsub("[0-9]|[[:space:]]", "", base$AgeuponOutcome)=='days' ~ (as.numeric(gsub("[^0-9]", "", base$AgeuponOutcome))),
      gsub("[0-9]|[[:space:]]", "", base$AgeuponOutcome)=='week' ~ (as.numeric(gsub("[^0-9]", "", base$AgeuponOutcome))*7),
      gsub("[0-9]|[[:space:]]", "", base$AgeuponOutcome)=='day' ~ (as.numeric(gsub("[^0-9]", "", base$AgeuponOutcome)))
    )
  )

# Binned Days Old
base <- base %>%
  mutate(
    daysOldBinned = case_when(
       daysOld <= 90 ~ "3 Months",
       daysOld <= 180 & daysOld >90 ~ "6 Months",
       daysOld <= 365 & daysOld >180 ~ "1 Year",
       daysOld <= 730 & daysOld >365 ~ "2 Years",
       daysOld <= 1825 & daysOld > 730 ~ "5 Years",
       daysOld <= 3650 & daysOld >1825 ~ "10 Years",
       daysOld >= 3651 ~ "10+ Years")
  )


# Pit Indicator
base <- base %>% mutate(pitIndicator=ifelse(grepl('pit',tolower(Breed)), 'Y', 'N'))

# Gender
base <- base %>%mutate(
  sex = case_when(
    grepl('female',tolower(SexuponOutcome)) ~ "Female",
    grepl('male',tolower(SexuponOutcome)) ~ "Male",
    TRUE ~ "Unknown"
    )
)

# Spayed / Neutered Indicator
base <- base %>%
  mutate(Spayed = case_when(
    (grepl('spayed',tolower(SexuponOutcome))|grepl('neutered',tolower(SexuponOutcome))) ~"Spayed/Neutered",
    (grepl('intact',tolower(SexuponOutcome))) ~"Intact",
    TRUE ~ "Unknown"))

# Mixed Breed Indicator
base <- base %>% mutate(mixedBreed=ifelse(grepl('mix',tolower(Breed)), 'Y', 'N'))

# Time
base <- base %>% mutate(hour=format(DateTime, "%H"))

# Month
base <- base %>% mutate(month=format(DateTime, "%m"))

# Black Color Indicator
base <- base %>% mutate(blackColorInd=ifelse(grepl('black',tolower(Color)), 'Y', 'N'))

#---------------------- Count Nulls ----------------------#
sapply(base, function(x) sum(is.na(x)))
md.pattern(base)

aggr(base[,1:9], numbers=T, sortVars=T, labels=T)

#---------------------- Split Train and Test -------------------------#

#base <- na.omit(base[,-2])
#drops <- c('AnimalID','DateTime','SexuponOutcome','AgeuponOutcome','numericPart','daysOld','SpayedIndicator')
drops <- c('AnimalID','DateTime','SexuponOutcome','AgeuponOutcome','numericPart')
base_tmp <- base[ , !(names(base) %in% drops)]

smp_size <- floor(0.80 * nrow(base_tmp))
set.seed(123)
train_ind <- sample(seq_len(nrow(base_tmp)), size = smp_size)
train <- base_tmp[train_ind, ]
test <- base_tmp[-train_ind, ]

# ---------------------- Create Breed and Color Transformed Features ------------ #

# Breed Transformation
side1 <- train %>% filter(OutcomeType=="Positive") %>% group_by(Breed) %>% summarise(positive=n())
side2 <- train %>% filter(OutcomeType=="Negative") %>% group_by(Breed) %>% summarise(negative=n())
fin <- inner_join(side1,side2,by="Breed")
fin <- fin %>% mutate(breedPcnt = round(positive/(negative+positive),2))
densityplot(fin$breedPcnt)

train <- fin %>% select(Breed,breedPcnt) %>% inner_join(train,fin,by="Breed")
train <- train %>% select(-Breed)
medianBreed <- median(train$breedPcnt)
test <- left_join(test,fin,by="Breed")
test<- mutate(test, breedPcnt = ifelse(is.na(breedPcnt), medianBreed, breedPcnt)) %>% select(-c(Breed,positive,negative))

# Color Transformation
side1.1 <- train %>% filter(OutcomeType=="Positive") %>% group_by(Color) %>% summarise(positive=n())
side2.1 <- train %>% filter(OutcomeType=="Negative") %>% group_by(Color) %>% summarise(negative=n())
fin2 <- inner_join(side1.1,side2.1,by="Color")
fin2 <- fin2 %>% mutate(colorPcnt = round(positive/(negative+positive),2))
densityplot(fin2$colorPcnt)

train <- fin2 %>% select(Color,colorPcnt) %>% inner_join(train,fin2,by="Color")
train <- train %>% select(-Color)
medianColor <- median(train$colorPcnt)
test <- left_join(test,fin2,by="Color")
test<- mutate(test, breedPcnt = ifelse(is.na(colorPcnt), medianColor, colorPcnt)) %>% select(-c(Color,positive,negative))

train <- train %>% select(-Name)
test <- test %>% select(-Name)

train <- na.omit(train)
test <- na.omit(test)
#---------------------- Plotting -------------------------#

factorPlot <-function(df,dataCol) {
df %>% select(OutcomeType,as.factor(dataCol))  %>% 
  ggplot(aes(x=dataCol, fill=OutcomeType)) +
  geom_bar(position = "fill") +
  labs(x = dataCol, y= "Outcome") + 
  coord_flip()
}

# No Name Indicator Using Plotting Function
factorPlot(train,train$AnimalType)

# No Name Indicator Formatted
train %>% select(OutcomeType,AnimalType,NoNameInd)  %>% 
  ggplot(aes(x=NoNameInd, fill=OutcomeType)) +
  facet_grid(. ~ AnimalType) +
  geom_bar(position = "fill") +
  labs(x = "Does the pet have a name?", y= "Outcome") + 
  scale_x_discrete(labels=c("1" = "No", "0" = "No")) +
  ggtitle("No Name Indicator by Adoption Outcome") +
  scale_y_continuous(labels = percent) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=c("#009e73","#0072b2")) +
  coord_flip()

# Animal Type Formatted
train %>% select(OutcomeType,AnimalType)  %>% 
  ggplot(aes(x=AnimalType, fill=OutcomeType)) +
  geom_bar(position = "fill") +
  labs(x = "Animal Type", y= "Outcome") + 
  ggtitle("Animal Type by Adoption Outcome") +
  scale_y_continuous(labels = percent) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=c("#009e73","#0072b2")) +
  coord_flip()

# Days Old Binned Formatted
train2 <- train %>% mutate(daysOldBinned2 = factor(train$daysOldBinned,c("3 Months","6 Months","1 Year","2 Years","5 Years","10 Years","10+ Years")))
train2 %>% select(OutcomeType,daysOldBinned2,AnimalType)  %>% 
  ggplot(aes(x=daysOldBinned2, fill=OutcomeType)) +
  geom_bar(position = "fill") +
  labs(x = "Days Old", y= "Outcome") + 
  ggtitle("No Name Indicator by Adoption Outcome") +
  scale_y_continuous(labels = percent) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=c("#009e73","#0072b2")) +
  facet_grid(. ~ AnimalType) +
  coord_flip()

# Pit Indicator Formatted
train %>% select(OutcomeType,pitIndicator,AnimalType)  %>% 
  ggplot(aes(x=pitIndicator, fill=OutcomeType)) +
  geom_bar(position = "fill") +
  labs(x = "Pit Indicator", y= "Outcome") + 
  ggtitle("Pit Indicator by Adoption Outcome") +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels=c("Y" = "Yes", "N" = "No")) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=c("#009e73","#0072b2")) +
  facet_grid(. ~ AnimalType) +
  coord_flip()

# Sex Formatted
train %>% select(OutcomeType,sex,AnimalType)  %>% 
  ggplot(aes(x=sex, fill=OutcomeType)) +
  geom_bar(position = "fill") +
  labs(x = "Sex", y= "Outcome") + 
  ggtitle("Sex by Adoption Outcome") +
  scale_y_continuous(labels = percent) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=c("#009e73","#0072b2")) +
  facet_grid(. ~ AnimalType) +
  coord_flip() 

# Spayed / Neutered Formatted
train %>% select(OutcomeType,Spayed,AnimalType)  %>% 
  ggplot(aes(x=Spayed, fill=OutcomeType)) +
  geom_bar(position = "fill") +
  labs(x = "Spayed / Neutered Indicator", y= "Outcome") + 
  ggtitle("Spayed / Neutered Indicator by Adoption Outcome") +
  scale_y_continuous(labels = percent) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=c("#009e73","#0072b2")) +
  facet_grid(. ~ AnimalType) +
  coord_flip() 

# Mixed Breed Formatted
train %>% select(mixedBreed,OutcomeType,AnimalType)  %>% 
  ggplot(aes(x=mixedBreed, fill=OutcomeType)) +
  geom_bar(position = "fill") +
  labs(x = "Mixed Breed Indicator", y= "Outcome") + 
  ggtitle("Mixed Breed Indicator by Adoption Outcome") +
  scale_y_continuous(labels = percent) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=c("#009e73","#0072b2")) +
  facet_grid(. ~ AnimalType) +
  coord_flip() 

# Black Color Indicator Formatted
train %>% select(blackColorInd,OutcomeType,AnimalType)  %>% 
  ggplot(aes(x=blackColorInd, fill=OutcomeType)) +
  geom_bar(position = "fill") +
  labs(x = "Black Color Indicator", y= "Outcome") + 
  ggtitle("Black Color Indicator by Adoption Outcome") +
  scale_y_continuous(labels = percent) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=c("#009e73","#0072b2")) +
  facet_grid(. ~ AnimalType) +
  coord_flip() 

# Days Old formatted Plot
ggplot(base, aes(OutcomeType, daysOld)) +
  geom_boxplot() + 
  ggtitle("Adoption Outcome by Days old") +
  theme_classic() + 
  labs(x = "Outcome Type", y="Days Old") +
  scale_y_continuous(labels = comma) +
  facet_grid(. ~ AnimalType)

# Days Old Binned Formatted
train %>% select(OutcomeType,month,AnimalType)  %>% 
  ggplot(aes(x=month, fill=OutcomeType)) +
  geom_bar(position = "fill") +
  labs(x = "Month", y= "Outcome") + 
  ggtitle("Adoption Outcome by Month") +
  scale_y_continuous(labels = percent) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=c("#009e73","#0072b2")) +
  facet_grid(. ~ AnimalType) +
  coord_flip()

# Hour Formatted Plot
avg_hour <- train %>% group_by(OutcomeType,AnimalType) %>% summarise(Average.Hour = mean(as.numeric(hour)))
ggplot(train, aes(OutcomeType, as.numeric(hour))) +
  geom_boxplot() + 
  ggtitle("Adoption Outcome by Hour") +
  theme_classic() +
  labs(x = "Outcome Type", y = "Hour") +
  scale_y_continuous(labels = comma) +
  facet_grid(. ~ AnimalType)

# 
train %>% select(OutcomeType)  %>% group_by(OutcomeType) %>% summarise(n=n())


#---------------------- Modeling -------------------------#

# Baseline model

glm1 <- glm(OutcomeType ~ . -sex, data=train, family = binomial())
summary(glm1)

# Check for Collinearity
library(car)
vif(glm1)


# Extract more interpretable coefficients and create a function to do it
formatCf <- function(glm_model) {
  for(i in glm_model$coefficients) 
    {
    print((exp(i) - 1)*100)
}}

formatCf(glm1)

# Extract model summary statistics and save model results to a dataframe 
modelSummary <-function(dataset,model,description) {
  predict.class<- round(predict(model,newdata= dataset, type="response"), digits = 0)
  conf <- xtabs(~ dataset$OutcomeType + predict.class)
  accuracy <- (round((conf[1,1] + conf[2,2])/sum(conf),4)*100)
  recall <- (round(conf[2,2]/sum(conf[2,]),4)*100)
  specificity <- (round(conf[1,1]/sum(conf[1,]),4)*100)
  precision <- (round(conf[2,2]/sum(conf[,2]),4)*100)
  newLine <- c(description,accuracy,recall,specificity,precision)
  ifelse(exists("model_results") && is.data.frame(get("model_results")),
         model_results <- rbind(model_results,newLine),
         model_results <- data.frame(description,accuracy,recall,specificity,precision,stringsAsFactors=FALSE))
  model_results <<- model_results
}

modelSummary(dataset = train,model = glm1, description = "Train glm1 with All Features")
modelSummary(dataset = test,model = glm1, description = "Test glm1 with All Features")

# Convert hour to numeric
train$hour <- as.numeric(train$hour)
test$hour <- as.numeric(test$hour)
glm2 <- glm(OutcomeType ~ . -sex, data=train, family = binomial())
summary(glm2)
modelSummary(dataset = train,model = glm2, description = "Train glm2 with All Features and hour converted to numeric")
modelSummary(dataset = test,model = glm2, description = "Test glm2 with All Features and hour converted to numeric")

# Convert month to numeric
train$month <- as.numeric(train$month)
test$month <- as.numeric(test$month)
glm3 <- glm(OutcomeType ~ . -sex, data=train, family = binomial())
summary(glm3)
modelSummary(dataset = train,model = glm3, description = "Train glm3 with All Features and hour & month converted to numeric")
modelSummary(dataset = test,model = glm3, description = "Test glm3 with All Features and hour & month converted to numeric")

# Drop daysOld Binned feature
train$hour <- as.factor(train$hour)
test$hour <- as.factor(test$hour)
train$month <- as.factor(train$month)
test$month <- as.factor(test$month)
glm4 <- glm(OutcomeType ~ . -sex -daysOldBinned, data=train, family = binomial())
summary(glm4)
modelSummary(dataset = train,model = glm4, description = "Train glm4 with factor hour & month AND removing binned age")
modelSummary(dataset = test,model = glm4, description = "Test glm4 with factor hour & month AND removing binned age")

# Include "sex" feature
glm5 <- glm(OutcomeType ~ . -daysOldBinned, data=train, family = binomial())
summary(glm5)
modelSummary(dataset = train,model = glm5, description = "Train glm5 - same as glm4 but includes sex")
modelSummary(dataset = test,model = glm5, description = "Test glm5 - same as glm4 but includes sex")

# Without color pcnt and breed pcnt
glm6 <- glm(OutcomeType ~ . -daysOldBinned -colorPcnt -breedPcnt, data=train, family = binomial())
summary(glm6)
modelSummary(dataset = train,model = glm6, description = "Train glm6 - same as glm5 but removes the pcnt features")
modelSummary(dataset = test,model = glm6, description = "Test glm6 - same as glm5 but removes the pcnt features")

# Only use spayed indicator, animal type and days old
glm7 <- glm(OutcomeType ~ AnimalType + Spayed + daysOld, data=train, family = binomial())
summary(glm7)
modelSummary(dataset = train,model = glm7, description = "Train glm7 - Only use spayed indicator, animal type and days old")
modelSummary(dataset = test,model = glm7, description = "Test glm7 - Only use spayed indicator, animal type and days old")

# Only use spayed indicator
glm8 <- glm(OutcomeType ~ Spayed, data=train, family = binomial())
summary(glm8)
modelSummary(dataset = train,model = glm8, description = "Train glm8 - Only use spayed indicator")
modelSummary(dataset = test,model = glm8, description = "Test glm8 - Only use spayed indicator")

# Only use AnimalType indicator
glm9 <- glm(OutcomeType ~ AnimalType, data=train, family = binomial())
summary(glm9)
modelSummary(dataset = train,model = glm9, description = "Train glm9 - Only use animalType")
modelSummary(dataset = test,model = glm9, description = "Test glm9 - Only use animalType")

# Only use AnimalType and Spayed indicator
glm10 <- glm(OutcomeType ~ AnimalType + Spayed, data=train, family = binomial())
summary(glm10)
modelSummary(dataset = train,model = glm10, description = "Train glm10 - Only use AnimalType and Spayed indicator")
modelSummary(dataset = test,model = glm10, description = "Test glm10 - Only use AnimalType and Spayed indicator")

# Only use AnimalType and Spayed indicator and no name indicator
glm11 <- glm(OutcomeType ~ AnimalType + Spayed + NoNameInd, data=train, family = binomial())
summary(glm11)
modelSummary(dataset = train,model = glm11, description = "Train glm11 - Only use AnimalType, no name ind and Spayed indicator")
modelSummary(dataset = test,model = glm11, description = "Test glm11 - Only use AnimalType, no name ind and Spayed indicator")


# --------------- Lasso Logistic Regression ------------ #
library(glmnet)

trainTmp <- train %>% mutate(hourNumeric = as.numeric(hour))
trainTmp <- trainTmp %>% mutate(monthNumeric = as.numeric(month))
testTmp <- test %>% mutate(hourNumeric = as.numeric(hour))
testTmp <- testTmp %>% mutate(monthNumeric = as.numeric(month))


trainLasso <- trainTmp %>% select("colorPcnt","breedPcnt","OutcomeType","AnimalType","NoNameInd","daysOld",
                               "daysOldBinned","pitIndicator","sex","Spayed","mixedBreed","hour","month","blackColorInd","hourNumeric","monthNumeric")

testLasso <- testTmp %>% select("colorPcnt","breedPcnt","OutcomeType","AnimalType","NoNameInd","daysOld",
                               "daysOldBinned","pitIndicator","sex","Spayed","mixedBreed","hour","month","blackColorInd","hourNumeric","monthNumeric")

#convert training data to matrix format
x <- model.matrix(OutcomeType~.,trainLasso)
#convert class to numerical variable
y <- ifelse(trainLasso$OutcomeType=="Positive",1,0)

#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso

# check docs to explore other type.measure options
cv.out <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse" )
#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)

#apply to train
lasso_prob <- predict(cv.out, newx = x,s=lambda_1se,type="response")
lasso_predict <- rep("Negative",nrow(trainLasso))
lasso_predict[lasso_prob>.5] <- "Positive"
conf<-table(pred=lasso_predict,true=trainLasso$OutcomeType)

#Test Model Metrics
  #accuracy
  accuracy <- round(mean(lasso_predict==trainLasso$OutcomeType),4)*100
  accuracy
  #recall
  recall <- (round(conf[2,2]/sum(conf[2,]),4)*100)
  recall
  #specificity
  specificity <- (round(conf[1,1]/sum(conf[1,]),4)*100)
  specificity
  #precision
  precision <- (round(conf[2,2]/sum(conf[,2]),4)*100)
  precision

description <- "Lasso regression train results with numeric versions of hour and month"

newLine <- c(description,accuracy,recall,specificity,precision)
model_results <- rbind(model_results,newLine)



testLasso$hour5 <- 1
testLasso$hour22 <- 1
x_test <- model.matrix(OutcomeType ~ .,testLasso)
#predict class, type="class"
lasso_prob <- predict(cv.out, newx = x_test,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("Negative",nrow(testLasso))
lasso_predict[lasso_prob>.5] <- "Positive"
#confusion matrix

conf<-table(pred=lasso_predict,true=testLasso$OutcomeType)

#Test Model Metrics
  #accuracy
  accuracy <- round(mean(lasso_predict==testLasso$OutcomeType),4)*100
  accuracy
  #recall
  recall <- (round(conf[2,2]/sum(conf[2,]),4)*100)
  recall
  #specificity
  specificity <- (round(conf[1,1]/sum(conf[1,]),4)*100)
  specificity
  #precision
  precision <- (round(conf[2,2]/sum(conf[,2]),4)*100)
  precision

description <- "Lasso regression test results with numeric versions of hour and month"

newLine <- c(description,accuracy,recall,specificity,precision)
model_results <- rbind(model_results,newLine)



# --------------- K-Fold Cross-Validation -------------- #
pet.glm.cv<-
  train(OutcomeType ~ ., data=train, method="glm", family=binomial(),
        trControl=trainControl(method="repeatedCV", repeats = 5, number = 5,
                               savePredictions = T))
pet.glm.cv$results
pet.glm.cv

pet.glm.cv$resample
ggplot(pet.glm.cv$resample, aes(x=Accuracy)) +
  geom_density(alpha=.2, fill="red")

