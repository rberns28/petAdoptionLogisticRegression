#### Data Profiling ###
setwd("~/Fordham/Statistical Programming with R/Final Project")
base <- read.csv(file = 'shelterdata.csv',sep = ",",header = T,na.strings="")

library("dplyr")

#--------------- Building Features ---------------#

# Build Date Features
base$DateTime = as.POSIXct(strptime(base$DateTime, "%m/%d/%Y %H:%M"))

# No Name Indicator
base<-mutate(base, NoNameInd = ifelse(is.na(Name), 1, 0))

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
      gsub("[0-9]|[[:space:]]", "", base$AgeuponOutcome)=='day' ~ (as.numeric(gsub("[^0-9]", "", base$AgeuponOutcome))),
    )
  )

# Binned Days Old
base <- base %>%
  mutate(
    daysOldBinned = case_when(
       daysOld < 90 ~ "3 Months",
       daysOld < 180 & daysOld >=90 ~ "6 Months",
       daysOld < 365 & daysOld >=180 ~ "1 Year",
       daysOld < 730 & daysOld >=365 ~ "2 Years",
       daysOld < 1825 & daysOld >= 730 ~ "5 Years",
       daysOld < 3650 & daysOld >=1825 ~ "10 Years",
       daysOld >= 3650 ~ "10+ Years")
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



#---------------------- Count Nulls ----------------------#
apply(base, 2, function(x) print(sum(is.na(x))))
base <- na.omit(base[,-2])

#---------------------- Plotting -------------------------#

factorPlot <-function(df,dataCol) {
df %>% select(OutcomeType,as.factor(dataCol))  %>% 
  ggplot(aes(x=dataCol, fill=OutcomeType)) +
  geom_bar(position = "fill") +
  labs(x = "Feature", y= "Outcome") + 
  coord_flip()
}

ggplot(base,aes(OutcomeType, daysOld)) +
  geom_boxplot()

library(ggthemes)
library(scales)
ggplot(base, aes(OutcomeType, daysOld)) +
  geom_boxplot() + 
  ggtitle("Outcome by Days old") +
  theme_tufte() + 
  scale_y_continuous(labels = comma)

ggplot(base, aes(OutcomeType, as.numeric(hour))) +
  geom_boxplot() + 
  ggtitle("Outcome by Hour") +
  theme_tufte() + 
  scale_y_continuous(labels = comma)

#---------------------- Check for Collinearity -------------------------#


#---------------------- Modeling -------------------------#

# Split into Train and Test
## 80% of the sample size
drops <- c('AnimalID','DateTime','SexuponOutcome','AgeuponOutcome','Breed','Color','numericPart','daysOld','SpayedIndicator')
base_tmp <- base[ , !(names(base) %in% drops)]

smp_size <- floor(0.80 * nrow(base_tmp))
set.seed(123)
train_ind <- sample(seq_len(nrow(base_tmp)), size = smp_size)
train <- base_tmp[train_ind, ]
test <- base_tmp[-train_ind, ]

# Baseline model
glm1 <- glm(OutcomeType ~ . , data=train, family = binomial())
summary(glm1)

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
  specificity <- (round(conf[1,1]/sum(conf[,1]),4)*100)
  precision <- (round(conf[2,2]/sum(conf[,2]),4)*100)
  newLine <- c(description,accuracy,recall,specificity,precision)
  ifelse(exists("model_results") && is.data.frame(get("model_results")),
         model_results <- rbind(model_results,newLine),
         model_results <- data.frame(description,accuracy,recall,specificity,precision,stringsAsFactors=FALSE))
  model_results <<- model_results
}
