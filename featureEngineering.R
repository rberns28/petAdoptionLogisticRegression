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
nrow(na.omit(base[,-2]))
