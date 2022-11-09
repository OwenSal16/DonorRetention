### Load in Libraries
library(tidyverse)
library(caret)
library(randomForest)
library(lubridate)
library(ggthemes)


### Read in Data
Rel_Type <- read.csv("1_4_Relationship_Type.csv")
Part_Hist <- read.csv("1_5a_Participation_History.csv")
Addresses <- read.csv("1_2a_Addresses.csv")
Majors <- read.csv("1_3_Degrees.csv")
DH1 <- read.csv("DonorHistoryFirstThree")
DH2 <- read.csv("DonorHistorySecondThree")
DH3 <- read.csv("DonorHistoryThirdThree")
DH4 <- read.csv("DonorHistoryFourthThree")
DH5 <- read.csv("DonorHistoryFifthThree")
DH6 <- read.csv("DonorHistorySixthThree")
DH7 <- read.csv("DonorHistoryLastTwo")
RetentionIndicators <- read.csv("retentionIndicatorsAnnualFund.csv")
ScholarshipRetention <- read.csv("retentionIndicatorsScholarship.csv")
ZipCodes <- read.csv("zip_data.csv")

### Clean retention data
Retention <- RetentionIndicators %>%
  mutate(OneYear = as.numeric(OneYearRetention),
         ThreeYear = as.numeric(ThreeYearRetention),
         FiveYear = as.numeric(FiveYearRetention),
         FiscalYear = year(ymd(Year, truncated = 2))) %>%
  select(!c(OneYearRetention, ThreeYearRetention, FiveYearRetention, Year))

RetentionSchol <- ScholarshipRetention %>%
  mutate(OneYear = as.numeric(OneYearRetention),
         ThreeYear = as.numeric(ThreeYearRetention),
         FiveYear = as.numeric(FiveYearRetention),
         FiscalYear = year(ymd(Year, truncated = 2))) %>%
  select(!c(OneYearRetention, ThreeYearRetention, FiveYearRetention, Year))

RetentionLong <- Retention %>%
  pivot_longer(cols = c(OneYear, ThreeYear, FiveYear), names_to = "RetainedType", values_to = "RetainedCount")

### Fix relationship spreadsheet
Rel_Type_Split <- Rel_Type %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = Relationship.Type, values_from = value)

Rel_Type_Split[is.na(Rel_Type_Split)] <- 0
## https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe%5C


### Fix participation spreadsheet
Part_Hist_Split <- Part_Hist %>%
  select(!c(4:7)) %>%
  mutate(Participation.Category = case_when(Participation.Category == "" ~ "Uncategorized",
                                            TRUE ~ Participation.Category)) %>%
  pivot_wider(names_from = Participation.Category, values_from = Activity.Name, values_fn = list)


### Fix addresses spreadsheet
#select relevant variables
Addresses <- Addresses %>%
  mutate(newZip = str_extract(Zip, "\\d+?(?=-)")) %>%
  select(Entity.ID, newZip, Country) %>%
  rename(Zip = newZip)

#find duplicate Entities
b <- rle(sort(Addresses$Entity.ID))
adCounts <- data.frame(ID=b$values, count=b$lengths)
dupEID <- adCounts %>%
  filter(count > 1)

#subset only duplicates
DupAd <- Addresses %>%
  filter(Entity.ID %in% dupEID$ID)

#remove duplicates from full data
Addresses <- Addresses %>%
  filter(!(Entity.ID %in% dupEID$ID))

#loop through duplicates and combine matches
i = 1
while (i < (nrow(DupAd))) {
  if(DupAd$Entity.ID[i] == DupAd$Entity.ID[i+1]) {
    if(is.na(DupAd$Zip[i]) || is.na(DupAd$Zip[i+1])) {
      if (DupAd$Country[i] == DupAd$Country[i+1]) {
        DupAd <- DupAd[-c(i),]
      }
    } else if(DupAd$Zip[i] == DupAd$Zip[i+1]) {
      DupAd <- DupAd[-c(i),]
    }
  }
  i = i + 1
}

#find remaining duplicates(couldn't be combined)
c <- rle(sort(DupAd$Entity.ID))
adCounts2 <- data.frame(ID=c$values, count=c$lengths)
dupEID2 <- adCounts2 %>%
  filter(count > 1)

#new subset of non-match duplicates
DupAd2 <- DupAd %>%
  filter(Entity.ID %in% dupEID2$ID)

#combined duplicates
DupAd <- DupAd %>%
  filter(!(Entity.ID %in% dupEID2$ID))

#add now combined duplicates to original data
Addresses <- rbind(Addresses, DupAd)



### Fix majors spreadsheet
# Separates majors into respective schools and lists them in new columns
# Year is saved as the most recent (max) year present
Majors_new <- Majors %>%
  select(!c(4)) %>%
  mutate(School.of.Graduation = case_when(School.of.Graduation == "" ~ "No College",
                                          TRUE ~ School.of.Graduation)) %>%
  pivot_wider(id_cols= Entity.ID, names_from = School.of.Graduation, values_from = Major, values_fn = list, unused_fn = max)


### Clean and add data to Donor History
DonorHistory <- cbind(DH1, DH2, DH3, DH4, DH5, DH6, DH7)

remove("DH1", "DH2", "DH3", "DH4", "DH5", "DH6", "DH7")

DonorHistory_Clean <- DonorHistory %>%
  select(!3) %>%
  left_join(Rel_Type_Split) %>%
  left_join(Part_Hist_Split) %>%
  left_join(Addresses) %>%
  left_join(Majors_new, by=c('Entity.ID')) %>%
  left_join(RetentionIndicators, by=c('Entity.ID', 'Fiscal.Year' = 'Year')) %>%
  left_join(ZipCodes, by = c('Zip' = 'zipcode')) %>%
  filter(!is.na(Entity.ID)) %>%
  select(!Zip)


### Create active donor and retention variables
a <- rle(sort(DonorHistory_Clean$Entity.ID))
counts <- data.frame(ID=a$values, count=a$lengths)

retainedAtAll <- counts %>%
  mutate(retainedAtAll = ifelse(count > 1, 1, 0)) %>%
  select(!count)

## https://stackoverflow.com/questions/1923273/counting-the-number-of-elements-with-the-values-of-x-in-a-vector

## Adding variables to new data set
DonorHistoryRetention <- DonorHistory_Clean %>%
  mutate(Active = ifelse(Fiscal.Year %in% c(2022, 2023), 1, 0)) %>%
  left_join(retainedAtAll, by = c("Entity.ID" = "ID"))



DonorHistoryRetention <- DonorHistoryRetention %>%
  mutate(SOROR = ifelse(SOROR == "NULL",0, 1)) %>%
  mutate(ALUEV = ifelse(ALUEV == "NULL",0, 1)) %>%
  mutate(FRTTY = ifelse(FRTTY == "NULL",0, 1)) %>%
  mutate(SRVCE = ifelse(SRVCE == "NULL",0, 1)) %>%
  mutate(UncatAct = ifelse(Uncategorized == "NULL",0, 1)) %>%
  mutate(CLBSP = ifelse(CLBSP == "NULL",0, 1)) %>%
  mutate(SPRTS = ifelse(SPRTS == "NULL",0, 1)) %>%
  mutate(ORGZS = ifelse(ORGZS == "NULL",0, 1)) %>%
  mutate(ADV = ifelse(ADV == "NULL",0, 1)) %>%
  mutate(LEAD = ifelse(LEAD == "NULL",0, 1)) %>%
  mutate(MUSIC = ifelse(MUSIC == "NULL",0, 1)) %>%
  mutate(MILIT = ifelse(MILIT == "NULL",0, 1)) %>%
  mutate(GOVT = ifelse(GOVT == "NULL",0, 1)) %>%
  mutate(REGOF = ifelse(REGOF == "NULL",0, 1)) %>%
  mutate(TRAVL = ifelse(TRAVL == "NULL",0, 1)) %>%
  mutate(STATE = ifelse(STATE == "NULL",0, 1)) %>%
  mutate(REUN = ifelse(REUN == "NULL",0, 1)) %>%
  mutate(CHAPT = ifelse(CHAPT == "NULL",0, 1)) %>%
  mutate(AS = ifelse(AS == "NULL",0, 1)) %>%
  mutate(FA = ifelse(FA == "NULL",0, 1)) %>%
  mutate(BU = ifelse(BU == "NULL",0, 1)) %>%
  mutate(EA = ifelse(EA == "NULL",0, 1)) %>%
  mutate(AP = ifelse(AP == "NULL",0, 1)) %>%
  mutate(IS = ifelse(IS == "NULL",0, 1)) %>%
  mutate(RC = ifelse(RC == "NULL",0, 1)) %>%
  mutate(NoCollege = ifelse(`No College` == "NULL",0, 1)) %>%
  mutate(YearsSinceGrad = Fiscal.Year - Degree.Year) %>%
  select(-c(Uncategorized, `No College`))

DonorHistoryRetention$USindicator <- ifelse(DonorHistoryRetention$Country == '', 1,0)
DonorHistoryRetention$Country <- ifelse(DonorHistoryRetention$Country == '', 'USA', DonorHistoryRetention$Country)
DonorHistoryRetention$StateIndicator <- ifelse(DonorHistoryRetention$state_abbr == 'OH', 1, 0)



## Create retention percentages from indicators
## Function that takes a retention dataset and creates summarized retention percentages
CalcRetPerc <- function(RetData) {
  
  ## One year retention
  year <- c()
  percent1 <- c()
  for(x in 2002:2021) {
    xEntities <- filter(RetData, FiscalYear == x)$Entity.ID
    UniqueRetained <- RetData %>%
      filter(FiscalYear == x+1 & Entity.ID %in% xEntities)
    per <- length(unique(UniqueRetained$Entity.ID)) / length(xEntities)
    percent1 <- c(percent1, per)
    year <- c(year, x)
  }
  OneYearRetPer <- data.frame(year, percent1)
  
  ## Three year Retention
  year <- c()
  percent3 <- c()
  for(x in 2002:2019) {
    xEntities <- filter(RetData, FiscalYear == x)$Entity.ID
    UniqueRetained <- RetData %>%
      filter(FiscalYear %in% c(x+1, x+2, x+3) & Entity.ID %in% xEntities)
    per <- length(unique(UniqueRetained$Entity.ID)) / length(xEntities)
    percent3 <- c(percent3, per)
    year <- c(year, x)
  }
  ThreeYearRetPer <- data.frame(year, percent3)
  
  ## Five Year Retention
  year <- c()
  percent5 <- c()
  for(x in 2002:2017) {
    xEntities <- filter(RetData, FiscalYear == x)$Entity.ID
    UniqueRetained <- RetData %>%
      filter(FiscalYear %in% c(x+1, x+2, x+3, x+4, x+5) & Entity.ID %in% xEntities)
    per <- length(unique(UniqueRetained$Entity.ID)) / length(xEntities)
    percent5 <- c(percent5, per)
    year <- c(year, x)
  }
  FiveYearRetPer <- data.frame(year, percent5)
  
  RetentionDataPercents <- OneYearRetPer %>%
    merge(ThreeYearRetPer, all.x = TRUE) %>%
    merge(FiveYearRetPer, all.x = TRUE)
  
  RetentionDataPercents
}

## Running the function to pull annual fund percents
RetentionPercents <- CalcRetPerc(Retention)

## Changing the data to long format
RetDataPercLong <- RetentionPercents %>%
  pivot_longer(cols = c(percent1, percent3, percent5), names_to = "RetentionType", values_to = "percent")

## Running the function to pull scholarship fund percents
RetentionScholPerc <- CalcRetPerc(RetentionSchol)

## Changing the data to long format
RetDataScholPercLong <- RetentionScholPerc %>%
  pivot_longer(cols = c(percent1, percent3, percent5), names_to = "RetentionType", values_to = "percent")


##### Code below was used to generate the Retention Variables.
##### Generated data has been saved as csv files and saved to the git

# # function takes Entity.ID and finds unique values of Year for when a given person has donated
# findYears <- function(e) {
#   entityYears <- DonorHistory_CLean %>%
#     filter(Entity.ID == e) %>%
#     select(Fiscal.Year) %>%
#     arrange(Fiscal.Year)
#   entityYearsVector = as.vector(entityYears[,1])
#   # Years are returned as a sorted vector
#   entityYearsVector
# }
#
# # function generates a data frame with all unique combinations of Entity.ID and Year with newly assigned retention variables
# RetainedByYear <- function() {
#   uniqueEntity <- unique(DonorHistory_Clean$Entity.ID)
#   fulldata <- data.frame('Entity.ID'=character(), 'Year'= integer(),
#                          'OneYearRetention'= logical(), 'ThreeYearRetention'= logical(), 'FiveYearRetention'= logical())
#   r = 1
#   for (e in uniqueEntity) {
#     entityYearsVector <- unique(findYears(e))
#     # if the year is the first year present in the vector, all retention values must be false
#     for (i in 1:length(entityYearsVector)) {
#       if (i == 1) {
#         OneYear = F
#         ThreeYear = F
#         FiveYear = F
#       }
#       # OneYearRetention requires that the year previous be present in the data
#       else if(entityYearsVector[i] - entityYearsVector[i-1] == 1) {
#         OneYear = T
#         ThreeYear = T
#         FiveYear = T
#       }
#       # ThreeYearRetention requires that any single year within the previous three be present
#       else if (entityYearsVector[i] - entityYearsVector[i-1] <= 3) {
#         ThreeYear = T
#         FiveYear = T
#         OneYear = F
#       }
#       # FiveYearRetention requires that any single year within the previous five be present
#       else if (entityYearsVector[i] - entityYearsVector[i-1] <= 5) {
#         FiveYear = T
#         ThreeYear = F
#         OneYear = F
#       }
#       else {
#         FiveYear = F
#         ThreeYear = F
#         OneYear = F
#       }
#       #combine values and append to full data
#       entYear <- data.frame(Entity.ID = e, Year = entityYearsVector[i], OneYearRetention = OneYear,
#                             ThreeYearRetention = ThreeYear, FiveYearRetention = FiveYear)
#       fulldata <- bind_rows(fulldata, entYear)
#      
#       # row marker to help estimate run time
#       r = r + 1
#       if(r%%10000 == 0) {
#         print(r)
#       }
#     }
#   }
#   fulldata
# }
#
# byYear2 <- RetainedByYear()
# head(byYear)