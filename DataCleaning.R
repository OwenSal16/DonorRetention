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
  filter(!is.na(Entity.ID))


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
