library(randomForest)
library(tidyverse)
library(caret)

source("DataCleaning.R")

DonorHistory_Retention_OneYear <- DonorHistoryRetention %>%
  select(-c(1,2,4,6:12,15:17,40,50:52)) %>%
  mutate(Transaction.Type.Code = as.factor(Transaction.Type.Code),
         #Appeal.Code = as.factor(Appeal.Code),
         #Campaign.Project.Code = as.factor(Campaign.Project.Code),
         Designation.School.Unit = as.factor(Designation.School.Unit),
         Designation.Purpose = as.factor(Designation.Purpose),
         OneYearRetention = as.factor(OneYearRetention))

set.seed(7867546)
ind <- sample(1:nrow(DonorHistory_Retention_OneYear), size=180572)
ret_test <- DonorHistory_Retention_OneYear[ind,]
ret_train <- DonorHistory_Retention_OneYear[-ind,]

ret_train <- DonorHistory_Retention_OneYear[sample(1:nrow(DonorHistory_Retention_OneYear), size=10000),]

# ctrl <- trainControl(number=5, repeats=1000)
set.seed(314159)
start <- Sys.time()
mod.rf <- randomForest(OneYearRetention ~ ., 
                data = na.roughfix(ret_train), ntry = 1000)
end <- Sys.time()
end - start
print(mod.rf)
imp <- varImp(mod.rf)
