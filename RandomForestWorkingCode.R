library(randomForest)
library(tidyverse)
library(caret)

source("DataCleaning.R")

DonorHistory_Retention_OneYear <- DonorHistoryRetention %>%
  select(-c(1,2,4,7,9:12,15:17,40,50:52))

set.seed(7867546)
ind <- sample(1:nrow(DonorHistory_Retention_OneYear), size=180572)
fev_test <- DonorHistory_Retention_OneYear[ind,]
fev_train <- DonorHistory_Retention_OneYear[-ind,]

ctrl <- trainControl(method= "repeatedcv", number=5, repeats=10)
set.seed(314159)
mod.rf <- train(OneYearRetention ~ ., 
                data=fev_train,
                method="rf",
                trControl=ctrl,
                tuneGrid=expand.grid(mtry=1:4),
                importance=TRUE)
print(mod.rf)
