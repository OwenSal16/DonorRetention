library(randomForest)
library(tidyverse)
library(caret)

source("DataCleaning.R")

set.seed(7867546)
ind <- sample(1:nrow(DonorHistoryRetention), size=180572)
fev_test <- DonorHistoryRetention[ind,]
fev_train <- DonorHistoryRetention[-ind,]

ctrl <- trainControl(method= "repeatedcv", number=5, repeats=10)
set.seed(314159)
mod.rf <- train(OneYearRetention ~ height + age + Sex + Smoke_status, 
                data=fev_train,
                method="rf",
                trControl=ctrl,
                tuneGrid=expand.grid(mtry=1:4),
                importance=TRUE)
print(mod.rf)
