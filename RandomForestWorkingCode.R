library(randomForest)
library(tidyverse)
library(caret)
library(doParallel)


## One Year
source("DataCleaning.R")

DonorHistory_Retention_OneYear <- DonorHistoryRetention %>%
  dplyr::select(-c(1,2,4,6:12,15:17,40,50:52)) %>%
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

ret_train <- DonorHistory_Retention_OneYear[sample(1:nrow(DonorHistory_Retention_OneYear), size=100000),]

cl<-makePSOCKcluster(2)

registerDoParallel(cl)
# ctrl <- trainControl(number=5, repeats=1000)
set.seed(314159)
start <- Sys.time()
# forest <- foreach(.combine=combine ) %dopar% {
#   randomForest(OneYearRetention ~ .,
#                data = na.roughfix(ret_train), ntree = 1000, norm.votes = FALSE)
# }
mod.rf <- randomForest(OneYearRetention ~ ., 
                 data = na.roughfix(ret_train), ntree = 1000)
end <- Sys.time()
end - start
stopCluster(cl)
print(mod.rf)

imp1 <- varImp(mod.rf)

## Three year

DonorHistory_Retention_ThreeYear <- DonorHistoryRetention %>%
  dplyr::select(-c(1,2,4,6:12,15:17,40,49,51:52)) %>%
  mutate(Transaction.Type.Code = as.factor(Transaction.Type.Code),
         #Appeal.Code = as.factor(Appeal.Code),
         #Campaign.Project.Code = as.factor(Campaign.Project.Code),
         Designation.School.Unit = as.factor(Designation.School.Unit),
         Designation.Purpose = as.factor(Designation.Purpose),
         ThreeYearRetention = as.factor(ThreeYearRetention))

set.seed(786753)
ind3 <- sample(1:nrow(DonorHistory_Retention_ThreeYear), size=180572)
ret_test3 <- DonorHistory_Retention_ThreeYear[ind,]
ret_train3 <- DonorHistory_Retention_ThreeYear[-ind,]

ret_train3 <- DonorHistory_Retention_ThreeYear[sample(1:nrow(DonorHistory_Retention_ThreeYear), size=100000),]

cl<-makePSOCKcluster(2)

registerDoParallel(cl)
# ctrl <- trainControl(number=5, repeats=1000)
set.seed(359123)
start <- Sys.time()
# forest <- foreach(.combine=combine ) %dopar% {
#   randomForest(OneYearRetention ~ .,
#                data = na.roughfix(ret_train), ntree = 1000, norm.votes = FALSE)
# }
mod.rf3 <- randomForest(ThreeYearRetention ~ ., 
                       data = na.roughfix(ret_train3), ntree = 1000)
end <- Sys.time()
end - start
stopCluster(cl)
print(mod.rf3)

imp3 <- varImp(mod.rf3)


## Three year

DonorHistory_Retention_FiveYear <- DonorHistoryRetention %>%
  dplyr::select(-c(1,2,4,6:12,15:17,40,49,50,52)) %>%
  mutate(Transaction.Type.Code = as.factor(Transaction.Type.Code),
         #Appeal.Code = as.factor(Appeal.Code),
         #Campaign.Project.Code = as.factor(Campaign.Project.Code),
         Designation.School.Unit = as.factor(Designation.School.Unit),
         Designation.Purpose = as.factor(Designation.Purpose),
         FiveYearRetention = as.factor(FiveYearRetention))

set.seed(7867531)
ind5 <- sample(1:nrow(DonorHistory_Retention_FiveYear), size=180572)
ret_test5 <- DonorHistory_Retention_FiveYear[ind,]
ret_train5 <- DonorHistory_Retention_FiveYear[-ind,]

ret_train5 <- DonorHistory_Retention_FiveYear[sample(1:nrow(DonorHistory_Retention_FiveYear), size=100000),]

cl<-makePSOCKcluster(2)

registerDoParallel(cl)
# ctrl <- trainControl(number=5, repeats=1000)
set.seed(35912323)
start <- Sys.time()
# forest <- foreach(.combine=combine ) %dopar% {
#   randomForest(OneYearRetention ~ .,
#                data = na.roughfix(ret_train), ntree = 1000, norm.votes = FALSE)
# }
mod.rf5 <- randomForest(FiveYearRetention ~ ., 
                        data = na.roughfix(ret_train5), ntree = 1000)
end <- Sys.time()
end - start
stopCluster(cl)
print(mod.rf5)

imp5 <- varImp(mod.rf5)


