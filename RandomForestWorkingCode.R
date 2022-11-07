library(randomForest)
library(tidyverse)
library(caret)

ctrl <- trainControl(method= "repeatedcv", number=5, repeats=10)
set.seed(314159)
mod.rf <- train(fev ~ height + age + Sex + Smoke_status, 
                data=fev_train,
                method="rf",
                trControl=ctrl,
                tuneGrid=expand.grid(mtry=1:4),
                importance=TRUE)
print(mod.rf)
plot(mod.rf)