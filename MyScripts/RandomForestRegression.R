#Random forest regression

#install.packages("randomForest")
library(randomForest)
set.seed(23)
rf_regressor<-randomForest(x=training_set[1:2],
                           y=training_set$length,
                           ntree=10)

Y_predictions<-predict(rf_regressor,data.frame(length=22.12))

