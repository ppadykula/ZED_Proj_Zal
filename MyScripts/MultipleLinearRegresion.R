#Multiple linear regresion
# formula: DV~. means DV depends on all variables that left in dataset
mlregressor<-lm(formula = Y~.,
                data = training_set)

Y_predictions<-predict(mlregressor,newdata=test_set)