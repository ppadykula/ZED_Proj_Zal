#CART-clasification and regression tree
#non-continues model
#install.packages("rpart")
library(rpart)

dt_regressor<-rpart(formula = length~.,
                    data=training_set,
                    control = rpart.control(minsplit = 1))