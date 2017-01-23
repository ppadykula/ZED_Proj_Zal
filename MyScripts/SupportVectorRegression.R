#SUPPORT VECTOR REGRESSION

install.packages("e1071")
library(e1071)

svr_regressor<-svm(formula= df$length ~.,
                   data = dataset,
                   type='eps-regression')