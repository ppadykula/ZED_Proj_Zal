#Polynomial regression

dataset$addColumn_two=dataset$Column^2
dataset$addColumn_three=dataset$Column^3
polynomial_regres<-lm(formula = Y ~.,
                      data=dataset)

Y_predictions<-predict(polynomial_regres,data.frame(IVColName=Value,
                                                    IVColName_two=Value^2))