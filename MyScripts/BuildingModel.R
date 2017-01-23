#Building model regression

# W przypadku jakich algorytmów się to stosuje?
# Jak się dobiera zmienne? Czy tylko linear regression?
# Methods of building model:
# 1.All-in
# 2.Forward selection -STEPWISE REGRESSION
# 3.Backword elimination -STEPWISE REGRESSION
# 4.Bidrirectional elimination -STEPWISE REGRESSION
# 5.Score comparison

#All-in
#When you have to or you know 

# 2.Forward selection -STEPWISE REGRESSION
# Check models y~xn and choose variable with the lowest p-Value
# to the chosen add create models with all possible variables and choose again the one with the lowest
# p-value. If p-value<SL continue else finish (there is no point adding another variable)

# 3.Backword elimination -STEPWISE REGRESSION
#choose variable with the highest p-Value 
#if p-value > SL remove the predictor and rebuilt the model else finish

# 4.Bidrirectional elimination -STEPWISE REGRESSION
# set SLENTER and SLSTAY
# Forward selection P-value<SLENTER to enter
# Backword elimination P-value<SLSTAY to stay
# No new variables can enter and no variables can leave-ready model

# 5.Score comparison
#Calculate all possible models (combinations in total: 2^n-1) against chosen criterion of comparison (Akaike,R-squered ,,,,)

cor(data, use="complete.obs", method="pearson") 

fit <- arima(AirPassengers, order=c(1,0,0), list(order=c(2,1,0), period=12))

fore <- predict(fit, n.ahead=24)
 # error bounds at 95% confidence level
  
U <- fore$pred + 2*fore$se
  
L <- fore$pred – 2*fore$se
