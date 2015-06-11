library(coefplot)
housing=read.csv("housing.csv")

names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", "SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value", "ValuePerSqFt", "Boro")




##removing na's and outliers
housing=na.omit(housing)
housing=housing[-which(housing$Units>1000),]


##scatterplot of numerical variables with output variables and corresponding linear fit
housing_scatter=housing[,c(3,5,12)]
ggplot(data = housing_scatter, aes(x=Units, y=ValuePerSqFt)) + 
  geom_point(shape=1) + geom_smooth(method=lm)
ggplot(data = housing_scatter, aes(x=SqFt, y=ValuePerSqFt)) + 
  geom_point(shape=1) + geom_smooth(method=lm)

##scatterplot of log(numerical variables) with output variables and corresponding linear fit
housing_scatter[,c(1,2)]=log(housing_scatter[,c(1,2)])
ggplot(data = housing_scatter, aes(x=Units, y=ValuePerSqFt)) + 
  geom_point(shape=1) + geom_smooth(method=lm)
ggplot(data = housing_scatter, aes(x=SqFt, y=ValuePerSqFt)) + 
  geom_point(shape=1) + geom_smooth(method=lm)


##distribution of categorical variables (Boro) compared to output (Market.Value.per.SqFt)
ggplot(housing, aes(x = ValuePerSqFt)) + geom_histogram(binwidth = 10) + labs( x = "Value Per Square Fit")
ggplot(housing, aes(x = ValuePerSqFt, fill = Boro)) + geom_histogram(binwidth = 10) + labs( x = "Value Per Square Fit")
ggplot(housing, aes(x = ValuePerSqFt, fill = Boro)) + geom_histogram(binwidth = 10) + labs( x = "Value Per Square Fit") + facet_wrap(~Boro)



housing1=housing

##model buliding for regression

model1=lm(ValuePerSqFt~Units + SqFt+ Boro,data=housing1)
summary(model1)

model2=lm(ValuePerSqFt~I(log(Units))+ I(log(SqFt))+ Boro,data=housing1)
summary(model2)

model3 <- lm(ValuePerSqFt ~ Units + SqFt  + Income , data = housing1)
summary(model3)

model4 <- lm(ValuePerSqFt ~ Units + SqFt + Units*SqFt + Boro, data = housing1)
summary(model4)

model5 <- lm(ValuePerSqFt ~ I(log(Units)) + I(log(SqFt)) + I(Units/SqFt) + Boro, data = housing1)
summary(model5)


anova(model1,model2,model3,model4,model5)
AIC(model1,model2,model3,model4,model5)
BIC(model1,model2,model3,model4,model5)
best_model=model4

##residual plot
p=ggplot(aes(x=.fitted,y=.resid),data=best_model)+geom_point()+geom_hline(yintercept = 0) + geom_smooth(se = FALSE) + labs( x = "Fitted Value", y = "Residuals")
p+geom_point(aes(color=Boro))##coefficient plot
coefplot(best_model,cex.var=0.6,intercept=F)
summary(best_model)



##loading test data
test_data=read.csv("housingNew.csv")
names(test_data)=names(housing)

##prediction output
p=predict(best_model,test_data)
##comparing mean squared error of test and train set

error_train=mean(best_model$residuals^2)  ##MSE of train set
error_test=mean((test_data[,12]-p)^2)  ##MSE of test set

##plotting fitted and actual values
plot(p,type="l",col="red",ylab=NA)
par(new=TRUE)
plot(test_data[,12],type="l",yaxt="n",col="green",ylab="values",main="Predicted vs Actual values")
legend('topright',c("actual values","predicted values"),lty=1,col=c("green","red"),bty='n', cex=.75)