# assignment-7.3
Use the below given data set
DataSet
Problem- prediction of the number of comments in the upcoming 24 hours on
those blogs, The train data was generated from different base times that may
temporally overlap. Therefore, if you simply split the train into disjoint partitions,
the underlying time intervals may overlap. Therefore, the you should use the
provided, temporally disjoint train and test splits to ensure that the evaluation is
fair.
a. Read the dataset and identify the right features
b. Clean dataset, impute missing values and perform exploratory data analysis.
c. Visualize the dataset and make inferences from that
d. Perform any 3 hypothesis tests using columns of your choice, make conclusions
e. Create a linear regression model to predict the number of comments in the next 24 hours
(relative to basetime)

library(mlbench)
data("BostonHousing")
head(BostonHousing)
model1 <- lm(medv~.,data = BostonHousing)

### Assumption 2.  Average of residuals should be zero
mean(model1$residuals)
round(mean(model1$residuals),1)

### Assumption 3. Homoscedasticity
#par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(model1)


### Assumption 4. Residuals should not be correlated
acf(model1$residuals)
write.csv(model1$residuals,'error.csv')
getwd()

library(lmtest)
dwtest(model1)
#since the p-value < 0.05, we can reject the null hypothesis

### Assumption 5. independent variable should not be correlated with residuals
head(BostonHousing)
cor.test(BostonHousing$crim,model1$residuals)
# p-value > 0.05 do not reject Null Hypothesis
# correlation zero
cor.test(BostonHousing$indus,model1$residuals)
cor.test(BostonHousing$nox,model1$residuals)
cor.test(BostonHousing$rm,model1$residuals)

### Assumption 6. No Multicollinearity
#VIF test
library(car)
vif(model1)

### Assumption 7. Residuals should be normally distributed
plot(model1)
plot(density(model1$residuals))

coefficients(model1) # model coefficients
confint(model1, level=0.95) # CIs for model parameters
anova(model1) # anova table
vcov(model1) # covariance matrix for model parameters
#influence(model1)# regression diagnostics

# Stepwise Regression
library(MASS)
fit <- lm(medv~.,data=BostonHousing)
step <- stepAIC(fit, direction="both")
step$anova # display results

# All Subsets Regression
library(leaps)
attach(BostonHousing)
leaps<-
  regsubsets(medv~.,data=BostonHousing,nbest=10)
# view results
summary(leaps)
# plot a table of models showing variables in each
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size
library(car)
subsets(leaps, statistic="rsq")

#variable importance:
library(caret)
varImp(model1)

#dummy variable regression
head(BostonHousing)

table(BostonHousing$chas)
table(BostonHousing$rad)

## Boxcox Transformation
t<-boxcox(model1)
df = as.data.frame(t)
optimal_lambda = df[which.max(df$y),1]
newdata = cbind(BostonHousing, BostonHousing$medv^optimal_lambda)
names(newdata)[15] = "medv_transf"
model2 <- lm(medv_transf~.-medv,data = newdata)
summary(model2)
