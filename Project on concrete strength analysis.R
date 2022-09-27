#-----------------------Concrete Strength Analysis (Multiple Linear regression)-----------------------

#Step I -----------------------Reading the data for analysis-----------------------------------------

mydata = read.csv('concrete_data.csv',header = T,sep = ",")

#Step II -----------------to view the head (first 6 rows) of my dataset------------------------------

head(mydata)

# Check for any NAN values in the dataset:

any(is.na(mydata))  # There is no NAN values in dataset

#Step III---------------Create a histogram of output or dependent variable---------------------------

library(RColorBrewer)
library(psych)
# Get the information on data set:
describe(mydata)

# Histogram:

hist(mydata$Strength,breaks=7, col=brewer.pal(3,"Set1"),main="Cement Strength Data")

#Boxplot of dataset:

boxplot(mydata,breaks=7, col=brewer.pal(3,"Set1"),main="Cement Strength Data")

# mean and variance before normalisation:
mean(mydata$Strength)
variance <- (sd(mydata$Strength))^2
variance

# The data was verified and outliers where removed (new data set after outlier removal)

mynewdata = read.csv('concrete_data_remove_outlier.csv',header = T,sep = ",")
hist(mydata$Strength,breaks=7, col=brewer.pal(3,"Set1"),main="Cement Strength Data")
boxplot(mydata,breaks=7, col=brewer.pal(3,"Set1"),main="Cement Strength Data")

mean(mynewdata$Strength)
variance <- (sd(mynewdata$Strength))^2
variance

# Inference: The output concrete strength is approximately normaly distributed with Mean = 34.6
# Variance = 288.7.


#Step IV------------Explore the correlation of variables in dataset----------------------------------

# To draw a  scatter plot for every x with other variable:

plot(mynewdata,col=brewer.pal(3,"Set1"))  # in this case we are not in a position to identify relation visually 

# To find correlation of data set:
crrelation <- cor(mynewdata)

cor.dataframe <- data.frame(crrelation)   # From correation value it is clear that only superplasticizer 
                                          # and water have some relation. whereas all other parameters
                                          # are independent


# exporting the correlation data as a CSV

write.csv(cor.dataframe, file = "strength_corr_project.csv", row.names = FALSE)

# Draw and check the relation with respect to some parameters
plot(mynewdata$Cement,mynewdata$Strength)

#____________________________________________________________________________________________________

#Step V ------------------------ Training ans testing split for regresion----------------------------

## splitting data 

library(caTools)

set.seed(123)
split = sample.split(mynewdata$Strength, SplitRatio = 0.70)

train_data <- subset(mynewdata, split==T)  # Created training data for analysis

test_data <- subset(mynewdata, split==F)   # Created testing data for final verification

# fitting linera regression to training set
regressor1<- lm(formula = Strength ~ Cement + Blast.Furnace.Slag
                + Fly.Ash + Water + Superplasticizer + 
                  Coarse.Aggregate + Fine.Aggregate + Age, data = train_data)

reg1_summ <- summary(regressor1)

reg1_summ

# Interpretation: As per the linear regression output Superplasticizer, Coarse.Aggregate, 
#                 Fine.Aggregate are not significant to predict any variation in cement strength
#                 We will remove and reevaluate the regression model.

regressor2 <- lm(formula = Strength ~ Cement + Blast.Furnace.Slag
                 + Fly.Ash + Water + Age, data = train_data)

reg2_summ <- summary(regressor2)

reg2_summ

## Exporting diffrent multiple Linear regression output to a document file:
library(texreg)
texreg::htmlreg(list(regressor1, regressor2),
                file='Linear regression output.doc')


## predicting the test set result by different modeling output

y_pred1 <- predict(regressor1, newdata= test_data)
y_pred2 <- predict(regressor2, newdata= test_data)


test_data$Predict1 <- y_pred1
test_data$Predict2 <- y_pred2

## exporting testset data with predictions in different models 
write.csv(test_data, "Model_output.csv")


#------------------------------Model validation techniques------------------------------------------------
#Variance Inflation factor:
library(car)
car :: vif(regressor1)    #vif <20 so no collinearity

car :: vif(regressor2)    #vif <20 so no collinearity

library(MASS)

stepAIC(regressor1)        # Our regressor2 is already based on the AIC output factors

#--------------------------------Annova Test for the model--------------------------------------------

anova(regressor2)

# Interpretation : All the parameters are significant 


#-----------------------------Normality check for the data-------------------------------------------

plot(regressor2)

residualPlots(regressor2)

qqPlot(regressor2)

#------------------------------------Deletion Diagnostic---------------------------------------------
influenceIndexPlot(regressor2)

#to find influencers or outliers so that we can delete them
#here 309, 416, 418 data points are outlier
# so we will remove the abovepoints and check the accuracy agian.


#----------------------------------Next Iteration to improve the final model-------------------------
mynewdata1 <- mynewdata[-c(309,416,418,417,689,27,289,311,819,183,10,414),]

set.seed(123)
split = sample.split(mynewdata1$Strength, SplitRatio = 0.70)

train_data1 <- subset(mynewdata1, split==T)  # Created training data for analysis

test_data1 <- subset(mynewdata1, split==F)   # Created testing data for final verification

# fitting linera regression to training set
regressor3<- lm(formula = Strength ~ Cement + Blast.Furnace.Slag
                + Fly.Ash + Water + Age, data = train_data1)

regressor3

summary(regressor3)

#_________________________________Check the final regressor best fit lines---------------------------

plot(regressor3)

residualPlots(regressor3)

qqPlot(regressor3)

car :: vif(regressor3)    #vif <20 so no collinearity

#----------------------------------Annova Final Model------------------------------------------------

anova(regressor3)

#---------------------------------Residual Analysis--------------------------------------------------

pred = fitted(regressor3)

Res = residuals(regressor3)

write.csv(pred,"pred_m.csv") 
write.csv(Res,"Res_m.csv")

# Standardizing Residuals
Std_Res = scale(Res, center = TRUE, scale = TRUE)

write.csv(Std_Res,"Std_Res_m.csv")


## predicting the test set result by different modeling output

y_pred_final <- predict(regressor3, newdata= test_data1)

test_data1$Predict1 <- y_pred_final


## exporting testset data with predictions in different models 
write.csv(test_data1, "Model_output_final.csv")


output = read.csv('Model_output_final.csv',header = T,sep = ",")

X <- output[["X"]]
Predicted <- output[["Predict1"]]
strength <-  output[["Strength"]]

# Create a first line
plot(X, Predicted, type = "S", frame = FALSE, pch = 30, 
     col = "red", xlab = "x", ylab = "y")
# Add a second line
lines(X, strength, pch = 30, col = "blue", type = "S")
# Add a legend to the plot
legend("topright", legend=c("Predicted", "Actual_Strength"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)

accuracy_pred <- cor(Predicted,strength)

accuracy_pred

#-----------------------------------*******THE END******-------------------------------------------#
