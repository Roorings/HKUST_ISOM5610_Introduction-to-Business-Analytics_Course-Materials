# Preparation
data = read.csv("mobile_churn.csv",sep=',',header=T)
data$Churn = toupper(data$Churn)
data$Churn = as.factor(data$Churn)
data$IntlPlan = as.factor(data$IntlPlan)
summary(data)
train_set = data[1:2665,]
test_set = data[-(1:2665),]
train_set$Churn = as.factor(train_set$Churn)
test_set$Churn = as.factor(test_set$Churn)

# Task 2
## Check Convergence Problem
find_sep = glm(Churn~., family = binomial(link = "logit"), 
               data = train_set, method = "detect_separation")
find_sep
## First Model
model1 = glm(Churn~.-DayMinutes-EveMinutes-NightMin-IntlMin, 
             family = binomial(link = "logit"), data = train_set)
summary(model1)
## Stepwise Selection 
model2 = step(model1)
summary(model2)
## Multicollinearity
vif2 = as.data.frame(vif(model2))
vif2
## Likelihood Ratio Test Compare model1 & model3_2)
model3_2 = glm(formula = Churn ~ IntlPlan + NVMailMsgs + DayCharge + 
                 EveCharges + NightCharge + IntlCalls + IntlCharge + 
                 NCustServiceCalls, family = binomial(link = "logit"), data = train_set)
summary(model3_2)
anova2 = anova(model3_2, model1,test = "Chisq")
flextable(data = anova2)
## Final Model
model3 = glm(formula = Churn ~ IntlPlan + VMPlan + DayCharge + 
               EveCharges + NightCharge + IntlCalls + IntlCharge + 
               NCustServiceCalls, family = binomial(link = "logit"), data = train_set)
summary(model3)
vif3 = as.data.frame(vif(model3))
vif3
### Leverage Point & outliers
glm.diag.plots(model3, glmdiag = glm.diag(model3), subset = NULL,
               iden = FALSE, labels = NULL, ret = FALSE)
outlierTest(model3)
## Likelihood Ratio Test Compare model1 & model3)
anova = anova(model3, model1,test = "Chisq")
flextable(data = anova)

# Task III
## Goodness of Fit
### Likelihood Ratio Test
residualDeviance = model3$deviance
nullDeviance = model3$null.deviance
devianceDiff = nullDeviance - residualDeviance
df = length(model3$coefficients) - 1
p_val = pchisq(devianceDiff, df=df, lower.tail=FALSE)
p_val
### Pseudo R-squared
pseudoRSquared = 1 - (residualDeviance/nullDeviance)
pseudoRSquared
### Deviance Test
df_resi = model3$df.residual
p_val_devi = pchisq(residualDeviance, df=df_resi, lower.tail=FALSE)
p_val_devi

# AccuracyCutoffInfo
AccuracyCutoffInfo <- function( train, test, predict, actual )
{
  # change the cutoff value's range as you please 
  cutoff <- seq( .1, .9, by = .05 )
  
  accuracy <- lapply( cutoff, function(c)
  {
    # use the confusionMatrix from the caret package
    cm_train <- confusionMatrix( as.factor(as.numeric(train[[predict]] > c)), train[[actual]] )
    cm_test  <- confusionMatrix(as.factor(as.numeric(test[[predict]] > c)), test[[actual]])
    
    dt <- data.table( cutoff = c,
                      train  = cm_train$overall[["Accuracy"]],
                      test   = cm_test$overall[["Accuracy"]] )
    return(dt)
  }) %>% rbindlist()
  
  # visualize the accuracy of the train and test set for different cutoff value 
  # accuracy in percentage.
  accuracy_long <- gather( accuracy, "data", "accuracy", -1 )
  
  plot <- ggplot( accuracy_long, aes( cutoff, accuracy, group = data, color = data ) ) + 
    geom_line( size = 1 ) + geom_point( size = 3 ) +
    scale_y_continuous( label = percent ) +
    ggtitle( "Train/Test Accuracy for Different Cutoff" )
  
  return( list( data = accuracy, plot = plot ) )
}
train_set['predict'] = predict(model3,newdata=train_set, type = 'response')
test_set['predict'] = predict(model3,newdata=test_set, type = 'response')
test_set['Churn'] = as.factor(as.numeric(unlist(test_set['Churn']))-1)
train_set['Churn'] = as.factor(as.numeric(unlist(train_set['Churn']))-1)
AccuracyCutoffInfo(train_set, test_set, 'predict', 'Churn')