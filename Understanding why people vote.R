#Data set consist of 344,000 voters grouped into four different treatment groups (CivicDuty, HawThorne, Self, Neighbors) based on the message sent to them and a control group who weren't sent any messages. Additionally it includes sex(0 for male, 1 for female), yob(year of birth) and the dependent variable voting(1 if they voted, 0 otherwise).

vote = read.csv("gerber.csv")
str(vote)

summary(vote)

t = table(vote$voting)

#Percentage of population that voted
t[2]/sum(t)

t = table(vote$hawthorne,vote$voting)
t

t = table(vote$civicduty,vote$voting)
t

t = table(vote$neighbors,vote$voting)
t

t = table(vote$self,vote$voting)
t

t = table(vote$control,vote$voting)
t

#neighbors group had largest percentage of population of people who voted
str(vote)

TreatmentLR = lm(voting ~ hawthorne + civicduty + neighbors + self, data = vote)
summary(TreatmentLR)

# all the variables seem to be significant

predLR = predict(TreatmentLR)

summary(predLR)

#Accuracy of Linear Regression model using a threshold of 0.3
t = table(vote$voting, predLR>0.3)
t
sum(diag(t))/sum(t)


#Accuracy of Linear Regression model using a threshold of 0.5
t = table(vote$voting, predLR>0.5)
t
sum(diag(t))/sum(t)


#Baseline prediction
t = table(vote$voting)
t[2]/sum(t)

#AUV value
library(ROCR)
predROC = prediction(predLR, vote$voting)
as.numeric(performance(predROC,'auc')@y.values)

#Event though all the variables are significant it is a weak predictive model as it has low AUC value

#CART model
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = vote, cp=0.0)
prp(CARTmodel)
#From CART tree only 0.3 of civicduty group voted

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data = vote, cp=0.0)
prp(CARTmodel2)

ControlTree = rpart( voting ~ control, data = vote, cp=0.0)
ControlTree2 = rpart( voting ~ control + sex, data = vote, cp=0.0)
prp(ControlTree, digits=6)
#Difference in predicted probability of voting being in control group Vs. voting being in differenct group
abs(0.296638-0.34)

prp(ControlTree2, digits=6)
womenAffected = abs(0.290456-0.334176)
menAffected = abs(0.302795-0.345818)
which.max(c(womenAffected,menAffected))
#women are being affected MORE by not being in control group

#Logistic Regression model
modelLR = glm(voting ~ control + sex, data = vote, family =binomial)
summary(modelLR)
#Coefficient of women is negative, reflecting that women are less likely to vote

#Regression tree calculated the percentage voting exactly for every one of the four possibilities (Man, Not Control), (Man ,Control), (Woman, Not Control) and (Woman, Control). Although logistic regression model has attempted to do the same, it did not perform as well because it can't consider the exact joint probability of being a women and in the control group

#To quantify this, we create a data frame which denotes four possibilites as discussed above in the same order.
Possibilities = data.frame(sex=c(0, 0, 1, 1), control=c(0, 1, 0, 1))
Possibilities
predict(ControlTree2, newdata=Possibilities )

#Difference between Tree and Logistic regression for (Woman, Control) case
abs(0.2904558-0.290456)

#Since the difference is not too big, we are going to add one more interaction variable to our Log reg model

LogModel2 = glm( voting ~ sex + control + sex:control, data = vote, family=binomial)
summary(LogModel2)
#Inspecting the coefficient of new variable we can conclude that, if a person is woman and in the control group, the chances that she voted goes down.

#Average for each group
pred = predict(LogModel2, newdata=Possibilities, type="response")
pred
abs(0.2904558-0.290456)
#This clearly explains trees can capture non-linear relationships can not, but we can get around this sometimes by using a combination of two variables
