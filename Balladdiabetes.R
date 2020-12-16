library(ISLR)
library(rpart)
library(rpart.plot)
library(class)
library(ggplot2)
library(gridExtra)
library(grid)
library(MASS)
library(outliers)
library(glmnet)
require(tree)
require(gridExtra)





#setwd("~/Documents/ActualDocuments/School/Grad/MATH 5900 PIC Math/Project")
setwd("C:/Users/Dono7/Documents/ActualDocuments/School/Grad/MATH 5900 PIC Math/Project")
getwd()
rm(list=ls())
ls()
options(max.print=70000000)

#diabetes<-read.csv("/Users/sethketron/Documents/ActualDocuments/School/Grad/MATH 5900 PIC Math/Project/diabetesdat.csv", header=T)
diabetes<-read.csv("C:/Users/Dono7/Documents/ActualDocuments/School/Grad/MATH 5900 PIC Math/Project/diabetesdat.csv", header=T)
head(diabetes)
set.seed(2000)

diabetes$Urban.or.Rural.dummy<-as.factor(diabetes$Urban.or.Rural.dummy)
diabetes$PosorNeg<-as.factor(diabetes$PosorNeg)
diabetes$HgbA1C.2020.Control<-as.factor(diabetes$HgbA1C.2020.Control)
diabetes<-droplevels(diabetes[!diabetes$HgbA1C.2020.Control == 'None',])
diabetes$Stress.2019.Risk<-factor(diabetes$Stress.2019.Risk, levels=c("Low", "Moderate", "High"))
diabetes$Stress.2020.Risk<-factor(diabetes$Stress.2020.Risk, levels=c("Low", "Moderate", "High"))
diabetes$Total.Number.Coaching.Sessions.2019<-as.factor(diabetes$Total.Number.Coaching.Sessions.2019)

# Dealing with outliers for A1CPercChange variable
y <- diabetes$A1CPercChange
qnt <- quantile(y, probs=c(.25, .75), na.rm = T)
caps <- quantile(y, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(y, na.rm = T)
y[y < (qnt[1] - H)] <- caps[1]
y[y > (qnt[2] + H)] <- caps[2]
diabetes$y<-y

attach(diabetes)
#dim(diabetes)
#View(diabetes)





##########  Models   ###############

index<-sample(1:nrow(diabetes),0.8*nrow(diabetes))
trn<-diabetes[index,]
tst<-diabetes[-index,]





#### LINEAR MODELS

linmod<-lm(Diabetes.Rx.Cost.2019~Coaching.Session.Total.Minutes.FY19, data=diabetes)
summary(linmod)
plot(diabetes$Coaching.Session.Total.Minutes.FY19, diabetes$Diabetes.Rx.Cost.2019, xlab="Total Minutes Spent in Coaching Sessions", ylab="Diabetes Rx Total Cost", main="2019 Total Minutes in Coaching Sessions v. Diabetes Rx Total Cost", cex.main=0.85 )
abline(linmod, col="red")



linmod6<-lm(diabetes$A1CPercChange~diabetes$Coaching.Session.Total.Minutes.FY1920, data=diabetes)
summary(linmod6)
plot(diabetes$Coaching.Session.Total.Minutes.FY1920, diabetes$HgbA1C.2020)



linmod1<-lm(MedicalAdherence.2020~Coaching.Session.Total.Minutes.FY19, data=diabetes)
summary(linmod1)



linmod2<-lm(diabetes$A1CPercChange ~ diabetes$Age + diabetes$HeartDisease.2019 + diabetes$Diabetes.2019 + diabetes$Urban.or.Rural.dummy + diabetes$EmotionalHealth.2019 + diabetes$Cancer.2019 + diabetes$WholeHealthIndex.2019 + diabetes$HealthAwarenessIndex.2019, data=diabetes)
summary(linmod2)



linmod3<-lm(diabetes$Diabetes.Rx.Cost.2019~diabetes$HealthAwarenessIndex.2019+diabetes$EmotionalHealth.2019+diabetes$Age+diabetes$Nutrition.2019+diabetes$WeightManagement.2019, data=diabetes)
summary(linmod3)



linmod4<-lm(diabetes$Diabetes.Rx.Cost.2019 ~ diabetes$WeightManagement.2019, data=diabetes)
summary(linmod4)



linmod5<-lm(diabetes$Diabetes.Rx.Cost.2019 ~ diabetes$A1CPercChange + diabetes$, data=diabetes)
summary(linmod5)



balladlin1<-lm(y ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Urban.or.Rural.dummy==0))
summary(balladlin1)
#pred.vals<-predict(balladlin1, newdata=tst)
#mean((tst[,362]-pred.vals)^2)

balladlin1alt<-lm(A1CPercChange ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Urban.or.Rural.dummy==0))
summary(balladlin1alt)



balladlin2<-lm(y ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Urban.or.Rural.dummy==1))
summary(balladlin2)

balladlin2alt<-lm(A1CPercChange ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Urban.or.Rural.dummy==1))
summary(balladlin2alt)



balladlin3<-lm(y ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Movement == 'Backward'))
summary(balladlin3)

balladlin3alt<-lm(A1CPercChange ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Movement == 'Backward'))
summary(balladlin3alt)



balladlin4<-lm(y ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Movement == 'Forward'))
summary(balladlin4)

balladlin4alt<-lm(A1CPercChange ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Movement == 'Forward'))
summary(balladlin4alt)



balladlin5<-lm(y ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Movement == 'Same'))
summary(balladlin5)

balladlin5alt<-lm(A1CPercChange ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Movement == 'Same'))
summary(balladlin5alt)



summary(Age.Range)
balladlin6<-lm(y ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Age.Range == '20-29'))
summary(balladlin6)



balladlin7<-lm(y ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Age.Range == '30-39'))
summary(balladlin7)



balladlin8<-lm(y ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Age.Range == '40-49'))
summary(balladlin8)



balladlin9<-lm(y ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Age.Range == '50-59'))
summary(balladlin9)



balladlin10<-lm(y ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Age.Range == '60-69'))
summary(balladlin10)



balladlin11<-lm(y ~ Coaching.Session.Total.Minutes.FY19, data=subset(diabetes, Age.Range == '70+'))
summary(balladlin11)





################ DECISION TREES

class.tree<-rpart(HgbA1C.2020.Control ~ Urban.or.Rural.dummy + Age + HeartDisease.2020 + HeartDisease.2019 + Diabetes.2020 + Diabetes.2019 + EmotionalHealth.2020 + EmotionalHealth.2019 + Cancer.2020 + Cancer.2019 + WholeHealthIndex.2020 + WholeHealthIndex.2019 + HealthAwarenessIndex.2020 + HealthAwarenessIndex.2019 + BiometricIndex.2020 + BiometricIndex.2019 + FinancialHealthIndex.2020 + FinancialHealthIndex.2019 + AlcoholUse.2020 + AlcoholUse.2019 + MedicalAdherence.2020 + MedicalAdherence.2019 + Nutrition.2020 + Nutrition.2019 + PhysicalActivity.2020 + PhysicalActivity.2019 + TobaccoUse.2020 + TobaccoUse.2019 + WeightManagement.2020 + WeightManagement.2019 + Depression.2020 + Depression.2019 + Stress.2020 + Stress.2019 + Sleep.2020 + Sleep.2019 + BMI.2020 + BMI.2019 + Coaching.Session.Total.Minutes.FY19 + Net.Pay.Rx.Diabetes.Drugs.July.2018 + Net.Pay.Rx.Diabetes.Drugs.August.2018 + Net.Pay.Rx.Diabetes.Drugs.September.2018 + Net.Pay.Rx.Diabetes.Drugs.October.2018 + Net.Pay.Rx.Diabetes.Drugs.November.2018 + Net.Pay.Rx.Diabetes.Drugs.December.2018 + Net.Pay.Rx.Diabetes.Drugs.January.2019 + Net.Pay.Rx.Diabetes.Drugs.February.2019 + Net.Pay.Rx.Diabetes.Drugs.March.2019 + Net.Pay.Rx.Diabetes.Drugs.April.2019 + Net.Pay.Rx.Diabetes.Drugs.May.2019 + Net.Pay.Rx.Diabetes.Drugs.June.2019 + Net.Pay.Rx.Diabetes.Drugs.July.2019 + Net.Pay.Rx.Diabetes.Drugs.August.2019 + Net.Pay.Rx.Diabetes.Drugs.September.2019 + Net.Pay.Rx.Diabetes.Drugs.October.2019 + Net.Pay.Rx.Diabetes.Drugs.November.2019 + Net.Pay.Rx.Diabetes.Drugs.December.2019 + Net.Pay.Rx.Diabetes.Drugs.January.2020 + Systolic.2020 + Systolic.2019 + Diastolic.2020 + Diastolic.2019 + TotalCholesterol.2020 + TotalCholesterol.2019 + LDL.2020 + LDL.2019 + HDL.2020 + HDL.2019 + Triglyceride.2020 + Triglyceride.2019, data=trn, method="class", maxdepth=4)
plot<-rpart.plot(class.tree)

class.tree.preds<-predict(class.tree,newdata=tst,type='class')

# both lines calculate test error
with(tst, table(class.tree.preds, HgbA1C.2020.Control))
mean(class.tree.preds!=tst$HgbA1C.2020.Control)



class.tree1<-rpart(Age.Range ~ Urban.or.Rural.dummy + HeartDisease.2020 + HeartDisease.2019 + Diabetes.2020 + Diabetes.2019 + EmotionalHealth.2020 + EmotionalHealth.2019 + Cancer.2020 + Cancer.2019 + WholeHealthIndex.2020 + WholeHealthIndex.2019 + HealthAwarenessIndex.2020 + HealthAwarenessIndex.2019 + BiometricIndex.2020 + BiometricIndex.2019 + FinancialHealthIndex.2020 + FinancialHealthIndex.2019 + AlcoholUse.2020 + AlcoholUse.2019 + MedicalAdherence.2020 + MedicalAdherence.2019 + Nutrition.2020 + Nutrition.2019 + PhysicalActivity.2020 + PhysicalActivity.2019 + TobaccoUse.2020 + TobaccoUse.2019 + WeightManagement.2020 + WeightManagement.2019 + Depression.2020 + Depression.2019 + Stress.2020 + Stress.2019 + Sleep.2020 + Sleep.2019 + BMI.2020 + BMI.2019 + Coaching.Session.Total.Minutes.FY19 + Net.Pay.Rx.Diabetes.Drugs.July.2018 + Net.Pay.Rx.Diabetes.Drugs.August.2018 + Net.Pay.Rx.Diabetes.Drugs.September.2018 + Net.Pay.Rx.Diabetes.Drugs.October.2018 + Net.Pay.Rx.Diabetes.Drugs.November.2018 + Net.Pay.Rx.Diabetes.Drugs.December.2018 + Net.Pay.Rx.Diabetes.Drugs.January.2019 + Net.Pay.Rx.Diabetes.Drugs.February.2019 + Net.Pay.Rx.Diabetes.Drugs.March.2019 + Net.Pay.Rx.Diabetes.Drugs.April.2019 + Net.Pay.Rx.Diabetes.Drugs.May.2019 + Net.Pay.Rx.Diabetes.Drugs.June.2019 + Net.Pay.Rx.Diabetes.Drugs.July.2019 + Net.Pay.Rx.Diabetes.Drugs.August.2019 + Net.Pay.Rx.Diabetes.Drugs.September.2019 + Net.Pay.Rx.Diabetes.Drugs.October.2019 + Net.Pay.Rx.Diabetes.Drugs.November.2019 + Net.Pay.Rx.Diabetes.Drugs.December.2019 + Net.Pay.Rx.Diabetes.Drugs.January.2020 + Systolic.2020 + Systolic.2019 + Diastolic.2020 + Diastolic.2019 + TotalCholesterol.2020 + TotalCholesterol.2019 + LDL.2020 + LDL.2019 + HDL.2020 + HDL.2019 + Triglyceride.2020 + Triglyceride.2019 + A1CPercChange, data=trn, method="class", maxdepth = 4)
plot1<-rpart.plot(class.tree1)

class.tree.preds1<-predict(class.tree1,newdata=tst,type='class')

with(tst, table(class.tree.preds1, Age.Range))
mean(class.tree.preds1!=tst$Age.Range)



class.tree2<-rpart(PosorNeg ~ Urban.or.Rural.dummy + Age + HeartDisease.2020 + HeartDisease.2019 + Diabetes.2020 + Diabetes.2019 + EmotionalHealth.2020 + EmotionalHealth.2019 + Cancer.2020 + Cancer.2019 + WholeHealthIndex.2020 + WholeHealthIndex.2019 + HealthAwarenessIndex.2020 + HealthAwarenessIndex.2019 + BiometricIndex.2020 + BiometricIndex.2019 + FinancialHealthIndex.2020 + FinancialHealthIndex.2019 + AlcoholUse.2020 + AlcoholUse.2019 + MedicalAdherence.2020 + MedicalAdherence.2019 + Nutrition.2020 + Nutrition.2019 + PhysicalActivity.2020 + PhysicalActivity.2019 + TobaccoUse.2020 + TobaccoUse.2019 + WeightManagement.2020 + WeightManagement.2019 + Depression.2020 + Depression.2019 + Stress.2020 + Stress.2019 + Sleep.2020 + Sleep.2019 + BMI.2020 + BMI.2019 + Coaching.Session.Total.Minutes.FY19 + Net.Pay.Rx.Diabetes.Drugs.July.2018 + Net.Pay.Rx.Diabetes.Drugs.August.2018 + Net.Pay.Rx.Diabetes.Drugs.September.2018 + Net.Pay.Rx.Diabetes.Drugs.October.2018 + Net.Pay.Rx.Diabetes.Drugs.November.2018 + Net.Pay.Rx.Diabetes.Drugs.December.2018 + Net.Pay.Rx.Diabetes.Drugs.January.2019 + Net.Pay.Rx.Diabetes.Drugs.February.2019 + Net.Pay.Rx.Diabetes.Drugs.March.2019 + Net.Pay.Rx.Diabetes.Drugs.April.2019 + Net.Pay.Rx.Diabetes.Drugs.May.2019 + Net.Pay.Rx.Diabetes.Drugs.June.2019 + Net.Pay.Rx.Diabetes.Drugs.July.2019 + Net.Pay.Rx.Diabetes.Drugs.August.2019 + Net.Pay.Rx.Diabetes.Drugs.September.2019 + Net.Pay.Rx.Diabetes.Drugs.October.2019 + Net.Pay.Rx.Diabetes.Drugs.November.2019 + Net.Pay.Rx.Diabetes.Drugs.December.2019 + Net.Pay.Rx.Diabetes.Drugs.January.2020 + Systolic.2020 + Systolic.2019 + Diastolic.2020 + Diastolic.2019 + TotalCholesterol.2020 + TotalCholesterol.2019 + LDL.2020 + LDL.2019 + HDL.2020 + HDL.2019 + Triglyceride.2020 + Triglyceride.2019, method="class", maxdepth=3)
plot2<-rpart.plot(class.tree2)

class.tree.preds2<-predict(class.tree2,newdata=tst,type='class')

with(tst, table(class.tree.preds2, PosorNeg))
mean(class.tree.preds2!=tst$PosorNeg)



class.tree3<-rpart(Urban.or.Rural ~ Age + HeartDisease.2020 + HeartDisease.2019 + Diabetes.2020 + Diabetes.2019 + EmotionalHealth.2020 + EmotionalHealth.2019 + Cancer.2020 + Cancer.2019 + WholeHealthIndex.2020 + WholeHealthIndex.2019 + HealthAwarenessIndex.2020 + HealthAwarenessIndex.2019 + BiometricIndex.2020 + BiometricIndex.2019 + FinancialHealthIndex.2020 + FinancialHealthIndex.2019 + AlcoholUse.2020 + AlcoholUse.2019 + MedicalAdherence.2020 + MedicalAdherence.2019 + Nutrition.2020 + Nutrition.2019 + PhysicalActivity.2020 + PhysicalActivity.2019 + TobaccoUse.2020 + TobaccoUse.2019 + WeightManagement.2020 + WeightManagement.2019 + Depression.2020 + Depression.2019 + Stress.2020 + Stress.2019 + Sleep.2020 + Sleep.2019 + BMI.2020 + BMI.2019 + Coaching.Session.Total.Minutes.FY19 + Net.Pay.Rx.Diabetes.Drugs.July.2018 + Net.Pay.Rx.Diabetes.Drugs.August.2018 + Net.Pay.Rx.Diabetes.Drugs.September.2018 + Net.Pay.Rx.Diabetes.Drugs.October.2018 + Net.Pay.Rx.Diabetes.Drugs.November.2018 + Net.Pay.Rx.Diabetes.Drugs.December.2018 + Net.Pay.Rx.Diabetes.Drugs.January.2019 + Net.Pay.Rx.Diabetes.Drugs.February.2019 + Net.Pay.Rx.Diabetes.Drugs.March.2019 + Net.Pay.Rx.Diabetes.Drugs.April.2019 + Net.Pay.Rx.Diabetes.Drugs.May.2019 + Net.Pay.Rx.Diabetes.Drugs.June.2019 + Net.Pay.Rx.Diabetes.Drugs.July.2019 + Net.Pay.Rx.Diabetes.Drugs.August.2019 + Net.Pay.Rx.Diabetes.Drugs.September.2019 + Net.Pay.Rx.Diabetes.Drugs.October.2019 + Net.Pay.Rx.Diabetes.Drugs.November.2019 + Net.Pay.Rx.Diabetes.Drugs.December.2019 + Net.Pay.Rx.Diabetes.Drugs.January.2020 + Systolic.2020 + Systolic.2019 + Diastolic.2020 + Diastolic.2019 + TotalCholesterol.2020 + TotalCholesterol.2019 + LDL.2020 + LDL.2019 + HDL.2020 + HDL.2019 + Triglyceride.2020 + Triglyceride.2019, method="class", maxdepth=4)
plot3<-rpart.plot(class.tree3, under = TRUE, cex=0.75)

class.tree.preds3<-predict(class.tree3,newdata=tst,type='class')

with(tst, table(class.tree.preds3, Urban.or.Rural))
mean(class.tree.preds3!=tst$Urban.or.Rural)



class.tree4<-rpart(Urban.or.Rural ~ Coaching.Session.Total.Minutes.FY19 + , method="class", maxdepth=4, data=trn)
plot4<-rpart.plot(class.tree4, under = TRUE, cex=0.75)

class.tree.preds4<-predict(class.tree4,newdata=tst,type='class')

with(tst, table(class.tree.preds4, Urban.or.Rural))
mean(class.tree.preds4!=tst$Urban.or.Rural)



class.tree5<-rpart(Urban.or.Rural ~ HeartDisease.2020 + HeartDisease.2019 + Diabetes.2020 + Diabetes.2019 + EmotionalHealth.2020 + EmotionalHealth.2019 + Cancer.2020 + Cancer.2019  + AlcoholUse.2020 + AlcoholUse.2019 + MedicalAdherence.2020 + MedicalAdherence.2019 + Nutrition.2020 + Nutrition.2019 + PhysicalActivity.2020 + PhysicalActivity.2019 + TobaccoUse.2020 + TobaccoUse.2019 + WeightManagement.2020 + WeightManagement.2019 + Depression.2020 + Depression.2019 + Stress.2020 + Stress.2019 + Sleep.2020 + Sleep.2019, method="class", maxdepth=2, data=trn)
plot5<-rpart.plot(class.tree5, under = TRUE, cex=0.75)

class.tree.preds5<-predict(class.tree4,newdata=tst,type='class')

with(tst, table(class.tree.preds5, Urban.or.Rural))
mean(class.tree.preds5!=tst$Urban.or.Rural)





##### LOGISTIC REGRESSION

# Increase (1), Decrease (0)
log.mod<-glm(PosorNeg ~ HeartDisease.2020 + Diabetes.2020 + EmotionalHealth.2020 + Cancer.2020 + AlcoholUse.2020 + MedicalAdherence.2020 + Nutrition.2020 + PhysicalActivity.2020 + TobaccoUse.2020 + WeightManagement.2020 + Depression.2020 + Stress.2020 + Sleep.2020 + HeartDisease.2019 + Diabetes.2019 + EmotionalHealth.2019 + Cancer.2019 + AlcoholUse.2019 + MedicalAdherence.2019 + Nutrition.2019 + PhysicalActivity.2019 + TobaccoUse.2019 + WeightManagement.2019 + Depression.2019 + Stress.2019 + Sleep.2019 + Coaching.Session.Total.Minutes.FY19, data=trn, family=binomial)
summary(log.mod)

log.probs<-predict(log.mod, tst$PosorNeg)
log.probs

log.preds<-ifelse(log.probs > 0.5, "Increase", "Decrease")
log.preds

# test error of predictions
tst_tab<-table(log.preds, diabetes$PosorNeg)
tst_tab
error<- (tst_tab[1,2]+tst_tab[2,1])/sum(tst_tab)
error



# Increase (1), Decrease (0)
log.mod1<-glm(PosorNeg ~  Age + HeartDisease.2020 + HeartDisease.2019 + Diabetes.2020 + Diabetes.2019 + EmotionalHealth.2020 + EmotionalHealth.2019 + Cancer.2020 + Cancer.2019 + WholeHealthIndex.2020 + WholeHealthIndex.2019 + HealthAwarenessIndex.2020 + HealthAwarenessIndex.2019 + BiometricIndex.2020 + BiometricIndex.2019 + FinancialHealthIndex.2020 + FinancialHealthIndex.2019 + AlcoholUse.2020 + AlcoholUse.2019 + MedicalAdherence.2020 + MedicalAdherence.2019 + Nutrition.2020 + Nutrition.2019 + PhysicalActivity.2020 + PhysicalActivity.2019 + TobaccoUse.2020 + TobaccoUse.2019 + WeightManagement.2020 + WeightManagement.2019 + Depression.2020 + Depression.2019 + Stress.2020 + Stress.2019 + Sleep.2020 + Sleep.2019 + BMI.2020 + BMI.2019 + Coaching.Session.Total.Minutes.FY19 + Net.Pay.Rx.Diabetes.Drugs.July.2018 + Net.Pay.Rx.Diabetes.Drugs.August.2018 + Net.Pay.Rx.Diabetes.Drugs.September.2018 + Net.Pay.Rx.Diabetes.Drugs.October.2018 + Net.Pay.Rx.Diabetes.Drugs.November.2018 + Net.Pay.Rx.Diabetes.Drugs.December.2018 + Net.Pay.Rx.Diabetes.Drugs.January.2019 + Net.Pay.Rx.Diabetes.Drugs.February.2019 + Net.Pay.Rx.Diabetes.Drugs.March.2019 + Net.Pay.Rx.Diabetes.Drugs.April.2019 + Net.Pay.Rx.Diabetes.Drugs.May.2019 + Net.Pay.Rx.Diabetes.Drugs.June.2019 + Net.Pay.Rx.Diabetes.Drugs.July.2019 + Net.Pay.Rx.Diabetes.Drugs.August.2019 + Net.Pay.Rx.Diabetes.Drugs.September.2019 + Net.Pay.Rx.Diabetes.Drugs.October.2019 + Net.Pay.Rx.Diabetes.Drugs.November.2019 + Net.Pay.Rx.Diabetes.Drugs.December.2019 + Net.Pay.Rx.Diabetes.Drugs.January.2020 + Systolic.2020 + Systolic.2019 + Diastolic.2020 + Diastolic.2019 + TotalCholesterol.2020 + TotalCholesterol.2019 + LDL.2020 + LDL.2019 + HDL.2020 + HDL.2019 + Triglyceride.2020 + Triglyceride.2019, data=trn, family=binomial)
summary(log.mod1)

log.probs1<-predict(log.mod1, tst$PosorNeg)
log.probs1

log.preds1<-ifelse(log.probs1 > 0.5, "Increase", "Decrease")
log.preds1

tst_tab1<-table(log.preds1, diabetes$PosorNeg)
tst_tab1
error1<- (tst_tab1[1,2]+tst_tab1[2,1])/sum(tst_tab1)
error1





######    LDA MODELS

lda.mod<-lda(HgbA1C.2020.Control ~ Sleep.2020 + Depression.2020 + HeartDisease.2020 + EmotionalHealth.2020 + Cancer.2020 + HealthAwarenessIndex.2020 + AlcoholUse.2020 + Nutrition.2020 + PhysicalActivity.2020 + WeightManagement.2020 + TobaccoUse.2020 ,data=trn)
lda.mod

pred.lda<-predict(lda.mod, tst)

table(predicted=pred.lda$class,actual=tst$HgbA1C.2020.Control)
mean(pred.lda$class!=tst$HgbA1C.2020.Control)





###########   Plots   ################

#par(mfrow = c(1, 2))

plot(diabetes$Age.Range, diabetes$Stress.2019, ylim =c(0, 200), xlab="Age Range", ylab="Stress Risk Index", main="2019 Stress Risk Score by Age Range", col.main="darkblue", col.lab="darkblue")



plot(diabetes$Age.Range, diabetes$Stress.2020, ylim=c(0, 200), xlab="Age Range", ylab="Stress Risk Index", main="2020 Stress Risk Score by Age Range", col.main="darkblue", col.lab="darkblue")



plot(diabetes$Stress.2019.Risk, diabetes$Sleep.2019, xlab="Stress Risk Level", ylab="Sleep Risk Index", main="2019 Sleep Risk Index by Stress Risk Level", col.main="darkblue", col.lab="darkblue")



plot(diabetes$Stress.2020.Risk, diabetes$Sleep.2020, xlab="Stress Risk Level", ylab="Sleep Risk Index", main="2020 Sleep Risk Index by Stress Risk Level", col.main="darkblue", col.lab="darkblue")



plot(diabetes$Sleep.2019, diabetes$HgbA1C.2019)



plot(diabetes$Coaching.Session.Total.Minutes.FY19, diabetes$MedicalAdherence.2019, xlab="Coaching Sessions Total Minutes", ylab="Medical Adherence Risk Index", main="2019 Coaching Sessions v. 2019 Medical Adherence", col.main="darkblue", col.lab="darkblue", cex.main=0.9)
abline(linmod, col="red")



plot(diabetes$Coaching.Session.Total.Minutes.FY19, diabetes$MedicalAdherence.2020, xlab="Coaching Sessions Total Minutes", ylab="Medical Adherence Risk Index", main="2019 Coaching Sessions v. 2020 Medical Adherence", col.main="darkblue", col.lab="darkblue", cex.main=0.9)
abline(linmod1, col="red")



plot(diabetes$Age.Range, diabetes$Diabetes.Rx.Cost.2019)



plot(diabetes$HealthAwarenessIndex.2019, diabetes$Diabetes.Rx.Cost.2019)



plot(diabetes$WeightManagement.2019, diabetes$Diabetes.Rx.Cost.2019)
abline(linmod4, col="red")



plot(diabetes$PosorNeg, diabetes$Diabetes.Rx.Cost.2019)



plot(diabetes$HgbA1C.2019.Control, diabetes$Diabetes.Rx.Cost.2019)



plot(diabetes$Total.Number.Coaching.Sessions.2019, diabetes$Diabetes.Rx.Cost.2019)



labplot<-ggplot(diabetes) + aes(diabetes$HgbA1C.2019, diabetes$Diabetes.Rx.Cost.2019) + geom_point(aes(colour=diabetes$Total.Number.Coaching.Sessions.2019))
labplot
  


labplot1<-ggplot(diabetes) + aes(diabetes$A1CPercChange, diabetes$Diabetes.Rx.Cost.2019) + geom_point(aes(colour=diabetes$Urban.or.Rural))
labplot1



labplot2<-ggplot(diabetes) + aes(diabetes$Age, diabetes$Diabetes.Rx.Cost.Part2019) + ylim(0, 30000) + geom_point(aes(colour=diabetes$Age.Range)) + labs(colour = "Age Range", x = "Age", y = "Total Cost July-December 2019")
labplot2



labplot3<-ggplot(diabetes) + aes(diabetes$Age, diabetes$Diabetes.Rx.Cost.2018) + ylim(0, 30000) + geom_point(aes(colour=diabetes$Age.Range)) + labs(colour = "Age Range", x = "Age", y = "Total Cost July-December 2018")
labplot3
grid.arrange(labplot3, labplot2, ncol=2, top = textGrob("Diabetes Rx Total Cost (U.S. Dollars) By Year and Age Range",gp=gpar(fontsize=20,font=3)))



labplot4<-ggplot(diabetes) + aes(diabetes$Age, diabetes$Rx.Cost.2018) + geom_point(aes(colour=diabetes$Age.Range)) + labs(colour = "Age Range", x = "Age", y = "Rx Total Cost July-December 2018")
labplot4



labplot5<-ggplot(diabetes) + aes(diabetes$Age, diabetes$Rx.Cost.Part2019) + geom_point(aes(colour=diabetes$Age.Range)) + labs(colour = "Age Range", x = "Age", y = "Rx Total Cost July-December 2019")
labplot5
grid.arrange(labplot4, labplot5, ncol=2, top = textGrob("Rx Total Cost By Year and Age Range",gp=gpar(fontsize=20,font=3)))



labplot6<-ggplot(diabetes) + aes(diabetes$Age, diabetes$Avoidable.Admits.Diab.2018) + geom_point(aes(colour=diabetes$Age.Range))
labplot6



labplot7<-ggplot(diabetes) + aes(diabetes$Age, diabetes$Avoidable.Admits.Diab.Part2019) + geom_point(aes(colour=diabetes$Age.Range))
labplot7
grid.arrange(labplot6, labplot7, ncol=2)



labplot8<-ggplot(diabetes) + aes(diabetes$HgbA1C.2020, diabetes$A1CPercChange) + geom_boxplot(aes(colour=diabetes$HgbA1C.2020.Control)) + labs(colour = "A1C Control Group 2020", x = "A1C Level 2020", y = "Percent Change in A1C 19-20", title="A1C Level 2020 v. Percent Change from 2019-2020")
labplot8



labplot9<-ggplot(diabetes) + aes(diabetes$Age, diabetes$A1CPercChange) + geom_point(aes(colour=diabetes$Age.Range)) + labs(colour = "Age Range", x = "Age", y = "Percent Change in A1C 19-20", title="Age v. A1C Percent Change from 2019-2020")
labplot9



labplot10<-ggplot(diabetes) + aes(diabetes$FinancialHealthIndex.2019, diabetes$A1CPercChange) + geom_point(aes(colour=diabetes$PosorNeg))
labplot10



labplot11<-ggplot(diabetes) + aes(diabetes$FinancialHealthIndex.2019, diabetes$A1CPercChange) + geom_point(aes(colour=diabetes$Movement.Group))
labplot11



labplot10<-ggplot(diabetes) + aes(Member.ID, Diabetes.Rx.Cost.Total.Program) + geom_point(aes(colour=diabetes$Movement))
labplot10



labplot11<-ggplot(diabetes) + aes(Coaching.Session.Total.Minutes.FY19, y) + geom_point(aes(colour=Urban.or.Rural)) + geom_abline(intercept = -1.30961, slope = 0.01290, color="darkturquoise") + geom_abline(intercept = 1.34137, slope = -0.02461, color="brown2") + labs(colour = "Urban or Rural", x = "Coaching Session Total Minutes FY19", y = "Percent Change in A1c 19-20 (w/ Outliers Capped)", title="Coaching Total Minutes v. A1c Percent Change w/ Lines of Best Fit")
labplot11



labplot12<-ggplot(diabetes) + aes(Coaching.Session.Total.Minutes.FY19, A1CPercChange) + geom_point(aes(colour=Urban.or.Rural)) + geom_abline(intercept = -1.30961, slope = 0.01290, color="darkturquoise") + geom_abline(intercept = 1.34137, slope = -0.02461, color="brown2") + labs(colour = "Urban or Rural", x = "Coaching Session Total Minutes FY19", y = "Percent Change in A1C 19-20", title="Coaching Total Minutes v. A1C Percent Change w/ Lines of Best Fit")
labplot12



labplot13<-ggplot(diabetes) + aes(Coaching.Session.Total.Minutes.FY19, y) + geom_point(aes(colour=Movement)) + geom_abline(intercept = 1.34927, slope = -0.01575, color="steelblue4") + geom_abline(intercept = -11.98937, slope = -0.01330, color="chartreuse4") + geom_abline(intercept = 14.51196, slope = -0.01563, color="orangered2")+ labs(colour = "Control Movement", x = "Coaching Session Total Minutes FY19", y = "Percent Change in A1c 19-20 (w/ Outliers Capped", title="Coaching Total Minutes v. A1c Percent Change w/ Lines of Best Fit")
labplot13



labplot14<-ggplot(diabetes) + aes(Coaching.Session.Total.Minutes.FY19, A1CPercChange) + geom_point(aes(colour=Movement)) + geom_abline(intercept = 1.34927, slope = -0.01575, color="steelblue4") + geom_abline(intercept = -11.98937, slope = -0.01330, color="chartreuse4") + geom_abline(intercept = 14.51196, slope = -0.01563, color="orangered2")+ labs(colour = "Control Movement", x = "Coaching Session Total Minutes FY19", y = "Percent Change in A1C 19-20", title="Coaching Total Minutes v. A1C Percent Change w/ Lines of Best Fit")
labplot14



labplot15<-ggplot(diabetes) + aes(Coaching.Session.Total.Minutes.FY19, y) + geom_point(aes(colour=Age.Range)) + geom_abline(intercept = 25.0925, slope = -0.3549, color="brown2") + geom_abline(intercept = -5.14388, slope = 0.05459, color="darkgoldenrod4") + geom_abline(intercept = -0.88214, slope = -0.02979, color="springgreen4") + geom_abline(intercept = -0.21809, slope = 0.01386, color="mediumturquoise") + geom_abline(intercept = 2.03273, slope = -0.03147, color="steelblue4") + geom_abline(intercept = 0.77515, slope = -0.04734, color="deeppink2") + labs(colour = "Age Range", x = "Coaching Session Total Minutes FY19", y = "Percent Change in A1C 19-20 (w/ Outliers Capped", title="Coaching Total Minutes v. A1C Percent Change w/ Lines of Best Fit")
labplot15



labplot16<-ggplot(diabetes) + aes(Coaching.Session.Total.Minutes.FY19, A1CPercChange) + geom_point(aes(colour=Age.Range)) + geom_abline(intercept = 25.0925, slope = -0.3549, color="brown2") + geom_abline(intercept = -5.14388, slope = 0.05459, color="darkgoldenrod4") + geom_abline(intercept = -0.88214, slope = -0.02979, color="springgreen4") + geom_abline(intercept = -0.21809, slope = 0.01386, color="mediumturquoise") + geom_abline(intercept = 2.03273, slope = -0.03147, color="steelblue4") + geom_abline(intercept = 0.77515, slope = -0.04734, color="deeppink2") + labs(colour = "Age Range", x = "Coaching Session Total Minutes FY19", y = "Percent Change in A1C 19-20", title="Coaching Total Minutes v. A1C Percent Change w/ Lines of Best Fit")
labplot16



labplot222<-ggplot(diabetes, aes(x=Age, y=A1CPercChange, color=Age.Range)) + geom_boxplot() + labs(colour = "Age Range", x = "Age", y = "Percent Change in A1C 19-20", title="Age v. A1C Percent Change from 2019-2020")
labplot222



labplot17<-ggplot(diabetes) + aes(HgbA1C.2020, A1CChange) + geom_point(aes(colour=diabetes$HgbA1C.2020.Control))
labplot17
