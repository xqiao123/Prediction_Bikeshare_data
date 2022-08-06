###Predict Registered Bike Rentals
attach(BIKESHARE)
str(BIKESHARE)
newModel=lm(registered~season+as.factor(hr)+holiday+as.factor(day)+as.factor(weather)+temp+feelslike+hum+windspeed)
summary(newModel)
vif(newModel)


##Graphics
plot(as.factor(hr),registered,xlab='hr',ylab='registered')
plot(as.factor(day),registered, xlab='day',ylab='registered')
plot(as.factor(holiday),registered, xlab='holiday',ylab-'registered')


##Multicollinearity check
library(car)
vif(newModel)^2
#it shows that temp and feelslike may be interchangeable

newModel2=lm(registered~season+as.factor(hr)+holiday+as.factor(day)+as.factor(weather)+temp+hum+windspeed) #removes feelslike
newModel3=lm(registered~season+as.factor(hr)+holiday+as.factor(day)+as.factor(weather)+feelslike+hum+windspeed) #removes temp
anova(newModel2,newModel) #p-value for nested F-test is 0.67, which means newModel2 is better than newModel
anova(newModel3,newModel) #p-value for nested F-test is 0.13, which means newModel3 is better than newModel

summary(newModel2)
summary(newModel3)
#we select newModel2 although both of newModel2 and newModel3 are more useful than newModel


##Partial residual plot for the term
library(car)
crPlots(newModel2)

#based on the partial residual plot, we can see a little gap between the blue line and pink line for the variable temp
#so we create newModel4 to add a new curviliar term for the variable temp
newModel4=lm(registered~season+as.factor(hr)+holiday+as.factor(day)+as.factor(weather)+poly(temp,degree=2)+hum+windspeed)
anova(newModel2,newModel4) #p-value for nested F-test is very small, which means newModel4 is better than newModel2
summary(newModel4)
crPlots(newModel4)

newModel5=lm(registered~season+as.factor(hr)+holiday+as.factor(day)+as.factor(weather)+poly(temp,degree=2)+poly(hum,degree=2)+windspeed)
anova(newModel4,newModel5) #p-value for nested F-test is 0.0014, which means newModel5 is better than newModel4
summary(newModel5)
crPlots(newModel5)
#so far, newModel 5 is the better model


##Qualitative variable
#the class hr_SixAndTwentyoneAndTwentythree (hr6,21-23) is left out
hr_SevenToNine=ifelse(hr==7|hr==8|hr==9,1,0)
hr_TenToFifteen=ifelse(hr==10|hr==11|hr==12|hr==13|hr==14|hr==15,1,0)
hr_SixteenToTwenty=ifelse(hr==16|hr==17|hr==18|hr==19|hr==20,1,0)

#the class day_Sun and day_Sat (day1,7) is left out
day_MonToFri=ifelse(day==2|day==3|day==4|day==5|day==6,1,0)

newModel6=lm(registered~hr_SevenToNine+hr_TenToFifteen+hr_SixteenToTwenty+season+holiday+day_MonToFri+poly(temp,2)+poly(hum,2)+windspeed)
summary(newModel6)

newModel7=lm(registered~hr_SevenToNine+hr_TenToFifteen+hr_SixteenToTwenty+season+holiday+day_MonToFri+poly(temp,2,raw=TRUE)+poly(hum,2,raw=TRUE)+windspeed)
summary(newModel7)


##Assumption Check
library(car)
crPlots(newModel6)
influencePlot(newModel6)
qqnorm(resid(newModel6))
dwt(newModel7)
