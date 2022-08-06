##Preparation
attach(BIKESHARE)
str(BIKESHARE)
sum(is.na(BIKESHARE)) #0


##Graphics
plot(as.factor(hr),casual,xlab='hr',ylab='casual')
plot(as.factor(day),casual, xlab='day',ylab='casual')
plot(as.factor(holiday),casual, xlab='holiday',ylab='casual')


modelOriginal=lm(casual~season+hr+holiday+day+weather+temp+feelslike+hum+windspeed)
library(car)
vif(modelOriginal)


###Predict Casual Bike Rentals
model=lm(casual~season+as.factor(hr)+as.factor(holiday)+as.factor(day)+as.factor(weather)+temp+feelslike+hum+windspeed)
summary(model)


##Multicollinearity check
library(car)
vif(model)^2
#it shows that temp and feelslike may be interchangeable

model2=lm(casual~season+as.factor(hr)+holiday+as.factor(day)+as.factor(weather)+temp+hum+windspeed) #removes feelslike
model3=lm(casual~season+as.factor(hr)+holiday+as.factor(day)+as.factor(weather)+feelslike+hum+windspeed) #removes temp
anova(model2, model) #p-value for nested F-test is 0.897, which means model2 is better than model
anova(model3, model) #p-value for nested F-test is 0.021, which means model3 is less useful than model
#thus, model2 is better, we can remove feelslike variable
vif(model2)^2 #check, looks good


##Partial Residual Plot for the term
crPlots(model2)
summary(model2)

model4=lm(casual~season+as.factor(hr)+holiday+as.factor(day)+as.factor(weather)+poly(temp,degree=2)+hum+windspeed) #add curvilinear term for temp
anova(model2,model4) #p-value for nested F-test is 0.46, which means model2 is better than model4

model5=lm(casual~season+as.factor(hr)+holiday+as.factor(day)+as.factor(weather)+temp+poly(hum,degree=2)+windspeed) #add curvilinear term for hum
anova(model2,model5) #p-value for nested F-test is 0.097, which means model2 is better than model5
#so far, model2 is still our best model
crPlots(model2)
influencePlot(model2)


##Qualitative variable
#the class hr_sixToNine and hr_TwentyToTwentythree (a group of hr6-9 and hr20-23) are left out
hr_TenToNinteen=ifelse(hr==10|hr==11|hr==12|hr==13|hr==14|hr==15|hr==16|hr==17|hr==18|hr==19,1,0)

#the season_Spring and season_Fall are left out
season_Summer=ifelse(season=='Summer',1,0)
season_Winter=ifelse(season=='Winter',1,0)

#the class day_MonToFri (a group of day2,3,4,5,6) is left out
day_Sat=ifelse(day==7,1,0)
day_Sun=ifelse(day==1,1,0)

model6=lm(casual~season_Summer+season_Winter+hr_TenToNinteen+holiday+day_Sat+day_Sun+as.factor(weather)+temp+hum+windspeed)
summary(model6)

model7=lm(casual~season_Summer+season_Winter+hr_TenToNinteen+holiday+day_Sat+day_Sun+temp+hum+windspeed)
anova(model7,model6) #p-value for nested F-test is 0.14, so model7 is better
summary(model7)


##Assumption Check
library(car)
crPlots(model7)
influencePlot(model7)
qqnorm(resid(model7))
dwt(model7)
#from the summary, we can find the individual p-value for weather doesn't have significant influence.

