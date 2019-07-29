setwd("C:/Users/chico/Documents/STAT 304")
require(ggplot2)
require(dplyr)
require(car)
require(MASS)

#Part 1
river<-read.csv("http://math.roanoke.edu//childers//STAT304//kd.csv",header=T)
river<-na.omit(river)

head(river)

#1.
par(mfrow=c(3,3))
hist(river$YSI.Salinity)
hist(river$YSI.pH)
hist(river$YSI.DO)
hist(river$YSI.Chlorophyll)
hist(river$YSI.Turbidity)
hist(river$Depth)
hist(river$YSI.WTemp)

#I'm worried about the variables YSI.DO, YSI.Turbidity, and Depth. YSI.DO may have a problem with outliers.
#YSI.Turbidity and Depth seem to have an outlier. YSI.pH looks interesting around the index of 2000, may not be significant.

#2.
fit.full<-lm(river$Kd~river$YSI.Salinity+river$YSI.pH+river$YSI.DO+river$YSI.Chlorophyll+river$YSI.Turbidity+river$Depth
             +river$YSI.WTemp)
fit.null<-lm(river$Kd~1)
step(fit.null, scope=list(lower=fit.null, upper=fit.full), direction="forward")

#Include all seven variables. Step procedure sees how significant each variable is if added to the model and adds the most
#significant variable, repeating the process until no variable is significant enough to add to the model.

#3.
vif(fit.full)

#VIF measures collinearity. If we see a VIF value above 10, we are concerned with collinearity. For this model, we are not
#concerned with collinearity.

#4.

#Ridge and Lasso regression are different regression methods that we can use if we are concerned with our assumptions and,
#specifically, collinear data. For this problem, it is not appropriate to use either method as our assumptions can be fixed
#with transformations and removing outliers, and we do not have any concerns with collinearity.

#5.

stack<-ls.diag(fit.full)
par(mfrow=c(1,3))
plot(stack$hat)
plot(stack$std.res)
plot(stack$cooks)

################################messing with leverage values###############################
(2*(7+1))/3948
which(stack$hat>((2*(7+1))/3948))
river.rmv<-river[which(stack$hat<=((2*(7+1))/3948)),]
fit.rmv<-fit.full<-lm(river.rmv$Kd~river.rmv$YSI.Salinity+river.rmv$YSI.pH+river.rmv$YSI.DO+river.rmv$YSI.Chlorophyll
                      +river.rmv$YSI.Turbidity+river.rmv$Depth+river.rmv$YSI.WTemp)
par(mfrow=c(3,3))
hist(river.rmv$YSI.Salinity)
hist(river.rmv$YSI.pH)
hist(river.rmv$YSI.DO)
hist(river.rmv$YSI.Chlorophyll)
hist(river.rmv$YSI.Turbidity)
hist(river.rmv$Depth)
hist(river.rmv$YSI.WTemp)

#May be some outliers. You can look at the leverage and Cook's values to identify possible outliers. High leverage or
#Cook's values are indicators. We can simply remove the high values we're worried about from our data set and recreate
#our model with the new data.

############################################################################################################################

# ################################messing with Cook's values#################################
# which(stack$cooks==max(stack$cooks))
# river.rmv<-river[-c(3948),]
# fit.rmv<-fit.full<-lm(river.rmv$Kd~river.rmv$YSI.Salinity+river.rmv$YSI.pH+river.rmv$YSI.DO+river.rmv$YSI.Chlorophyll
#                       +river.rmv$YSI.Turbidity+river.rmv$Depth+river.rmv$YSI.WTemp)
# stack.rmv<-ls.diag(fit.rmv)
# plot(stack.rmv$hat)
# plot(stack.rmv$std.res)
# plot(stack.rmv$cooks)
# ############################################################################################################################

#6.

#Despite some variables being skewed, I'm not worried about any violation of the assumptions. We're not worried about
#collinearity between any variables.

#7.

summary(fit.rmv)

#R^2 of 0.7752. Tells us that these predictors are alright in explaining light attenuation (77% of variation in the response)

############################################################################################################################
#Part 2 (12.4)

fg1<-read.csv('http://math.roanoke.edu/childers/STAT304/fg.csv',header=T)
fg1<-fg1[-c(1),]
fg1$Dist2<-fg1$Distance^2

kick.nfl <- fg1 %>% filter(League == 'NFL')
kick.afl <- fg1 %>% filter(League == 'AFL')

#a.
fit.nfl <- glm(kick.nfl$Made~kick.nfl$Distance+kick.nfl$Dist2,family=binomial)
summary(fit.nfl)
fit.afl <- glm(kick.afl$Made~kick.afl$Distance+kick.afl$Dist2,family=binomial)
summary(fit.afl)

#b.
fit.total <- glm(fg1$Made~fg1$Distance+fg1$Dist2+fg1$Z,family=binomial)
summary(fit.total)

#c.
#Quadratic variable not significant

#d.
kick.nfl$expected<-fitted(fit.nfl)
kick.afl$expected<-fitted(fit.afl)
t.test(kick.nfl$expected,kick.afl$expected)
#evidence that difference in means is not 0