require(ggplot2)
require(car)
require(MASS)

body<-read.table('http://math.roanoke.edu/childers/STAT304/body.txt',header=T)
attach(body)
names(body)

# Question 1

#waist.girth
#thigh.girth
#chest.girth
fit<-lm(weight~waist.girth+thigh.girth+chest.girth)
summary(fit)
#R-squared: .9032
vif(fit)
#VIF under 10 for all variables
#not concerned about collinearity

fit.ridge<-lm.ridge(weight~waist.girth+thigh.girth+chest.girth,lambda=seq(0,10,.01))
select(fit.ridge)
fit.ridge<-lm.ridge(weight~waist.girth+thigh.girth+chest.girth,lambda=4.25)
fit.ridge
summary(fit)$coefficients

# Question 2
#waist.girth
#thigh.girth
#chest.girth
#shoulder.girth
#bicep.girth
#calf.girth
#forearm.girth
#pelvic.breadth
#height
#hip.girth

# part a
fit.ten<-lm(weight~waist.girth+chest.girth+thigh.girth+shoulder.girth+bicep.girth+calf.girth+
              forearm.girth+pelvic.breadth+height+hip.girth)
summary(fit.ten)

# part b
# remove bicep.girth first

# part c
fit.null<-lm(weight~1)
step(fit.null, scope=list(lower=fit.null, upper=fit.ten), direction="forward")

# part d
step(fit.ten, scope=list(lower=fit.null, upper=fit.ten), direction="backward")

# part e
# waist.girth
# height
# calf.girth
fit.final<-lm(weight~waist.girth+height+calf.girth)
summary(fit.final)
