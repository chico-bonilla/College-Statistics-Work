setwd("C:/Users/chico/Documents/STAT 304")

data<-read.csv("C:/Users/chico/Documents/STAT 304/Test 2 data.csv",header=T)
library(ggplot2)
ggplot(data,aes(x=X,y=Y))+
  geom_point()+
  ggtitle("Effect of New Medication")+
  labs(y="Response",x="Health Indicator")
#a.
fit<-lm(data$Y~data$X+factor(data$Z))
summary(fit)
stack<-ls.diag(fit)
ggplot(data,aes(x=X,y=Y,color=factor(Z)))+
  geom_point()+
  geom_line(aes(y=fitted(fit)))
par(mfrow=c(1,2))
plot(stack$std.res,main="Standardized Residuals")
plot(stack$hat,main="Leverage Values")
which(stack$std.res>3)
data[10,]
data.rmv<-data[-c(10),]
fit.rmv<-lm(data.rmv$Y~data.rmv$X+factor(data.rmv$Z))
summary(fit.rmv)
stack.rmv<-ls.diag(fit.rmv)
plot(stack.rmv$std.res,main="Standardized Residuals")
plot(stack.rmv$hat,main="Leverage Values")
ggplot(data.rmv,aes(x=X,y=Y,color=factor(Z)))+
  geom_point()+
  geom_line(aes(y=fitted(fit.rmv)))+
  labs(title="Effect of New Medication",y="Response",x="Health Indicator")+
  scale_color_manual(name="On New Medication?", 
                     labels = c("No", 
                                "Yes"),
                     values = c('0'="red", 
                                '1'="blue"))


#b.

data.meds<-data.frame("X"=data.rmv$X[data.rmv$Z!='0'],
                      "Z"=data.rmv$Z[data.rmv$Z!='0'],
                      "Y"=data.rmv$Y[data.rmv$Z!='0'])
data.nomeds<-data.frame("X"=data.rmv$X[data.rmv$Z!='1'],
                        "Z"=data.rmv$Z[data.rmv$Z!='1'],
                        "Y"=data.rmv$Y[data.rmv$Z!='1'])

t.test(data.meds$Y,data.nomeds$Y)
#Use 2-sided t-test, p-val >0.05, no evidence of a difference

#c.
# The medication does not seem to have an affect on the response. In essence, what we're looking for is best
# explained simply by the patient's health indicator. However, there seemed to be an outlier in the data given
# to me. Therefore the data point, with a health indicator of 118, was not taken into account when performing
# these tests.

