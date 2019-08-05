require(oddsratio)
require(mosaic)

#Test 3
#Chico Bonilla

#loading the data
cool<-read.csv("C:\\Users\\chico\\OneDrive\\Documents\\STAT 302\\test3.csv")

#Question 1
cool.varGym <- table(cool$Gym,cool$Varsity.Sport)
prop.test(cool.varGym,alternative="greater")
?prop.test
#prop test gives us p-val of 0.089, not significant

#Question 2
#likely = odds ratio
cool.honorsLib <- table(cool$Honors,cool$Library)
oddsRatio(cool.honorsLib)
#Odds Ratio = 0.64

#Question 3
chisq.test(cool$Hall,cool$Breakfast)
#Chi Squared test gives us p-val of 0.069, not significant

#Question 4
cool.lm <- lm(cool$GPA~cool$Hall)
plot(cool$Hall,cool$GPA,main="Is There a Relationship Between a Student's Residence Hall and Their GPA?",xlab="Residence Hall",ylab="GPA")
anova(cool.lm)
#ANOVA gives us p-val of 0.063, not significant

#Question 5
t.test(cool$GPA,cool$Library,alternative="greater")
plot(as.factor(cool$Library),cool$GPA,main="Is There a Relationship Between Student's Visiting the Library and Their GPA?",xlab="Does the Student Visit the Library More Than 3 Times a Week?",ylab="Student's GPA")
#2 sample t-test gives us p-val of <.001, significant

#Question 6
plot(cool$GPA,cool$Positive.Experience,main="Relationship Between GPA and Positive Experience",xlab="GPA",ylab="Probability of Positive Experience")
cool.GPAPos = glm(Positive.Experience~GPA,family=binomial,cool)
curve(predict(cool.GPAPos,data.frame(GPA=x),type="resp"),add=TRUE)
points(cool$GPA,fitted(cool.GPAPos),pch=20)
summary(cool.GPAPos)
#Positive.Experience explained by GPA
exp(-4.04+1.50*3.0)/(1+exp(-4.04+1.50*3.0))
#0.61

exp(-4.04+1.50*2.5)/(1+exp(-4.04+1.50*2.5))
#0.43

prop.test(cool$Positive.Experience~cool$Gym,alternative="greater")