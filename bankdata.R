install.packages("")
library(ggplot2)
library(gridExtra)

setwd("C:/Users/HS/Documents/GitHub/analsying-bank-data")

bank<-read.csv("bank-additional-full.csv", header = TRUE)
head(bank)
names(bank)


summary(bank)
str(bank)

table(bank$job)
table(bank$marital)
table(bank$education)
table(bank$default)
table(bank$housing)
table(bank$loan)
table(bank$contact)
table(bank$month)
table(bank$day_of_week)
table(bank$duration)
table(bank$campaign)
table(bank$poutcome)
table(bank$y)



table(bank$y)

p0<- ggplot(bank)+geom_bar(aes(age), stat="count")+
  ggtitle("age") +
  xlab("age")
p1<- ggplot(bank)+geom_bar(aes(job), stat="count")+
  ggtitle("job") +
  xlab("job") + coord_flip()
p2<- ggplot(bank)+geom_bar(aes(marital), stat="count")+
  ggtitle("marital") +
  xlab("marital")
p3<- ggplot(bank)+geom_bar(aes(education), stat="count")+
  ggtitle("education") +
  xlab("education") +coord_flip()
p4<- ggplot(bank)+geom_bar(aes(default), stat="count")+
  ggtitle("default") +
  xlab("default")
p5<- ggplot(bank)+geom_bar(aes(housing), stat="count")+
  ggtitle("housing") +
  xlab("housing")
p6<- ggplot(bank)+geom_bar(aes(loan), stat="count")+
  ggtitle("loan") +
  xlab("loan")



p0
grid.arrange(p1,p2,p3,p4, ncol=2)
p5
p6


a<- bank[,c(16:20)]
a

cor(a)
plot(a)

