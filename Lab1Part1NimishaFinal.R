#TEAM MEMBER1- NIMISHA PHILIP RODRIGUES
#TEAM MEMBER2- MANASI SAMIR TAMBOLI

#Problem 1
sales1<-c(12,14,16,29,30,45,19,20,16, 19, 34, 20)
sales2<-rpois(12,34) # random numbers, Poisson distribution, mean at 34, 12 numbers
par(bg="cornsilk")
plot(sales1, col="blue", type="o", ylim=c(0,100), xlab="Month", ylab="Sales" )
title(main="Sales by Month")
lines(sales2, type="o", pch=22, lty=2, col="red")
grid(nx=NA, ny=NULL)
legend("topright", inset=.05, c("Sales1","Sales2"), fill=c("blue","red"), horiz=TRUE)


#Problem2
sales<-read.table(file.choose(), header=T)
sales # to verify that data has been read
barplot(as.matrix(sales), main="Sales Data", ylab= "Total",beside=T, col=rainbow(5))

#Problem3
fn<-boxplot(sales,col=c("orange","green"))$stats
text(1.45, fn[3,2], paste("Median =", fn[3,2]), adj=0, cex=.7)
text(0.45, fn[3,1],paste("Median =", fn[3,1]), adj=0, cex=.7)
grid(nx=NA, ny=NULL)fn<-boxplot(sales,col=c("orange","green"))$stats

#Problem4

fb1<-read.csv(file.choose())
aapl1<-read.csv(file.choose())

par(bg="cornsilk")
plot(aapl1$Adj.Close, col="blue", type="o", ylim=c(0,100), xlab="Days", ylab="Price" )
lines(fb1$Adj.Close, type="o", pch=22, lty=2, col="red")
legend("topright", inset=.05, c("Apple","Facebook"), fill=c("blue","red"), horiz=TRUE)

hist(aapl1$Adj.Close, col=rainbow(8))

#Problem5
data()
attach(fb1)
head(fb1)
summary(fb1)
library(datasets)
head(uspop)
plot(uspop)

#Problem6
library("ggmap")
library("maptools")
library(maps)
visited <- c("SFO", "Chennai", "London", "Melbourne", “Lima,Peru”, "Johannesbury, SA")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))

points(visit.x,visit.y, col="red", pch=36)
library("ggmap")
library("maptools")
library(maps)
visited <- c("SFO", "New York", "Buffalo", "Dallas, TX")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
map("state", fill=TRUE, col=rainbow(50), bg="lightblue", mar=c(0,0,0,0))
points(visit.x,visit.y, col="yellow", pch=36)

#Problem7
library(lattice)
splom(mtcars[c(1,3,4,5,6)], main="MTCARS Data")
splom(mtcars[c(1,3,4,6)], main="MTCARS Data")
splom(mtcars[c(1,3,4,6)], col=rainbow(4),main="MTCARS Data")

#Problem8

library(ggplot2)
theme_set(theme_classic())

# Histogram on a Categorical variable
g <- ggplot(mpg, aes(manufacturer))
g + geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes") 