setwd("D:/Study/Coursera_Reproducible Research/Project 1/repdata-data-activity")
act <- read.csv("./activity.csv")
str(act)
D <- act$date
S <- act$steps
I <- act$interval
D <- as.Date(D)

bad <- is.na(S)
S <- S[!bad]
D <- D[!bad]
I <- I[!bad]
table(D)


act2 <- matrix(rep(1,30528),ncol=2,nrow=15264)
act2 <- data.frame(act2)
act2$D <- D
act2$S <- S
act2$I <- I
act2 <- act2[,-c(1,2)]

s <- split(act2, act2$D)
SUM <- sapply(s,function(x) sum(x[,c("S")]))
SUM <- data.frame(SUM)
SUM


##1.1
SUM$D<-as.character(labels(SUM)[[1]])
SUM$D <- strptime(SUM$D, "%Y-%m-%d")
plot(SUM$D, SUM$S, type="h", lwd=5, col="#ff2500", xlab="Date", ylab="Total Steps", main="Steps for Every Day")


##1.2
s1 <- split(act2, act2$D)
MEAN <- sapply(s1,function(x) colMeans(x[,c("S","I")]))
t(MEAN)  ##Look at the first column of the output data frame, just simply ignore the second column
MEAN <- data.frame(t(MEAN))
mean(MEAN$S)
median(MEAN$S)


##2.1
s2 <- split(act2, act2$I)
MEAN_Interval <- sapply(s2,function(x) colMeans(x[,c("S","I")]))
MEAN_Interval <- data.frame(t(MEAN_Interval))
plot(MEAN_Interval$I, MEAN_Interval$S, type="l", col="navy", xlab="Interval", ylab="Mean Steps", main="Steps for Each Interval")

##2.2
MAX_I <- MEAN_Interval[MEAN_Interval[,"S"]==max(MEAN_Interval$S),][,2]
paste("The 5-min interval that contains the maximum number of means of steps is the ", MAX_I, "th one.", sep="")


##3.1
table(complete.cases(act$date, act$interval, act$steps))


##3.2
any(is.na(act$date))
any(is.na(act$interval))
any(is.na(act$steps))
bad2 <- is.na(act$steps)
act_missing <- act[bad2, ]
ACT <- act
for (i in 1:17568){
  if (is.na(ACT[i,1])==TRUE){
    ACT[i,1] <- MEAN_Interval[MEAN_Interval[,"I"]==ACT[i,3],1]
  }    ##Substitute the NA's wirh the mean for corresponding 5-minute interval
}
any(is.na(ACT$date))
any(is.na(ACT$interval))
any(is.na(ACT$steps))      ##ACT is the new data set without any NA's.


##3.4
D2 <- ACT$date
S2 <- ACT$steps
I2 <- ACT$interval
D2 <- as.Date(D2)
ACT2 <- matrix(rep(1,35136),ncol=2,nrow=17568)
ACT2 <- data.frame(ACT2)
ACT2$D2 <- D2
ACT2$S2 <- S2
ACT2$I2 <- I2
ACT2 <- ACT2[,-c(1,2)]
s <- split(ACT2, ACT2$D2)
SUM2 <- sapply(s,function(x) sum(x[,c("S2")]))
SUM2 <- data.frame(SUM)
SUM2$D2<-as.character(labels(SUM2)[[1]])
SUM2$D2 <- strptime(SUM2$D2, "%Y-%m-%d")
plot(SUM2$D, SUM2$SUM, type="h", lwd=5, col="#ff2500", xlab="Date", ylab="Total Steps", main="Steps for Every Day")
s3 <- split(ACT, ACT$date)
MEAN2 <- sapply(s3, function(x) colMeans(x[,c("steps","interval")]))
t(MEAN2)  ##Look at the first column of the output data frame, just simply ignore the second column
MEAN2 <- data.frame(t(MEAN2))
mean(MEAN2$steps)
median(MEAN2$steps)


##4.1
AC <- ACT
AC$date <- strptime(ACT$date, "%Y-%m-%d")
AC$Wkd <- weekdays(AC$date)
Wk <- rep("A",nrow(AC))
AC$Wk <- Wk
for (i in 1:nrow(AC)){
  if (AC[i,4]=="Saturday" | AC[i,4]=="Sunday"){
    AC[i,5] <-"weekend"
  }
  else {
    AC[i,5] <-"weekday"
  }
}
head(AC)
AC_1 <- AC[AC[,"Wk"]=="weekday",]
AC_2 <- AC[AC[,"Wk"]=="weekend",]
s_1 <- split(AC_1, AC_1$interval)
s_2 <- split(AC_2, AC_2$interval)
MEAN_Interval_1 <- sapply(s_1,function(x) colMeans(x[,c("steps","interval")]))
MEAN_Interval_2 <- sapply(s_2,function(x) colMeans(x[,c("steps","interval")]))
MEAN_Interval_1 <- data.frame(t(MEAN_Interval_1))
MEAN_Interval_2 <- data.frame(t(MEAN_Interval_2))
par(mfrow=c(2,1))
plot(MEAN_Interval_1$interval, MEAN_Interval_1$steps, type="l", col="skyblue2", xlab="Interval", ylab="Number of Steps", main="(Weekday) Steps for Each Interval")
plot(MEAN_Interval_2$interval, MEAN_Interval_2$steps, type="l", col="orange", xlab="Interval", ylab="Number of Steps", main="(Weekend) Steps for Each Interval")

