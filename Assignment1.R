#reading activity.csv#
activity<-read.csv("./data/activity.csv")
good<-complete.cases(activity)
#removing NA entries and copying to act#
act<-activity[good,]
perday<-tapply(act$steps,act$date,sum)
#histogram of steps per day#
hist(perday)
#mean and median #
mean(perday,na.rm=TRUE) ##[1]10766
median(perday,na.rm=TRUE) ##[2]10765
#calculating and plotting average daily activity pattern#
persecond<-tapply(act$steps,act$interval,mean)
plot(names(persecond),persecond,type="l")
#algorith for missing values#
actcomplete<-act
for(i in 1:17568){
  if(is.na(actcomplete$steps[i])){
    actcomplete$steps[i]<-persecond[which(actcomplete$interval[i]==names(persecond))]
  }
}
#plotting weekday and weekend activity#
ac<-actcomplete
ac$date<-as.Date(ac$date)
ac$d<-weekdays(ac$date)
weekday<-subset(ac,d=="Monday"|d=="Tuesday"|d=="Wednesday"|d=="Thursday"|d=="Friday")
weekend<-subset(ac,d=="Saturday"|d=="Sunday")
perweekday<-tapply(weekday$steps,weekday$interval,mean)
perweekend<-tapply(weekend$steps,weekend$interval,mean)
plot(names(perweekday),perweekday,type="l")
plot(names(perweekend),perweekend,type="l")
## More distributed activity on the weekends than on the weekdays where there is one peak dude to office commute##




