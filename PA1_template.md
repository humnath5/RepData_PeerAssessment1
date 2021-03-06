# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
As suggested by assignment instructions,"RepData_PeerAssessment" Repository was forked and cloned in local machine and made as working directory.


```r
     #loading and processing data 
      unzip("activity.zip")
      data =read.csv("activity.csv")
      data$date <- as.Date(data$date)
      data$interval<-sprintf("%04d",data$interval)
```


## What is mean total number of steps taken per day?


```r
      missedData <- is.na(data$steps)
      pureData   <- data[!missedData,]
      totalsteps <- tapply(pureData$steps,pureData$date,sum)
      
      hist(totalsteps,
           main="Histogram of total steps",
           xlab="total steps",col = "Blue")
```

![](figure/unnamed-chunk-2-1.png) 

 **Mean and Median total steps per day**

```r
    meansteps <- mean(totalsteps)
    mediansteps <-median(totalsteps)
```

mean total steps per day : **10766.19** 

median total steps per day : **10765.00**


## What is the average daily activity pattern?


```r
      meanByInterval <-tapply(pureData$steps,pureData$interval,mean)
      timeInterval<-unique(pureData$interval)                                        
      plot(timeInterval,meanByInterval,
                type="l",main="Avarage daily Steps per Intervals",
                xlab="Interval[HM format]",ylab="Average steps",
                col="red",lwd = 2)
```

![](figure/unnamed-chunk-4-1.png) 

```r
      maxSteps <-max(meanByInterval)
      Interval_of_max <-timeInterval[which(meanByInterval==maxSteps)]
```

The maximum average steps over all days: **206.1698113** 

Maximum average steps was in the interval **"0835"**.

It means the activities  were highest during interval of **"8:35-8:40"**.



## Imputing missing values


```r
       missedData <- is.na(data$steps)
       num_missData <-sum(missedData)
```

Total number of missing data : **2304** 

Replacing NAs by mean steps per intervals over all days:


```r
        library(dplyr)
```


```r
        missPosition <- which(missedData==TRUE)
        temp<-data$steps  #temporary variable
        temp[missPosition] <-meanByInterval[paste(data$interval[missPosition], "", sep = "")]
        stepsFilled <-temp
        
        dataFilled <- mutate(data,stepsFilled =stepsFilled)
        dataFilled<-select(dataFilled,stepsFilled,date,interval)
```

Ploting histogram :

```r
        totalStepsFilled <- tapply(dataFilled$stepsFilled,dataFilled$date,sum)
        hist(totalStepsFilled,
                  main="Histogram of total steps per day in filled dataset",
                  col="red",
                  xlab="total steps ")
```

![](figure/unnamed-chunk-8-1.png) 

Calculating mean and median total steps per day in filled dataset:


```r
          meanStepsFilled<- mean(totalStepsFilled)
          medianStepsFilled <-median(totalStepsFilled)
```

**Mean** total steps per day in filled dataset : **10766.19**

**Median** total steps per day in filled data set : **10766.19**

Result shows  mean doesnot change but median changes by filling NAs using mean steps of corresponding interval.Also,mean and median values are same [at least upto two decimal places].


## Are there differences in activity patterns between weekdays and weekends?


```r
          library(tidyr)
          library(lattice)
          dataFilled$interval <-as.factor(dataFilled$interval)
          day <-weekdays(dataFilled$date)
          day <-ifelse(day==c("Saturday","Sunday"),"weekend","weekday")
          day<-as.factor(day)
          dataFilled<-mutate(dataFilled,day=day) # adding "day" column in dataFilled
          mean_by_day<-tapply(dataFilled$stepsFilled,
                              list(dataFilled$interval,dataFilled$day),mean)
          mean_by_day <-as.data.frame(mean_by_day) 
          mean_by_day <- mutate(mean_by_day,interval=rownames(mean_by_day))

          #making data tidy using gather funtion in "tidyr" package 
          tidyData <- gather(mean_by_day,day,value,-interval)
          tidyData <- select(tidyData,mean=value,interval,day)
          tidyData$interval <-as.numeric(tidyData$interval)

          p <-xyplot(mean ~ interval |day,
                                      data=tidyData,
                                      layout =c(1,2),
                                       type="l",
                                       ylab ="Average steps",
                                       xlab ="time interval[HM format]"
                              )
         print(p)
```

![](figure/unnamed-chunk-10-1.png) 

Plot in  weekend is more dense than plot  in weekday which means there were more activities 
during weekend.




