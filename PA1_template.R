          #========PA1_Reproducible Research========#
          #   Coruse: Data Science Specialization 
          #  Institution : John Hopkin University/Coursera 
          #   Due data :  7/19/2015 


            #loading and processing data 
            library(dplyr)
            library(tidyr)
            library(lattice)
            
            unzip("activity.zip")
            data =read.csv("activity.csv")
            data$date <- as.Date(data$date)
            data$interval<-sprintf("%04d",data$interval)

            #Mean and median  total per day
             
            missedData <- is.na(data$steps)
            pureData   <- data[!missedData,]
            totalsteps <- tapply(pureData$steps,pureData$date,sum)
            hist(totalsteps,
                 main="Histogram of total steps",
                 xlab="total steps",col = "Blue")

            meansteps <- mean(totalsteps)
            mediansteps <-median(totalsteps)
          
        
            meanByInterval <-tapply(pureData$steps,pureData$interval,mean)
            
            timeInterval<-unique(pureData$interval)
            
            plot(timeInterval,meanByInterval,
                 type="l",main="Avarage daily Steps per Intervals",
                 xlab="Interval[HM format]",ylab="Average steps",
                 col="red",lwd = 2)
    
            maxSteps <-max(meanByInterval)
            
            Interval_of_max <-timeInterval[which(meanByInterval==maxSteps)]
            #finding total number of missing values
            missedData <- is.na(data$steps)
            num_missData <-sum(missedData)

#============inputing missing values================================#
#===================================================================#
            missPosition <- which(missedData==TRUE)
            temp<-data$steps  #temporary variable
            temp[missPosition] <-meanByInterval[paste(data$interval[missPosition],"",sep="")]
            stepsFilled <-temp
            
            dataFilled <- mutate(data,stepsFilled =stepsFilled)
            dataFilled<-select(dataFilled,stepsFilled,date,interval)


#========Calculating mean and median total for filled dataset====#
#=================================================================#
    
            totalStepsFilled <- tapply(dataFilled$stepsFilled,dataFilled$date,sum)
            hist(totalStepsFilled,
                 main="Histogram of total steps per day in filled dataset",
                 col="red",
                 xlab="total steps ")
    
    
            meanStepsFilled<- mean(totalStepsFilled)
            medianStepsFilled <-median(totalStepsFilled)
    


#=========average daily activity pattern in weekday and weekend==#
#================================================================#

              dataFilled$interval <-as.factor(dataFilled$interval)
              day <-weekdays(dataFilled$date)
              day <-ifelse(day==c("Saturday","Sunday"),"weekend","weekday")
              day<-as.factor(day)
              
              dataFilled<-mutate(dataFilled,day=day)
              mean_by_day<-tapply(dataFilled$stepsFilled,
                                  list(dataFilled$interval,dataFilled$day),mean)
              
              mean_by_day <-as.data.frame(mean_by_day) 
              
              mean_by_day <- mutate(mean_by_day,interval=rownames(mean_by_day))
              
              tidyData <- gather(mean_by_day,day,value,-interval)
              tidyData <- select(tidyData,mean=value,interval,day)
              tidyData$interval <-as.numeric(tidyData$interval)

#=================panel plot =================================#
#=============================================================#

               p <- xyplot(mean ~ interval |day,
                        data=tidyData,
                        layout =c(1,2),
                         type="l",
                         ylab="Average steps",
                         xlab="time interval[HM format]"
                         )
               print(p)
      

  
  
  



