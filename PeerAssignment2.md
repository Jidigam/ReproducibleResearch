Loss in Economic and Health Consequences due to Storm and other severe Weather Events
========================================================

The aim of this Assessment is to provide a analysis by which we can know the the economic and health issues that are faced due to the Storm and other weather events. The analysis is carried out on the data shared by NOAA, the data contains the characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

## Data Processing

##### Set the Current Working Directory
##### Make Use of only useful data




```r
data = read.csv(bzfile("repdata-data-StormData.csv.bz2"))
tidydata <- subset(data, !is.na(PROPDMGEXP) & !is.na(CROPDMGEXP), select = c("FATALITIES", 
    "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "EVTYPE"))
str(tidydata)
```

```
## 'data.frame':	902297 obs. of  7 variables:
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
```



## Getting and Cleaning Data 

```r
Fatalities <- aggregate(tidydata$FATALITIES, by = list(tidydata$EVTYPE), sum)

F1 <- Fatalities[order(Fatalities$x, decreasing = TRUE), ][1:20, ]

Injuries <- aggregate(tidydata$INJURIES, by = list(tidydata$EVTYPE), sum)

I1 <- Injuries[order(Injuries$x, decreasing = TRUE), ][1:20, ]


tidydata$PROPDMGEXP <- as.character(tidydata$PROPDMGEXP)
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "B"] <- 1e+09
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "h"] <- 100
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "H"] <- 100
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "K"] <- 1000
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "m"] <- 1e+06
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "M"] <- 1e+06
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "0"] <- 0
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "1"] <- 1
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "2"] <- 100
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "3"] <- 1000
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "4"] <- 10000
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "5"] <- 1e+05
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "6"] <- 1e+06
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "7"] <- 1e+07
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "8"] <- 1e+08
tidydata$PROPDMGEXP[tidydata$PROPDMGEXP == "9"] <- 1e+09



tidydata$CROPDMGEXP <- as.character(tidydata$CROPDMGEXP)
tidydata$CROPDMGEXP[tidydata$CROPDMGEXP == "B"] <- 1e+09
tidydata$CROPDMGEXP[tidydata$CROPDMGEXP == "K"] <- 1000
tidydata$CROPDMGEXP[tidydata$CROPDMGEXP == "m"] <- 1e+06
tidydata$CROPDMGEXP[tidydata$CROPDMGEXP == "M"] <- 1e+06
tidydata$CROPDMGEXP[tidydata$CROPDMGEXP == "0"] <- 0
tidydata$CROPDMGEXP[tidydata$CROPDMGEXP == "2"] <- 100
tidydata$CROPDMGEXP[tidydata$CROPDMGEXP == "?"] <- 0

tidydata$totaldamage <- as.numeric(tidydata$PROPDMGEXP) * (tidydata$PROPDMG)
```

```
## Warning: NAs introduced by coercion
```

```r
TotalProp <- aggregate(tidydata$totaldamage, by = list(tidydata$EVTYPE), sum)
T1 <- TotalProp[order(TotalProp$x, decreasing = TRUE, na.last = NA), ][1:30, 
    ]


tidydata$totalcropdamage <- as.numeric(tidydata$CROPDMGEXP) * tidydata$CROPDMG
```

```
## Warning: NAs introduced by coercion
```

```r
TotalCropProp <- aggregate(tidydata$totalcropdamage, by = list(tidydata$EVTYPE), 
    sum)
T2 <- TotalCropProp[order(TotalCropProp$x, decreasing = TRUE, na.last = NA), 
    ][1:30, ]

```


# Results:

##1.Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

## Make Use of Aggregate Function for summing of Fatalities based up on EVTYPE 


```r

barplot(F1$x, names.arg = F1$Group.1, main = "Fatalities", ylab = "fatalities", 
    cex.axis = 0.8, cex.names = 0.7, las = 2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r

```


## Make Use of Aggregate Function for summing of Injuries based up on EVTYPE 


```r

barplot(I1$x, names.arg = I1$Group.1, main = "Injuries", ylab = "Injuries", 
    cex.axis = 0.8, cex.names = 0.7, las = 2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


### The Major events that causes the health issues(Fatalities,Injuries) are the events "Tornado"


##2.Across the United States, which types of events have the greatest economic consequences?



```r

barplot(T1$x/10^9, names.arg = T1$Group.1, main = "Property Damages", ylab = "Properties", 
    cex.axis = 0.8, cex.names = 0.7, las = 2)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-51.png) 

```r


barplot(T2$x, names.arg = T2$Group.1, main = "Crop Damages", ylab = "No of Crops", 
    cex.axis = 0.8, cex.names = 0.7, las = 2)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-52.png) 


