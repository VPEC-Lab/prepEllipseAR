## Sept 2021
## @gorkemer
## Generating a set of aspect ratios to be used as ellipse stimuli. 

#setting up working directory
getwd()
setwd("Desktop")
options(scipen=999) #remove scientific notation

#### 0- setup data ####

#create AR values in between, not just 0.046 steps of logAR

# In the motion-ellipses studies, I had 21 AR test stimuli options, 
# and 27 AR stimuli options for the response stimuli (+3 on both sides). 
# each was distanced from each other by 0.046331818, which is referred as 
# ARChangeConstant. 

#### 1- create target ARs ####
# This script allows for creating a larger ellipse AR values. 
# I will start with doubling the size: 42 test AR, 54 response AR.
# But I will keep the flattest and tallest, ie. upper and lower limit, the same. 
# Only the inside range between those limits are diversified.

upperLimit = -1.01930048 #-0.602314116
upperLimit = 1.01930048
lastlogARValue = -1.01930048

changeConst = 0.04633181818/2 # here I populate two times more than the original set size. 

logARList_wider = c(-1.01930048)
while (lastlogARValue <= 1.01930048){
  logAR = lastlogARValue + changeConst
  logARList_wider = append(logARList_wider, logAR)
  lastlogARValue = logAR
}
logARList_wider
#remove first and last values. Note! look at the list then decide what to remove
logARList_wider = head(logARList_wider, -1)
logARList_wider
length(logARList_wider) #now the range is 53.

#### 2- Create height & width pairings using the AR ####

#first we convert them into non-log ARs.
ARs <- 10^logARList_wider
ARs
# now using AR list, I create width & height
lastHeight = 1.2 #I arbitrarily select a height value here. 1.2 was used in the original excel sheet. 
heightList = c(lastHeight) #starting (here referred as lastHeight) height is fixed. 

# For some reason, heights and widths are defined differently for each side of the AR 1.
# So I find the width & height values where AR is 1. Then use that position to stop the
# loop below. 
AR1<- subset(ARs, ARs>0.99 & ARs<1) #this is how I identify the AR of 1 (or close). 
rowN_ofAR1 <- match(AR1, ARs)

for (i in 2:rowN_ofAR1){ # loop starts from 2 because I start with a element already in the list c(lastHeight)
  print(i)
  height = lastHeight + 0.15/2 # here I divide this number into two because I want the values to be less spread. 0.15 was used in the original excel sheet.
  heightList = append(heightList, height)
  lastHeight = height
  print(heightList[i])
}

widthList = c()
for (i in 1:length(heightList)){
  width = heightList[i]/ARs[i] #define width using height and AR
  widthList = append(widthList,width)
}

widthList
heightList
length(widthList)
length(heightList)

## now do the same for the flat ARs, numbers below AR of 1. 
#get the last element in the width list
lastWidth = tail(widthList, 1)

#increment on the last item by 0.15 to create the rest of the width values
for (i in (rowN_ofAR1+1):length(ARs)){ # rowN_ofAR1 + 1 because I start after the AR of 1
  width = lastWidth - 0.15/2 # 0.15/2
  widthList = append(widthList, width)
  lastWidth = width
  
  height = width * ARs[i]
  heightList = append(heightList, height)
}

widthList
heightList
length(widthList)
length(heightList)

#### 3- calculate equating variable, equatVar ####
equatVarList = c()
equatedArea = 5.15
for (i in 1:length(heightList)){
  equatVarExp = equatedArea/ (0.5*0.5*pi*heightList[i]*widthList[i])
  equatVar = sqrt(equatVarExp) #get the square root of this
  equatVarList = append(equatVarList,equatVar)
}

equatVarList

#### 4- Final: calculate newHeight and newWidth using the equating variable ####

newHeightList = c()
newWidthList = c()

for (i in 1:length(ARs)){
  newHeight = heightList[i] * equatVarList[i]
  newWidth = widthList[i] * equatVarList[i]
  
  newHeightList = append(newHeightList, newHeight)
  newWidthList = append(newWidthList, newWidth)
}

newHeightList
newWidthList

#### 5- cross-checks ####

#the area calculated by the new height and width should give 5.15
newArea <- pi*0.5*newHeightList*0.5*newWidthList
newArea

#### 6- finalize and send output ####

#actually, in background-motion experiment, I multiplied the resulting (above) 
# width and heights with 50. So, the final widths are:
finalHeight = newHeightList*50
finalWidth = newWidthList*50

finalARList = data.frame(finalHeight, finalWidth)

#last checks
finalARList$AR <- finalARList$finalHeight/ finalARList$finalWidth
finalARList$logAR <- log(finalARList$AR,10)

possibleLogDifferences <- outer(finalARList$logAR,finalARList$logAR, '-')
which(possibleLogDifferences == possibleLogDifferences[4,3], arr.ind=TRUE) # find the column and row of a given value
array(possibleLogDifferences)
max(unique(array(possibleLogDifferences)))
#### 6.a test of random samlping bias in rendering non-uniform distribution ####

firstLog <- sample(finalARList$logAR, 1000, replace = TRUE) # pick the first shape log
secondLog <- sample(finalARList$logAR, 1000, replace = TRUE) # pick the second shape log

shapeDiff <- firstLog - secondLog
hist(shapeDiff, freq = FALSE, xlab = 'x', density = 20)




#### 7- creating bins of AR differences ####

#create a new matrix to work on this 7)
ARList_rounded <-data.frame(height=1:length(finalARList$finalHeight),width=NA, AR= NA, logAR= NA)
# rounding AR values
ARList_rounded$height <- round(finalARList$finalHeight, digits = 2)
ARList_rounded$width <- round(finalARList$finalWidth, digits = 2)
ARList_rounded$AR <- round(finalARList$AR, digits = 2)
ARList_rounded$logAR <- round(finalARList$logAR, digits = 2)

# throw random shapes and calculate their AR values (by looking into the dictionary),
# then calculate the AR difference between them, then group by AR difference.
randomAR_1 <- sample(ARList_rounded$logAR, 100, replace = TRUE)
randomAR_2 <- sample(ARList_rounded$logAR, 100, replace = TRUE)

#randomly selects item from a list, select 400 random AR, then pair them. 
shapeDetailList <-data.frame(Log1=1:400000,Log2=NA, LogDiff= NA)

for (i in 1:400000){
  randomAR_1 <- sample(ARList_rounded$logAR, 1)
  randomAR_2 <- sample(ARList_rounded$logAR, 1)
  shapeDetailList$Log1[i] <- randomAR_1
  shapeDetailList$Log2[i] <- randomAR_2
  shapeDetailList$LogDiff[i] <- abs(randomAR_1- randomAR_2)
}
shapeDetailList <- shapeDetailList[order(shapeDetailList$LogDiff),]

# shapeList_1 <- c(shapeList_1, randomAR) ## appending item to a list

#> max(diffOfList)
#[1] 1.204627
#> min(diffOfList)
#[1] -1.181461

