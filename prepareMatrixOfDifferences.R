## Sept 2021
## @gorkemer
## Generating a set of aspect ratios to be used as ellipse stimuli. 

library(xlsx)
library(stats)

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

upperLimit = -0.602314116 #-0.602314116 #1.01930048
upperLimit = 0.602314116 #1.01930048
lastlogARValue = -0.602314116 #1.01930048

changeConst = 0.04633181818 # here I populate two times more than the original set size. 

logARList_wider = c(-0.602314116)
while (lastlogARValue <= 0.602314116){
  logAR = lastlogARValue + changeConst/2
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
  height = lastHeight + 0.15 # here I divide this number into two because I want the values to be less spread. 0.15 was used in the original excel sheet.
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
  width = lastWidth - 0.15 # 0.15/2
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

diffMatrix <- data.frame(outer(finalARList$logAR,finalARList$logAR, '-'))


#colnames(possibleLogDifferences) change the column and row names
library(stats)
nms <- as.character(finalARList$logAR)
rownames(diffMatrix) <- nms
names(diffMatrix) <- nms
diffMatrix

#get the full range of AR difference possibilities

vectorized <- unlist(diffMatrix)
diffRange <- sort(unique(vectorized)) # 237 UNIQUE FOR THE original list; 833 unique difference types. 
range(vectorized) #-1.20 to 1.20 range of different values

#### 7- meaty part ####
# now I will calculate a list of 767 different AR difference

##addressOfDiffValues <- data.frame(which(diffMatrix == diffRange[2], arr.ind=TRUE)) ## here I get the address of the matrix where this value sits. 


listOfAllCombinations <- list() #that the difference 

for (i in 1:length(diffRange)){
  #get the row and col addresses, go by row
  # get the first row (first difference value), get the rowand column index of that in the main address sheet
  ##rowAddress <- addressOfDiffValues[i,1] #get the row address (first column in the main address sheet)
  ##colAddress <- addressOfDiffValues[i,2] #get the column address
  #print(names(addressOfValue[rowAddress)
  addressOfDiffValues <- which(diffMatrix == diffRange[i], arr.ind=TRUE)
  diffRowNames <- as.numeric(row.names(diffMatrix[addressOfDiffValues[,1],] )) # get the row names 
  diffColNames <- as.numeric(names(diffMatrix[addressOfDiffValues[,2]])) # get the column names
  oneList <- list(diffRowNames,diffColNames) #bind the row and col names vectors

  name <- paste('item:',diffRange[i],sep='')
  listOfAllCombinations[[name]] <- oneList
}



listOfSingleFrame <- list()
singleFrame <- NULL
### create new lists with row and column as the nested variable ###
for (i in 1:length(listOfAllCombinations)){
  for (a in 1:length(listOfAllCombinations[[i]][[1]])){#extract the list and go through each row in a list
    print(a)
    firstAR <- unlist(listOfAllCombinations[[i]][1])[a] #get the row, 1st AR

    secondAR<- unlist(listOfAllCombinations[[i]][2])[a] #get the col, 2st AR
    logPair <- array(c(firstAR,secondAR)) #    c <- data.frame(a,b)
    logPairSingMatrix <- matrix(list(logPair),1) #1 is for 1 row
    print(logPairSingMatrix)

    #push that array to the end of the matrix
    singleFrame=rbind(singleFrame,logPairSingMatrix)
    print(singleFrame)
  } 
  ##listOfSingleFrame$i <- i
  ##listOfSingleFrame[[i]] <- singleFrame
  ##z <- array( c( x , y ) , dim = c( 3 , 3 , 2 ) )
   
  name <- paste('diff',firstAR-secondAR,sep='')
  listOfSingleFrame[[name]] <- singleFrame
  singleFrame<- NULL
  
}

##test <- data.table(listOfAllCombinations)
##listOfAllCombinations[[1]]

#import the list to json
library(rjson)
listOfListJSON <- toJSON(listOfSingleFrame, indent=0, method="C" )
listOfListJSON
#write json
write(listOfListJSON, "test.json")


