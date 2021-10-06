# question: how can I arrange lists so that the randomly drawing an element from each of list, and substracting from each other,
# lead to uniform distribution of (all possible differences have the same probability)

# I think Box-Muller does the conversion of uniform to normal, but how can I arrange my lists so that 
# the the substraction of the random pairings between the two sets have the equal probability of appearing across the range?

listA <- -10:10
listB <- -10:10

#plot the distribution of the list
hist(listB, freq = FALSE, xlab = 'x', density = 20)

#sample random elements from both of the lists
sampledListA <- sample(listA, 1000, replace = TRUE)
sampledListB <- sample(listB, 1000, replace = TRUE) 

#I then draw one element from each of the list and I calculate the difference of the two drawn value
#and I want the occurences of the differences to be equal.

# I can calculate the difference by element
listDiff <- sampledListA - sampledListB

#here is the normal distribution this leads to
hist(listDiff, freq = FALSE, xlab = 'x', density = 20)
#calculate quantile
quantile(listDiff)

### attempt ###
# I can calculate the possible differences using outer function

diffMatrix <- data.frame(outer(listA,listB, '-'))

#change the column and row names
library(stats)
nms <- as.character(listA)
rownames(diffMatrix) <- nms
names(diffMatrix) <- nms
diffMatrix

# I can then find the list of possible unique differences, and draw samples from that
vectorized <- unlist(diffMatrix)
diffRange <- unique(vectorized)

getDiffSamples<-sample(diffRange, 1000, replace = TRUE) #get 1000 random sample from each diff value
hist(getDiffSamples,freq = FALSE, xlab = 'x', density = 20) #then I will have uniform distribution

#I can then get a value from this distribution, find its index in the matrix of differences
which(diffMatrix == a[1], arr.ind=TRUE)

#but this solution does not tight and I am looking for a way to adjust the list.
#as my ultimate goal is to have a list of A and B that when I randomly pick one from each list, and 
# present them side by side, the subtraction of them, when plotted will have the same probability. 


#desired output
# I can then approximate the density function for X
rand.unif <- runif(1000, min = min(diffRange), max = max(diffRange))
hist(rand.unif, freq = FALSE, xlab = 'x', density = 20)

#attempts
qunif(quantile(listDiff),min = min(diffRange), max = max(diffRange))


