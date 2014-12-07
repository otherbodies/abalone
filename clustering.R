# here - regression lines, both methods computations - and writing to the table. 

load("data.Rda")

library(plyr)
library(reshape2)

means2 = means
names(means2) = c("m","condition","task","participant","z","x","y")
means2$x = means2$x*-1

computeSlopAngles = function(subs)
  {

  slop = lm(subs$z~subs$x)
  
 angle = format(atan(slop$coef[2])*360/2/pi, digits = 3)
 return(angle)
}


angleSlope = ddply(means2,.(participant,task,condition),computeSlopAngles)

angleSlope = subset(angleSlope, task != "mini")

angSlopWide = dcast(angleSlope, participant + task ~ condition)

write.table(angSlopWide, file = "angles-slopes.csv", sep=";", row.names = FALSE)


# cluster analysis of angle - slopes
cluLabels2 = paste(as.character(angSlopWide$participant),as.character(angSlopWide$task))
clu3 = hclust(dist(angSlopWide[c("hd","str","tr")]),method="ward.D")
plot(clu3,labels=cluLabels2,main="hierarchical cluster ward")

#






## alternative slope analysis 
# find largest distance between points
subs = subset(means2,participant=="Co3"& task=="month"& condition=="str")


altSlopeAnalysis = function(subs){
  
  distanceList =  data.frame(distances=numeric(nrow(subs)),id=numeric(nrow(subs))) 
  pointId = 999
  
  # for each point {
  for (i in 1:nrow(subs)){
    
  largestDist = 0
  point = c(subs$z[i],subs$x[i]) 
  
  #debug 
 # point = c(subs$z[1],subs$X[1])
  
# create a list of distances from that one point to all others
    for (j in 1:nrow(subs)){
    secondPoint = c(subs$z[j],subs$x[j])
    currentDist = dist(rbind(point,secondPoint))
   # distanceList$distances[j] = currentDist
  #  }
#distanceList    

    if(currentDist>largestDist){
    largestDist = currentDist
    pointId = j   
      }  
    }
  distanceList$distances[i]=largestDist
  distanceList$id[i]=pointId
  
  }
  distanceList 

distList = distanceList
which(distList$distances==max(distList$distances))
#shortDistList = subs[which(distList$distances==max(distList$distances)),]

}
# slopes for largest distance points
slop = lm(shortDistList$z~shortDistList$x)
angle = format(atan(slop$coef[2])*360/2/pi, digits = 3)
return(angle)
}

##
meanSubset2 = subset(means,participant==input$participant & condition=="str" & task==input$task)
distList = altSlopeAnalysis(meanSubset2)
which(distList$distances==max(distList$distances))
meanSubset = meanSubset[which(distList$distances==max(distList$distances)),]

##

testaltdistList = altSlopeAnalysis(subs)
testaltdistList
#which(distList$distances==max(distList$distances))
#shortDistList = subs[which(distList$distances==max(distList$distances)),]

# slopes for largest distance points
#slop = lm(shortDistList$z~shortDistList$X)
#angle = format(atan(slop$coef[2])*360/2/pi, digits = 3)

altSlopeContinue = function(subs){
 
  
  
  subsetForlm = c(subs$V1,subs$V2)
  
  subsetForlm_head = subset(means2, participant==subs$participant & task==subs$task & condition=="hd")
  ts = subsetForlm_head[subsetForlm] # ts - temp subset
 slop = lm(ts$z~ts$x)
 angle = format(atan(slop$coef[2])*360/2/pi, digits = 3)
 return(angle)
 
 
}
altangleSlopeTemp["angleHd"]= NA
altangleSlopeTemp["angleStr"]= NA
altangleSlopeTemp["angleTr"]= NA

subs = altangleSlopeTemp
for(i in 1:nrow(altangleSlopeTemp)){
  #i = 1
  subsetForlm = c(subs[i,]$V1,subs[i,]$V2)
  subsetForlm_head = subset(means2, participant==subs[i,]$participant & task==subs[i,]$task & condition=="hd")
  ts = subsetForlm_head[subsetForlm,] # ts - temp subset
  slop = lm(ts$z~ts$x)
  angle = format(atan(slop$coef[2])*360/2/pi, digits = 3)
  altangleSlopeTemp[i,]$angleHd = angle
  
  subsetForlm_tr = subset(means2, participant==subs[i,]$participant & task==subs[i,]$task & condition=="tr")
  ts = subsetForlm_tr[subsetForlm,] # ts - temp subset
  slop = lm(ts$z~ts$x)
  angle = format(atan(slop$coef[2])*360/2/pi, digits = 3)
  altangleSlopeTemp[i,]$angleTr = angle
  
  subsetForlm_str = subset(means2, participant==subs[i,]$participant & task==subs[i,]$task & condition=="str")
  ts = subsetForlm_str[subsetForlm,] # ts - temp subset
  slop = lm(ts$z~ts$x)
  angle = format(atan(slop$coef[2])*360/2/pi, digits = 3)
  altangleSlopeTemp[i,]$angleStr = angle
}

## a hack! to check it! manually putting Co1month data because automated way got it wrong
altangleSlopeTemp[2,]$angleStr = -4.38
altangleSlopeTemp[2,]$angleTr = -8.37
altangleSlopeTemp[2,]$angleHd = -4.23
##

##
means2 = means2[-48,]
means2subs = subset(means2,condition=="str")


altangleSlopeTemp = ddply(means2subs,.(participant,task,condition),altSlopeAnalysis)

altangleSlope = ddply(altangleSlopeTemp,.(participant,task),altSlopeContinue)

altangleSlope = subset(altangleSlope, task != "mini")

altangSlopWide = dcast(altangleSlope, participant + task ~ condition)

write.table(altangSlopWide, file = "altangles-slopes.csv", sep=";", row.names = FALSE)