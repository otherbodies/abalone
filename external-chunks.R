## @knitr model-fit-adjusted

# computing 45 degree model shift -----------------------------------------

## computing assumed position shifted 45deg  - to finish
theta1 = 45
theta = pi * theta1 / 180      # convert to radians.
radius = 60  # distance between chair rotation axis and participant trunk
pz = 2100 + radius * cos(theta)
px = 0 - radius * sin(theta)
pz  # 2142.426
px # -42.42641

### computing for eyes-head - base y = dist from chair to eyes plus eyes to head rot - 2100 + 60 + 90
theta1 = 45
theta = pi * theta1 / 180      # convert to radians.
radius = 90  # distance between chair rotation axis and participant trunk
pz = 2250 - radius * cos(theta)
px = 0 + radius * sin(theta)
pz  # eyes-head y 2186.36
px # eyes-head x 63.63961


##
# correction for plotting straight in trunk trials (for head centered RF) 
# add to all str points -17.574 on z , -42.42641 on x


# compute trunk fit 
# first shifted means with chair-body dist corrected origin position
# 

## computing model points for 45deg shift and writing to file

# chair corrected origin
originX=0
originz=2160


originX=0
originz=2160
deg = 45*(pi/180)

computeShift = function(m,condition,task,participant,z,X,y){
  currentX = X
  currentz = z
  distFromSeat = sqrt((currentX-originX)^2+(currentz-originz)^2)
  modelX = originX + (currentX - originX)*cos(deg) - (currentz - originz)*sin(deg)
  modelz = originz + (currentX - originX)*sin(deg) + (currentz - originz) *cos(deg) 
  returnFrame = data.frame(m,condition,task,participant,modelz,modelX,y)
  
}  


mstr = subset(means,condition=="str" & task!="mini")
#row.names(mstr)=NULL

originX=px
originz=pz
shiftedMeans = do.call(function(m,condition,task,participant,z,X,y,...) computeShift(mstr$m,mstr$condition,mstr$task,mstr$participant,mstr$z,mstr$X,mstr$y),mstr)
write.table(shiftedMeans, file = "model-shifts.csv", sep=";", row.names = FALSE)



# test block  -------------- 
# 2000-distFromSeat //validation of distance
distModel = sqrt((modelX)^2+(modelz-2100)^2)
distThird = sqrt((modelX-currentX)^2+(modelz-currentz)^2)

# Angle = arccos ( (A^2+B^2-C^2) / 2AB )  //validation of angle
acos((distFromSeat^2+distModel^2-distThird^2)/(2*distFromSeat*distModel))




# Comparisons to model shift analysis  -------------------------------------------------------

mtr = subset(means,condition=="tr" & task!="mini")
mhd = subset(means,condition=="hd" & task!="mini")
row.names(mtr)=NULL
row.names(mhd)=NULL

#removal of missed september in Co1months, head condition - REFACTOR THIS - remove by data name not number! 
#mstr = mstr[-9,]
#mtr = mtr[-9,]
#shiftedMeans = shiftedMeans[-9,]


# computing differences - to be refactored
DifX = abs(shiftedMeans$modelX - mtr$X)
Dify = abs(shiftedMeans$y - mtr$y)
Difz = abs(shiftedMeans$modelz - mtr$z)
DifDist = sqrt((shiftedMeans$modelX - mtr$X)^2 + (shiftedMeans$modelz - mtr$z)^2)

difSwingTrunk = data.frame(mstr$m,mstr$condition,mstr$task,mstr$participant,Difz,DifX,Dify,DifDist)
colnames(difSwingTrunk) = c("m","condition","task","participant","z","X","y","Dist")

DifX = abs(shiftedMeans$modelX - mhd$X)
Dify = abs(shiftedMeans$y - mhd$y)
Difz = abs(shiftedMeans$modelz - mhd$z)
DifDist = sqrt((shiftedMeans$modelX - mhd$X)^2 + (shiftedMeans$modelz - mhd$z)^2)
difSwingHead = data.frame(mstr$m,mstr$condition,mstr$task,mstr$participant,Difz,DifX,Dify,DifDist)
colnames(difSwingHead) = c("m","condition","task","participant","z","X","y","Dist")  

DifX = abs(mstr$X - mtr$X)
Dify = abs(mstr$y - mtr$y)
Difz = abs(mstr$z - mtr$z)
DifDist = sqrt((mstr$X - mtr$X)^2 + (mstr$z - mtr$z)^2)
difStaticTrunk = data.frame(mstr$m,mstr$condition,mstr$task,mstr$participant,Difz,DifX,Dify,DifDist)
colnames(difStaticTrunk) = c("m","condition","task","participant","z","X","y","Dist")

### - correction of straight in tr trials for hd centered fit
DifX = abs((mstr$X-42.42641) - mtr$X)
Dify = abs(mstr$y - mtr$y)
Difz = abs((mstr$z-17.574) - mtr$z)
DifDist = sqrt(((mstr$X-42.42641) - mtr$X)^2 + ((mstr$z-17.574) - mtr$z)^2)
difStaticTrunk = data.frame(mstr$m,mstr$condition,mstr$task,mstr$participant,Difz,DifX,Dify,DifDist)
colnames(difStaticTrunk) = c("m","condition","task","participant","z","X","y","Dist")
###

DifX = abs(mstr$X - mhd$X)
Dify = abs(mstr$y - mhd$y)
Difz = abs(mstr$z - mhd$z)
DifDist = sqrt((mstr$X - mhd$X)^2 + (mstr$z - mhd$z)^2)
difStaticHead = data.frame(mstr$m,mstr$condition,mstr$task,mstr$participant,Difz,DifX,Dify,DifDist)
colnames(difStaticHead) = c("m","condition","task","participant","z","X","y","Dist")


avDifSwingTrunk = aggregate(difSwingTrunk[c("z","X","y","Dist")],difSwingTrunk[c("task","participant")],mean)
avDifSwingHead = aggregate(difSwingHead[c("z","X","y","Dist")],difSwingHead[c("task","participant")],mean)
avDifStaticTrunk = aggregate(difStaticTrunk[c("z","X","y","Dist")],difStaticTrunk[c("task","participant")],mean)
avDifStaticHead = aggregate(difStaticHead[c("z","X","y","Dist")],difStaticHead[c("task","participant")],mean)

fitForTrunk = (avDifSwingTrunk$Dist + avDifStaticHead$Dist)/2
fitForHead = (avDifSwingHead$Dist + avDifStaticTrunk$Dist)/2
fitForRoom = (avDifStaticHead$Dist + avDifStaticTrunk$Dist)/2
fitAllRF = data.frame(avDifStaticHead$task,avDifStaticHead$participant,fitForHead,fitForTrunk,fitForRoom)
colnames(fitAllRF) = c("task","participant","fitHead","fitTrunk","fitRoom")

#adding type column and filling it // improve this part
fitAllRF["type"]=NA
fitAllRF$type[1:9]="con"
fitAllRF$type[10:19]="syn" #10:23 months, but 10:19 for crazy
#adding a coulmn of our visual inspection from quest3d
fitAllRF["quest"]=c("room","head","head","room","room","trunk","head","head","ambig","head",
                    "ambig","trunk","head","room","trunk","trunk","trunk","ambig","head","NA","NA","NA","NA")


#writing to files, and plotting
write.table(fitAllRF, file = "fitAllRF.csv", sep=";", row.names = FALSE)





### compare 2 fit methods corrected and uncorrected ------

fitAllRF["bestFit"] = NA
for(i in 1:nrow(fitAllRF)){
fitAllRF$bestFit[i] = min(c(fitAllRF$fitTrunk[i],fitAllRF$fitHead[i],fitAllRF$fitRoom[i]))
}

fitAllRFnotcorr["bestFit"] = NA
for(i in 1:nrow(fitAllRFnotcorr)){
  fitAllRFnotcorr$bestFit[i] = min(c(fitAllRFnotcorr$fitTrunk[i],fitAllRFnotcorr$fitHead[i],fitAllRFnotcorr$fitRoom[i]))
}


mean(fitAllRF$bestFit,na.rm=T)
mean(fitAllRFnotcorr$bestFit,na.rm=T)

t.test(fitAllRF$bestFit,fitAllRFnotcorr$bestFit)


###
sRF = subset(fitAllRF,participant==input$participant & task==input$task)

vect = with(sRF,c(fitHead,fitTrunk,fitRoom))
sorted = sort(vect)
ratio = format((sorted[2]/sorted[1]),digits=3)
ratio2 = format((sum(sorted)/sorted[1]),digits=3)
outTxt = paste("Proportions - (Next to best fit/best fit)= ",ratio," and (Sum of all/best fit)= ",ratio2)
outTxt