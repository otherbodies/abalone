patchToData = "C:/OTHERBODIES/bergen-research-data/data-analysis-may-2014/allfiles3/"

# function that takes pre-prepared files listed in metatable.csv, updates conditions, and binds them into one file 
# TODO : CAUTION! : for mini need to do this manually or write additional function

# this is written as a function, but better run the code in chunks, directly from here
loadDataFrames = function(patchToData)
{
  patchToMetatable = paste(patchToData,"metatable.csv",sep="")
  metatable = read.csv(patchToMetatable,head=TRUE,sep=";")
  
  # down here - number of rows in metatable.csv minus 1 //nrow command gives exactly this number
  nrowsmetatable = nrow(metatable)
  for (i in 1:nrowsmetatable){
    
    currentDataPatch = paste(patchToData,metatable$Filename[i],".csv",sep="")
    dat = read.csv(currentDataPatch,head=TRUE,sep=";")
  
    str = c(28:39,93:104,119:130,145:156)
    hd = c(54:65,80:91,106:117,158:169)
    tr = c(41:52,67:78,132:143,171:182)
    sys = c(1:27,40,53,66,79,92,105,118,131,144,157,170,183)
    cond = c(1:183)
    for (i in str){cond[i]="str"}
    for (i in hd){cond[i]="hd"}
    for (i in tr){cond[i]="tr"}
    for (i in sys){cond[i]="sys"}
    dat$condition=cond
    factor(dat$condition)
    
    write.table(dat, file = currentDataPatch, sep=";", row.names = FALSE)
  }
  
  ###
  
  for (i in 1:nrowsmetatable){
    
    currentDataPatch = paste(patchToData,metatable$Filename[i],".csv",sep="")
    dat = read.csv(currentDataPatch,head=TRUE,sep=";")
    con = metatable$Condition[i]
    group = metatable$Group[i]
    part = metatable$Participant[i]
    
    task = c(1:183)
    for (j in task){task[j]= as.character(con)}
    type = c(1:183)
    for (j in type){type[j]= as.character(group)}
    participant = c(1:183)
    for (j in participant){participant[j]= as.character(part)}
    
    dat$task = task
    dat$type = type
    dat$participant = participant
    
    # factor(dat$task)
    
    write.table(dat, file = currentDataPatch, sep=";", row.names = FALSE)
}
  
  # before moving on - copy manually fixed mini files first to the directory!
  # reading the separate dataframes, and assigning names to them from metadata file
for (i in 1:nrowsmetatable){
  currentDataPatch = paste(patchToData,metatable$Filename[i],".csv",sep="")
  dat = read.csv(currentDataPatch,head=TRUE,sep=";")
  name = toString(metatable$Filename[i])
  assign(name,dat)
}

  ## merging a list of data frames into one - this is point "Create whole data sheet" from documentation
  #  

  listframes = list(Co1_months, Co1_crazy, Co1_mini, Co2_months, Co2_crazy, Co2_mini, Co3_months, 
  Co3_crazy,   Co3_mini,    Co4_months,  Co4_crazy,   Co4_mini,    Co5_months,  Co5_crazy,  
  Co5_mini,    Co6_months,  Co6_crazy,   Co6_mini,    Co7_months,  Co7_crazy,   Co7_mini,  
   Co8_months,  Co8_crazy,   Co8_mini,    Co9_months,  Co9_crazy,   Co9_mini, Co10_months, Co10_crazy, Co10_mini,  Sa4_months,
   Sa5_months,  Sa6_months,  Sa6_crazy,   Sa6_mini,    Sa7_months,  Sa7_crazy,   Sa7_mini,   
   Sa9_months,  Sa9_crazy,   Sa9_mini,    Sa11_months, Sa11_crazy, Sa11_mini, Sa14_months, Sa1_crazy,   Sa1_mini,   
   Sa13_crazy,  Sa13_mini,   Sa8_crazy,   Sa8_mini,    Sa20_months, Sa20_crazy,  Sa20_mini,  
   Sa21_months, Sa21_crazy,  Sa21_mini, Sa22_months, Sa22_crazy, Sa22_mini, Sa23_months, Sa23_crazy, 
  Sa23_mini, Sa24_months, Sa24_crazy, Sa24_mini, Sa25_months, Sa25_crazy, Sa25_mini, Sa26_months) 
  
  #listframes = list(Co1_mini,Co1_months,Co1_crazy)
  wholedata = do.call("rbind.data.frame",listframes)
  wholedata$m = factor(wholedata$m,labels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","sys1","sys2","sys3","sys4"))

  patchToWholeData = paste(patchToData,"wholedata.csv",sep="")
  write.table(wholedata, file = patchToWholeData, sep=";", row.names = FALSE)
  ####
  
########## !!! Re-run the code from this point only! Before some manual changes for MINI and Sa14 were applied!!!  


wholedata = read.csv("wholedata.csv",head=TRUE,sep=";")


# adding block number information - not finished
addBlockInfo = function(sub){
 
block = c(1:183)

block1 = c(28:39,54:65,41:52)           
block2 = c(93:104,80:91,67:78)                
block3 = c(119:130,132:143,106:117)   
block4 = c(145:156,158:169,171:182)
blocksys = c(1:27,40,53,66,79,92,105,118,131,144,157,170,183)

for (i in block1){block[i]="block1"}
for (i in block2){block[i]="block2"}
for (i in block3){block[i]="block3"}
for (i in block4){block[i]="block4"}
for (i in blocksys){block[i]="blocksys"}

block

block = rep(block,times=47)
}
wholedataBlock = subset(wholedata,task != "mini")

wholedataBlock$block = block


#####

# data cleaning and outlier removal procedures  ------------------------------

# simple procedure - system fault's outliers
# first clear outlier column 
for (i in 1:nrow(wholedata)){wholedata$outlier[i]=0}

out = which(wholedata$z==0 | wholedata$z>3000)
for (i in out){wholedata$outlier[i]=1}
# how many system outliers are there - without mini, and/or system trials
systemOutlierCount = subset(wholedata, condition !="sys" & (outlier=="1")) #list of system outliers
write.table(systemOutlierCount, "systemOutliers.csv", sep=";", row.names = FALSE)
nrow(systemOutlierCount) #number of system outliers


## advanced outlier procedure - NEW
library(plyr)
advOutlierRemoval = function(subs){
  
  localMean = c(mean(subs$z),mean(subs$X),mean(subs$y))
  distanceList =  data.frame(distances=numeric(nrow(subs))) 
  
  for (i in 1:nrow(subs)){
  localPoint = c(subs$z[i],subs$X[i],subs$y[i])
  distance = dist(rbind(localMean,localPoint))
  distanceList$distances[i]=distance[1]
  }
  
  avDist = mean(distanceList$distance)
  
  for(i in 1:nrow(subs)){
    
    outlierRatio = distanceList$distances[i]/avDist
    if (outlierRatio>2.5){
      subs$outlier[i] = 1
    }
  }
  return(subs)
}

wholedout2 = subset(wholedata, outlier !="1" & condition !="sys" & task != "mini")
wholedout3 = wholedout2[-29,]
wholeAdvOut = ddply(wholedout3,.(participant,task,condition),advOutlierRemoval)

write.table(wholeAdvOut, file="wholeAdvOut.csv", sep=";", row.names = FALSE)

advOutliers = subset(wholeAdvOut,outlier==1)
write.table(advOutliers, file="advOutliers.csv", sep=";", row.names = FALSE)

# adding column with - who did all, who only months, who only crazy
wholedata["rounds"] = NA 
monthsOnly = which(wholedata$participant %in% c("Sa4","Sa5","Sa14","Sa26"))
for (i in monthsOnly){wholedata$rounds[i]="monthsOnly"}
crazyOnly = which(wholedata$participant %in% c("Sa1","Sa8","Sa13"))
for (i in crazyOnly){wholedata$rounds[i]="crazyOnly"}
both = which(wholedata$participant %in% c("Co1","Co2","Co3","Co4","Co5","Co6","Co7","Co8","Co9","Co10","Sa6","Sa7","Sa9",
                                          "Sa11","Sa20","Sa21","Sa22","Sa23","Sa24","Sa25"))
for (i in both){wholedata$rounds[i]="both"}


# saving the wholedata once again
write.table(wholedata, file = patchToWholeData, sep=";", row.names = FALSE)


# adding an option to specify output filenames - i.e. when only a subset of data will be analyzed
fileNameAdd = "bothRounds"


# basic analysis - subsetting, means, sds -----------------------------------
# this is after both rounds of outlier removal 

#!!! fixing missing september in Co1 head - putting NA value in one trial instead of an outlier   
wholedata[62,2]=NA
wholedata[62,3]=NA
wholedata[62,4]=NA
wholedata[62,5]=0

# wholedata without outliers and irrelevant trials
#wholedout = subset(wholedata, outlier !="1" & condition !="sys") ## refactor this - recomputing again after improved outlier removal

wholedout = subset(wholeAdvOut, outlier !="1")
# here maybe inserting a row with NA for september co1 months
co1missingsep = c("sep",NA,NA,NA,0,"hd","month","control","Co1","both")
wholedout = rbind(wholedout[1:140,],co1missingsep,wholedout[-(1:140),])
rownames(wholedout)=NULL
wholedout$z = as.numeric(wholedout$z)
wholedout$X = as.numeric(wholedout$X)
wholedout$y = as.numeric(wholedout$y)

means = aggregate(wholedout[c("z","X","y")],wholedout[c("m","condition","task","participant","type","rounds")],mean)
medians = aggregate(wholedout[c("z","X","y")],wholedout[c("m","condition","task","participant")],median)
sds = aggregate(wholedout[c("z","X","y")],wholedout[c("m","condition","task","participant","type","rounds")],sd)
lengths = aggregate(wholedout[c("z","X","y")],wholedout[c("m","condition","task","participant","type","rounds")],length)
avofsds = aggregate(sds[c("z","X","y")],sds[c("condition","task","participant")],mean,na.rm=TRUE)
sdofsds = aggregate(sds[c("z","X","y")],sds[c("condition","task","participant")],sd,na.rm=TRUE)

# write above to files - this is saved in RStudioProjects/Abalone - need to copy somewhere 
write.table(means, file = "means.csv", sep=";", row.names = FALSE)
write.table(medians, file = "medians.csv", sep=";", row.names = FALSE)
write.table(sds, file = "sds.csv", sep=";", row.names = FALSE)
write.table(lengths, file = "lengths.csv", sep=";", row.names = FALSE)
write.table(avofsds, file = "avofsds.csv", sep=";", row.names = FALSE)
write.table(sdofsds, file = "sdofsds.csv", sep=";", row.names = FALSE)

############ saving for Unity - for now, has to be comma separated
wholedout3 = subset(wholeAdvOut, outlier !="1")
#wholedout3 = subset(wholedataBlock, outlier !="1" & condition !="sys")
#wholedout3 = wholedout3[-29,] # removing the one with NA
write.table(wholedout3, file = "wholedout-unity.csv", sep=",", row.names = FALSE)
############



############ pooled sd's
 tobepooled = sds
 tobepooled$length = lengths$X
 tobepooled$df = tobepooled$length-1


library(plyr)
pooledsds3 = ddply(tobepooled,c("participant","condition","task","type","rounds"),summarize,pooled_z=weighted.mean(z,df),pooled_X=weighted.mean(X,df),pooled_y=weighted.mean(y,df))
pooledsds3$overall = rowMeans(pooledsds3[,c("pooled_z","pooled_X","pooled_y")])
pooledsds3 = subset(pooledsds3,task!="mini")
pooledsds3["type"]=NA
pooledsds3$type[1:60]="control"
pooledsds3$type[61:141]="synaesth" #69 crazy 70(?) months 
write.table(pooledsds3, file = "pooledsds.csv", sep=";", row.names = FALSE)
pooledSummary = aggregate(pooledsds3[c("pooled_z","pooled_X","pooled_y","overall")],pooledsds3[c("condition","type","task")],mean)



# computing 45 degree model shift -----------------------------------------


  

## computing model points for 45deg shift and writing to file

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


# computing model 45 deg shift from straight and saving to file 

mstr = subset(means,condition=="str" & task!="mini")
  #row.names(mstr)=NULL
  shiftedMeans = do.call(function(m,condition,task,participant,z,X,y,...) computeShift(mstr$m,mstr$condition,mstr$task,mstr$participant,mstr$z,mstr$X,mstr$y),mstr)
 
  write.table(shiftedMeans, file = "model-shifts.csv", sep=";", row.names = FALSE)


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
  
  DifX = abs(mstr$X - mhd$X)
  Dify = abs(mstr$y - mhd$y)
  Difz = abs(mstr$z - mhd$z)
  DifDist = sqrt((mstr$X - mhd$X)^2 + (mstr$z - mhd$z)^2)
  difStaticHead = data.frame(mstr$m,mstr$condition,mstr$task,mstr$participant,Difz,DifX,Dify,DifDist)
  colnames(difStaticHead) = c("m","condition","task","participant","z","X","y","Dist")
  
  
  avDifSwingTrunk = aggregate(difSwingTrunk[c("z","X","y","Dist")],difSwingTrunk[c("task","participant")],mean,na.rm=TRUE)
  avDifSwingHead = aggregate(difSwingHead[c("z","X","y","Dist")],difSwingHead[c("task","participant")],mean,na.rm=TRUE)
  avDifStaticTrunk = aggregate(difStaticTrunk[c("z","X","y","Dist")],difStaticTrunk[c("task","participant")],mean,na.rm=TRUE)
  avDifStaticHead = aggregate(difStaticHead[c("z","X","y","Dist")],difStaticHead[c("task","participant")],mean,na.rm=TRUE)
  
  fitForTrunk = (avDifSwingTrunk$Dist + avDifStaticHead$Dist)/2
  fitForHead = (avDifSwingHead$Dist + avDifStaticTrunk$Dist)/2
  fitForRoom = (avDifStaticHead$Dist + avDifStaticTrunk$Dist)/2
  fitAllRF = data.frame(avDifStaticHead$task,avDifStaticHead$participant,fitForHead,fitForTrunk,fitForRoom)
  colnames(fitAllRF) = c("task","participant","fitHead","fitTrunk","fitRoom")
  
  #formating - less decimal numbers
  fitAllRF$fitTrunk = as.numeric(format(fitAllRF$fitTrunk,digits=4)) 
  fitAllRF$fitHead = as.numeric(format(fitAllRF$fitHead,digits=4)) 
  fitAllRF$fitRoom = as.numeric(format(fitAllRF$fitRoom,digits=4)) 
  #writing to files, and plotting
  write.table(fitAllRF, file = "fitAllRF.csv", sep=";", row.names = FALSE)
  
  # export for unity3d
  write.table(fitAllRF, file = "C://allfiles2//fitAllRF.txt", sep=",", row.names = FALSE)
  

#### making main table ------------------
  fitForTrunkSimple = avDifSwingTrunk$Dist
  fitForHeadSimple = avDifSwingHead$Dist



# bestfit ratio
fitAllRF["ratio"] = NA
fitAllRF["bestfit"] = NA
for(i in 1:nrow(fitAllRF)){
  sorted = sort(c(fitAllRF$fitTrunk[i],fitAllRF$fitHead[i],fitAllRF$fitRoom[i]))
  ratio = format((sorted[2]/sorted[1]),digits=3) 
  if(sorted[1]==fitAllRF$fitTrunk[i]){fitAllRF$bestfit[i]="trunk"}
  if(sorted[1]==fitAllRF$fitHead[i]){fitAllRF$bestfit[i]="head"}
  if(sorted[1]==fitAllRF$fitRoom[i]){fitAllRF$bestfit[i]="room"}
  fitAllRF$ratio[i] = ratio
}

library("reshape2", lib.loc="~/R/win-library/3.1")

# angles - regression lines (algorithm is in clustering.R file), here only data transformations
angSlopWide$str = as.numeric(angSlopWide$str)
angSlopWide$hd = as.numeric(angSlopWide$hd)
angSlopWide$tr = as.numeric(angSlopWide$tr)
angSlopWide["strHd"] = angSlopWide$str - angSlopWide$hd
angSlopWide["strTr"] = angSlopWide$str - angSlopWide$tr
angSlopWide["diffOfDiffs"] = angSlopWide$strHd - angSlopWide$strTr


altangleSlopeTemp$angleStr = as.numeric(altangleSlopeTemp$angleStr)
altangleSlopeTemp$angleTr = as.numeric(altangleSlopeTemp$angleTr)
altangleSlopeTemp$angleHd = as.numeric(altangleSlopeTemp$angleHd)
altangleSlopeTemp["furthest_str_hd"] = altangleSlopeTemp$angleStr - altangleSlopeTemp$angleHd
altangleSlopeTemp["furthest_str_tr"] = altangleSlopeTemp$angleStr - altangleSlopeTemp$angleTr
altangleSlopeTemp["furthest_diffOfDiffs"] = altangleSlopeTemp$furthest_str_hd - altangleSlopeTemp$furthest_str_tr

## making the wide format

  mainTable1 = dcast(fitAllRF, participant ~ task, value.var = "fitHead")
  mainTable2 = dcast(fitAllRF, participant ~ task, value.var = "fitTrunk")
  mainTable3 = dcast(fitAllRF, participant ~ task, value.var = "fitRoom")
  mainTable4 = dcast(fitAllRF, participant ~ task, value.var = "ratio")
  mainTable41 = dcast(fitAllRF, participant ~ task, value.var = "bestfit")
  mainTable5 = dcast(angSlopWide, participant ~ task, value.var = "strHd")
  mainTable6 = dcast(angSlopWide, participant ~ task, value.var = "strTr")
  mainTable7 = dcast(angSlopWide, participant ~ task, value.var = "diffOfDiffs")
mainTable8 = dcast(altangleSlopeTemp, participant ~ task, value.var = "furthest_str_hd")
mainTable9 = dcast(altangleSlopeTemp, participant ~ task, value.var = "furthest_str_tr")
mainTable10 = dcast(altangleSlopeTemp, participant ~ task, value.var = "furthest_diffOfDiffs")

mainTable = merge(mainTable1,mainTable2,by="participant")
  mainTable = merge(mainTable,mainTable3,by="participant")
  mainTable = merge(mainTable,mainTable4,by="participant")
mainTable = merge(mainTable,mainTable41,by="participant")
mainTable = merge(mainTable,mainTable5,by="participant")
mainTable = merge(mainTable,mainTable6,by="participant")
mainTable = merge(mainTable,mainTable7,by="participant")
mainTable = merge(mainTable,mainTable8,by="participant")
mainTable = merge(mainTable,mainTable9,by="participant")
mainTable = merge(mainTable,mainTable10,by="participant")
  colnames(mainTable)=c("participant","head_fit_horse","head_fit_month","trunk_fit_horse","trunk_fit_month","room_fit_horse","room_fit_month","ratio_crazy","ratio_month",
"bestfit_horse","bestfit_month","angle_str_hd_crazy","angle_str_hd_month","angle_str_tr_crazy","angle_str_tr_month","angle_diff_of_diffs_crazy","angle_diff_of_diffs_month",
"furthest_strhd_horse","furthest_strhd_month","furthest_strtr_horse","furthest_strtr_month","furthest_diffofdiffs_horse","furthest_diffofdiffs_month","type")

mainTable["type"]=NA
mainTable$type[1:10]="control"
mainTable$type[11:27]="synaesthete"

#rel_trunk is trunk fit divided by mean of other two
mainTable["rel_trunk"]=NA
mainTable$rel_trunk=(mainTable$trunk_fit_month/((mainTable$head_fit_month+mainTable$room_fit_month)/2))
mainTable["rel_head"]=NA
mainTable$rel_head=(mainTable$head_fit_month/((mainTable$trunk_fit_month+mainTable$room_fit_month)/2))
mainTable["rel_room"]=NA
mainTable$rel_room=(mainTable$room_fit_month/((mainTable$head_fit_month+mainTable$trunk_fit_month)/2))

#rel_trunk_conserv is trunk fit divided by the one with smaller difference from trunk fit
#example:  min(c(abs(9-5),abs(9-11)))

mainTable["rel_trunk_conserv"]=NA
mainTable$rel_trunk_conserv = mainTable$trunk_fit_month/(min(c(abs(mainTable$trunk_fit_month-mainTable$head_fit_month),abs(mainTable$trunk_fit_month-mainTable$room_fit_month)  )   ))
##

##new fit based on regression line angles
mainTable["trunk_fit_reg"]=NA
mainTable$trunk_fit_reg = with(mainTable,abs(45-angle_str_tr_month)+abs(angle_str_hd_month))
mainTable["head_fit_reg"]=NA
mainTable$head_fit_reg = with(mainTable,abs(45-angle_str_hd_month)+abs(angle_str_tr_month))
mainTable["room_fit_reg"]=NA
mainTable$room_fit_reg = with(mainTable,abs(angle_str_tr_month)+abs(angle_str_hd_month))

##new fit based on furthest point angles
mainTable["trunk_fit_fur"]=NA
mainTable$trunk_fit_fur = with(mainTable,abs(45-furthest_strtr_month)+abs(furthest_strhd_month))
mainTable["head_fit_fur"]=NA
mainTable$head_fit_fur = with(mainTable,abs(45-furthest_strhd_month)+abs(furthest_strtr_month))
mainTable["room_fit_fur"]=NA
mainTable$room_fit_fur = with(mainTable,abs(furthest_strtr_month)+abs(furthest_strhd_month))

## new fit - crazy - based on regression line angles
mainTable["trunk_fit_reg_cr"]=NA
mainTable$trunk_fit_reg_cr = with(mainTable,abs(45-angle_str_tr_crazy)+abs(angle_str_hd_crazy))
mainTable["head_fit_reg_cr"]=NA
mainTable$head_fit_reg_cr = with(mainTable,abs(45-angle_str_hd_crazy)+abs(angle_str_tr_crazy))
mainTable["room_fit_reg_cr"]=NA
mainTable$room_fit_reg_cr = with(mainTable,abs(angle_str_tr_crazy)+abs(angle_str_hd_crazy))

## new fit - crazy - based on furthest point angles
mainTable["trunk_fit_fur_cr"]=NA
mainTable$trunk_fit_fur_cr = with(mainTable,abs(45-furthest_strtr_horse)+abs(furthest_strhd_horse))
mainTable["head_fit_fur_cr"]=NA
mainTable$head_fit_fur_cr = with(mainTable,abs(45-furthest_strhd_horse)+abs(furthest_strtr_horse))
mainTable["room_fit_fur_cr"]=NA
mainTable$room_fit_fur_cr = with(mainTable,abs(furthest_strtr_horse)+abs(furthest_strhd_horse))



# bestfit fur ratio
mainTable["best_ratio_fur"] = NA
mainTable["bestfit_fur"]=NA
for(i in 1:nrow(mainTable)){
  sorted = sort(c(mainTable$trunk_fit_fur[i],mainTable$head_fit_fur[i],mainTable$room_fit_fur[i]))
  ratio = format((sorted[2]/sorted[1]),digits=3) 
  if(sorted[1]==mainTable$trunk_fit_fur[i] & is.na(sorted[1])==0){mainTable$bestfit_fur[i]="trunk"}
  if(sorted[1]==mainTable$head_fit_fur[i] & is.na(sorted[1])==0){mainTable$bestfit_fur[i]="head"}
  if(sorted[1]==mainTable$room_fit_fur[i] & is.na(sorted[1])==0){mainTable$bestfit_fur[i]="room"}
  mainTable$best_ratio_fur[i] = ratio
}




## here - mainTable analysed_by column was edited manually in excel and reloaded - refactor this

## - who did all
mainTable["rounds"] = NA 
monthsOnly = which(mainTable$participant %in% c("Sa4","Sa5","Sa14","Sa26"))
for (i in monthsOnly){mainTable$rounds[i]="monthsOnly"}
crazyOnly = which(mainTable$participant %in% c("Sa1","Sa8","Sa13"))
for (i in crazyOnly){mainTable$rounds[i]="crazyOnly"}
both = which(mainTable$participant %in% c("Co1","Co2","Co3","Co4","Co5","Co6","Co7","Co8","Co9","Co10","Sa6","Sa7","Sa9",
                                          "Sa11","Sa20","Sa21","Sa22","Sa23","Sa24","Sa25"))
for (i in both){mainTable$rounds[i]="both"}
##


## creating mixed fur reg fits (based on analyse_by column)
mainTable["trunk_fit_mixed"] = NA
mainTable["head_fit_mixed"] = NA
mainTable["room_fit_mixed"] = NA
for (i in 1:nrow(mainTable)){
  if (mainTable$analyse_by[i]=="reg"){
    mainTable$trunk_fit_mixed[i]= mainTable$trunk_fit_reg[i]
    mainTable$head_fit_mixed[i]= mainTable$head_fit_reg[i]
    mainTable$room_fit_mixed[i]= mainTable$room_fit_reg[i]
  }
 
  
  if (mainTable$analyse_by[i]=="fur"){
    mainTable$trunk_fit_mixed[i]= mainTable$trunk_fit_fur[i]
    mainTable$head_fit_mixed[i]= mainTable$head_fit_fur[i]
    mainTable$room_fit_mixed[i]= mainTable$room_fit_fur[i]
  } 
}

##


write.table(mainTable, file = "mainTable.csv", sep=";", row.names = FALSE)








############





  #ggplot plotting
  gg = ggplot(fitAllRF,aes(x=fitHead,y=fitTrunk,colour=type))+coord_equal()+geom_point(size=3)
  gg = gg +scale_x_continuous(limits=c(50, 300)) #adjust this to show all data points nicely
  gg +geom_text(aes(label=participant),hjust=0, vjust=0)



  ## preparing and plotting comparison to model shift - in process...
aa = merge(avDifSwingTrunk,avDifSwingHead,by="participant")
bb = merge(avDifStaticTrunk,avDifStaticHead,by="participant")
colnames(aa) = c("participant","task","z.SwingTr","X.SwingTr","y.SwingTr","Dist.SwingTr","_task","z.SwingHd","X.SwingHd","z.SwingHd","Dist.SwingHd")
colnames(bb) = c("participant","task","z.StaticTr","X.StaticTr","y.StaticTr","Dist.StaticTr","_task","z.StaticHd","X.StaticHd","z.StaticHd","Dist.StaticHd")
cc = merge(aa,bb,by="participant")
cc["type"]=NA
cc$type[1:9]="control"
cc$type[10:19]="synaesth" #19 for crazy, 23 for months
#adding a column that tells which subjects did all: months crazy and mini
cc$did_all = 0
temp_didall = which(cc$participant %in% c("Sa4","Sa5","Sa11","Sa14"))
cc$did_all[temp_didall]=1


gg= ggplot(cc,aes(x=Dist.SwingHd,y=Dist.StaticTr,colour=type))+coord_equal()+geom_point(size=5)
#gg +geom_point(data=cc[temp_didall,],aes(x=Dist.SwingHd,y=Dist.StaticTr),colour="black",size=3)
gg +geom_text(aes(label=participant),hjust=0, vjust=0)
#gg +geom_smooth(method="lm")

gg= ggplot(cc,aes(x=Dist.SwingTr,y=Dist.StaticHd,colour=type))+coord_equal()+geom_point(size=5)
gg +geom_point(data=cc[temp_didall,],aes(x=Dist.SwingTr,y=Dist.StaticHd,colour="black",size=3)
gg +geom_text(aes(label=participant),hjust=0, vjust=0)

#trying with linear model
gg= ggplot(cc,aes(x=Dist.SwingHd,y=Dist.StaticTr))+coord_equal()+geom_point(data=cc,aes(size=5,colour=type))
#gg +geom_point(data=cc[temp_didall,],aes(x=Dist.SwingHd,y=Dist.StaticTr),colour="black",size=3)
#gg +geom_text(aes(label=participant),hjust=0, vjust=0)
gg= gg +geom_smooth(method="lm",se=FALSE)
gg

# messy - tried here to plot together two above scatterplots...
gg= ggplot(cc,aes(x=Dist.SwingHd,y=Dist.StaticTr,colour=type))+coord_equal()+geom_point(size=3)
gg= gg +geom_text(aes(label=participant),hjust=0, vjust=0)
gg= gg +geom_point(data=cc,aes(x=Dist.SwingTr,y=Dist.StaticHd,colour=type),shape=8,size=3)
gg +geom_text(aes(label=participant),hjust=0, vjust=0)

### some older stuff..
par(mar=c(5,4,4,4))
plot(fitAllRFX$fitHeadX,fitAllRFX$fitTrunkX,xlab="Fit to Head X: low values = better fit",ylab="Fit to Trunk X")
text(fitAllRFX$fitHeadX,fitAllRFX$fitTrunkX,labels=fitAllRFX$participant)


fitClu = na.omit(fitAllRF)
cluLabels = paste(as.character(fitClu$participant),as.character(fitClu$task))
clu2 = hclust(dist(fitClu[c("fitHead","fitTrunk","fitRoom")]),method="ward.D")
plot(clu2,labels=cluLabels,main="hierarchical cluster ward")
groups = cutree(clu2,3)
rect.hclust(clu2, k=3, border="red")

fitClu3d = fitClu[c("fitHead","fitTrunk","fitRoom")]
scatterplot3d(fitClu3d,color=groups)


## anovas on model fit
fitAllRF["group"]=NA
fitAllRF$group[1:20]="control"
fitAllRF$group[21:47]="synaesth"



#plotting 3d - adding column with colours
fitAllRF["typecolour"]="green"
#fitAllRF$typecolour[1:9]="red"
#fitAllRF$typecolour[10:19]="green"

##plotting 3d ant to the web  ------------------------------
library("rgl", lib.loc="C:/Users/andrzej/Documents/R/win-library/3.0")
plot3d(fitAllRF$fitHead,fitAllRF$fitTrunk,fitAllRF$fitRoom)

with(fitAllRF,plot3d(fitHead,fitTrunk,fitRoom,type="s",col=typecolour,radius=5))
with(fitAllRF,text3d(fitHead,fitTrunk,fitRoom,paste(participant,task),cex=0.7))
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), 
                                      width=700,height=700), sep=""))



mainTable["plot3d_col_month"]=NA

for (i in 1:nrow(mainTable)){
  if (mainTable$bestfit_month[i]=="trunk" & is.na(mainTable$bestfit_month[i])==0 ){mainTable$plot3d_col_month[i]="green"}
  if (mainTable$bestfit_month[i]=="head" & is.na(mainTable$bestfit_month[i])==0){mainTable$plot3d_col_month[i]="blue"}
  if (mainTable$bestfit_month[i]=="room" & is.na(mainTable$bestfit_month[i])==0){mainTable$plot3d_col_month[i]="red"}
 
}






with(mainTable,plot3d(trunk_fit_reg,head_fit_reg,room_fit_reg,type="s",col=plot3d_col_month,radius=2))
#with(mainTable,text3d(trunk_fit_reg,head_fit_reg,room_fit_reg,participant,cex=0.7))
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), 
                                      width=700,height=700), sep=""))



with(mainTable,plot3d(trunk_fit_fur,head_fit_fur,room_fit_fur,type="s",col=plot3d_col_month,radius=2))
with(mainTable,text3d(trunk_fit_fur,head_fit_fur,room_fit_fur,participant,cex=0.5))
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), 
                                      width=700,height=700), sep=""))



with(fitClu,plot3d(trunk_fit_fur,head_fit_fur,room_fit_fur,type="s",col=plot3d_col_month,radius=2))
with(fitClu,text3d(trunk_fit_fur,head_fit_fur,room_fit_fur,participant,cex=0.5))
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), 
                                      width=700,height=700), sep=""))


with(fitClu,plot3d(trunk_fit_mixed,head_fit_mixed,room_fit_mixed,type="s",col=plot3d_col_month,radius=2))
with(fitClu,text3d(trunk_fit_mixed,head_fit_mixed,room_fit_mixed,participant,cex=0.5))
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), 
                                      width=700,height=700), sep=""))

### here - playing with ellipsoids
open3d()

meanvec=c(mean(fitClu$trunk_fit_mixed),mean(fitClu$head_fit_mixed),mean(fitClu$room_fit_mixed))
sigma=cov(dat)
plot3d(ellipse3d(x=sigma,alpha=0.1,centre=meanvec))
plot3d(ellipse3d(x=sigma,centre=c(1,1,1)),add=T)
with(fitClu,plot3d(trunk_fit_mixed,head_fit_mixed,room_fit_mixed,add=TRUE,type="s",col=plot3d_col_month,radius=2))



plot3d(,add=T,type="s",radius=2)
plot3d(dat,add=T,type="s",radius=6,alpha=0.1)

# with(fitClu,text3d(trunk_fit_mixed,head_fit_mixed,room_fit_mixed,participant,cex=0.5))

browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), 
                                      width=700,height=700), sep=""))

##

## clustering furthest 
fitcluna_both = which(mainTable$rounds=="both" & is.na(mainTable$trunk_fit_fur)==0)
fitcluna = which(is.na(mainTable$trunk_fit_fur)==0)
fitClu = mainTable[fitcluna,]
fitClu_both = mainTable[fitcluna_both,]

cluLabels = paste(as.character(fitClu$participant),as.character(fitClu$bestfit_month))
clu2 = hclust(dist(fitClu[c("trunk_fit_fur","head_fit_fur","room_fit_fur")]),method="ward.D")
plot(clu2,labels=cluLabels,main="hierarchical cluster ward")
groups = cutree(clu2,3)
rect.hclust(clu2, k=3, border="red")


#mixed fit clustering
fitcluna = which(is.na(mainTable$trunk_fit_fur)==0)
fitClu = mainTable[fitcluna,]

cluLabels = paste(as.character(fitClu$participant),as.character(fitClu$bestfit_month))
clu2 = hclust(dist(fitClu[c("trunk_fit_mixed","head_fit_mixed","room_fit_mixed")]),method="ward.D")
plot(clu2,labels=cluLabels,main="hierarchical cluster ward")
groups = cutree(clu2,3)
rect.hclust(clu2, k=3, border="red")
##

##kmeans
# K-Means Cluster Analysis
fitkm <- kmeans(fitClu[c("trunk_fit_fur","head_fit_fur","room_fit_fur")], 3) # 3 cluster solution
# get cluster means
aggregate(fitClu[c("trunk_fit_fur","head_fit_fur","room_fit_fur")],by=list(fitkm$cluster),FUN=mean)
# append cluster assignment
fitClu["kmeans"]=NA

fitClu$kmeans = fitkm$cluster

fitmanova = manova(cbind(fitClu$trunk_fit_fur,fitClu$head_fit_fur,fitClu$room_fit_fur)~factor(fitClu$bestfit_fur))
summary.manova(fitmanova, test="Wilks")

summary.aov(fitmanova)

testaov = aov(trunk_fit_fur ~ bestfit_fur, data=fitClu)
summary(testaov)
TukeyHSD(testaov)

##manova pairwise multivariate comparisons, below example from Ch_analysis_of_variance.pdf tutorial
#summary(manova(cbind(mb, bh, bl, nh) ~ epoch, data = skulls, subset = epoch %in% c("c4000BC", "c3300BC")))

summary(manova(cbind(trunk_fit_fur,head_fit_fur,room_fit_fur) ~ bestfit_fur, data=fitClu, subset = bestfit_fur %in% c("head","trunk") ))
summary(manova(cbind(trunk_fit_fur,head_fit_fur,room_fit_fur) ~ bestfit_fur, data=fitClu, subset = bestfit_fur %in% c("head","room") ))
summary(manova(cbind(trunk_fit_fur,head_fit_fur,room_fit_fur) ~ bestfit_fur, data=fitClu, subset = bestfit_fur %in% c("room","trunk") ))
##

## manova for mixed 
fitmanova_mixed = manova(cbind(fitClu$trunk_fit_mixed,fitClu$head_fit_mixed,fitClu$room_fit_mixed)~factor(fitClu$bestfit_month))
fitmanova_mixed_both = manova(cbind(fitClu_both$trunk_fit_mixed,fitClu_both$head_fit_mixed,fitClu_both$room_fit_mixed)~factor(fitClu_both$bestfit_month))
fitmanova_fur_both = manova(cbind(fitClu_both$trunk_fit_fur,fitClu_both$head_fit_fur,fitClu_both$room_fit_fur)~factor(fitClu_both$bestfit_fur))

save(fitmanova_mixed,fitClu,fitClu_both,fitmanova_mixed_both,fitmanova_fur_both,file="data2.Rda")
##

##taking the largest value in fitallRF ----------------------
fitAllRF["largest"]=NA
for(i in 1:nrow(fitAllRF)){
vect = c(fitAllRF[i,3],fitAllRF[i,4],fitAllRF[i,5])
minval = which.min(vect)
if (minval==1){fitAllRF$largest[i]="red"}
if (minval==2){fitAllRF$largest[i]="blue"}
if (minval==3){fitAllRF$largest[i]="green"}
}



## anovas - for pooled sd's - consistency data
# mixed design 2x2x3 Anova - con/syn (between group) x month/crazy (within sub) x str/hd/tr (within sub)
pooledsds3$task = factor(pooledsds3$task) #to remove mini from factor levels
pooledsds3$condition = factor(pooledsds3$condition)

pooledsds4 = subset(pooledsds3,rounds=="both")
write.table(pooledsds4, file = "pooledsds.csv", sep=";", row.names = FALSE)

library("ggplot2", lib.loc="~/R/win-library/3.1")

gg = ggplot(pooled_month,aes(x=participant,y=overall,colour=type,shape=condition))+geom_point()
#gg = gg+facet_grid(. ~ type)
gg = gg+stat_smooth(method=lm,aes(group=type))
gg
ggsave(gg,file="sds-months.png",scale=2)





gg = ggplot(pooled_crazy,aes(x=participant,y=overall,colour=type,shape=condition))+geom_point()
#gg = gg+facet_grid(. ~ type)
gg = gg+stat_smooth(method=lm,aes(group=type))
gg
ggsave(gg,file="sds-crazy.png",scale=2)


with(pooledsds4,tapply(overall,list(type,task,condition),mean))


gg = ggplot(pooledsds4,aes(x=overall,fill=type))+geom_histogram(binwidth=1)
gg = gg+facet_wrap(~ task)
gg

gg = ggplot(pooledsds4,aes(x=overall,colour=type))+geom_density()
gg = gg+facet_wrap(~ task)
gg
ggsave(gg,file="sds-distribution.png",scale=2)

sdsanova = aov(overall ~ type*task*condition +Error(participant/(task*condition)), pooledsds4)
summary(sdsanova)
#boxplot(overall~task*condition*type,data=pooledsds3)

sdsanova3 = aov(overall ~ type, pooledsds3)
summary(sdsanova3)
with(pooledsds4,pairwise.t.test(overall,type,p.adj="none"))
pooled_month = subset(pooledsds4,task=="month")
pooled_crazy = subset(pooledsds4,task=="crazy")
with(pooled_month,t.test(overall~type))
with(pooled_crazy,t.test(overall~type))

library("lsr", lib.loc="~/R/win-library/3.1")
with(pooled_month,cohensD(overall~type))
with(pooled_crazy,cohensD(overall~type))


#loading summary info and saving as Rda file for load into markdown #move this at the end of script
participantSummary = read.csv("participants.csv",head=TRUE,sep=";")

save(participantSummary,wholedata,systemOutlierCount,advOutliers,pooledSummary,sumpool,means,shiftedMeans,fitAllRF,mainTable,wholedout_mini,fitmanova,file="data.Rda")
