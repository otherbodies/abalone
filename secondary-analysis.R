# in this file there are parts of the code removed from loadfromtimetable.R

# 0 - advanved outlier removal, old version of procedure
# 0.1 - some old summary of data code
# 0.2 - pooled sds old procedure
# 1 - compute differences between conditions in position
# 2 - t-tests for those differences
# 3 - angular shifts with all stuff related, trying algorithms etc. 
# 4 - testing block for model fit 

############

# advanced outlier removal procedure - OLD VERSION!
wholedoutsplit = split(wholedout2,list(factor(wholedout2$m),factor(wholedout2$condition),factor(wholedout2$task),factor(wholedout2$participant)))
wholedoutsplit2 = wholedoutsplit[sapply(wholedoutsplit,nrow)>0]

outlierMeasure = lapply(wholedoutsplit2, function(x) {(x$X - means2$X[means2$m==x$m & means2$condition==x$condition & means2$task==x$task & means2$participant==x$participant])/avofsds2$X[avofsds2$condition==x$condition & avofsds2$task==x$task & avofsds2$participant==x$participant]})
outlierCount = lapply(outlierMeasure, function(x){x>2})
bindedOutlierCount = as.data.frame(do.call("rbind",outlierCount)) 
## end of sd outlier region
outlierCount[1]


#testing how many points are outliers in terms of 3 or 2 SD from mean 
# wholedata2 without outliers and irrelevant trials
wholedout2 = subset(wholedata, outlier !="1" & condition !="sys" & task != "mini")
means2 = aggregate(wholedout[c("z","X","y")],wholedout[c("m","condition","task","participant")],mean)
medians2 = aggregate(wholedout[c("z","X","y")],wholedout[c("m","condition","task","participant")],median)
sds2 = aggregate(wholedout[c("z","X","y")],wholedout[c("m","condition","task","participant")],sd)
avofsds2 = aggregate(sds[c("z","X","y")],sds[c("condition","task","participant")],mean,na.rm=TRUE)
sdofsds2 = aggregate(sds[c("z","X","y")],sds[c("condition","task","participant")],sd,na.rm=TRUE)
aa = factor(wholedout$condition)


# pooled sds old procedure
# # pooledsds = sqrt( sum(tobepooled$z^2 * tobepooled$df) / sum(tobepooled$df) ) #this for each participant and axis of sd
# pool = function(func, column){(sum(func[,column] * func$df) / sum(func$df) )}
# #pooledsds = aggregate(tobepooled[c("z","X","y")],tobepooled[c("participant","condition")],FUN = pool,column="df")
# pooledsds = by(tobepooled,tobepooled[c("participant","condition")],FUN=pool,column="z")
# pooledsds2 = data.frame(do.call("rbind",
# testpool = subset(tobepooled,participant=="Co1" & condition=="hd")
# pool(testpool,"z")
# mean(testpool$z)



## compute differences block

# Compute differences -----------------------------------------------------



computeDifferences = function (argTask,arg_hd_tr){
  
  forsplit = subset(means,task==argTask)
  
  meansspl = split(forsplit,list(forsplit$participant))
  
  diffs_z = lapply(meansspl, function(func) func$z[func$condition=="str"]-func$z[func$condition==arg_hd_tr])
  diffs_X = lapply(meansspl, function(func) func$X[func$condition=="str"]-func$X[func$condition==arg_hd_tr])
  diffs_y = lapply(meansspl, function(func) func$y[func$condition=="str"]-func$y[func$condition==arg_hd_tr])
  
  bindeddiffs_z = as.data.frame(do.call("rbind",diffs_z))
  bindeddiffs_X = as.data.frame(do.call("rbind",diffs_X))
  bindeddiffs_y = as.data.frame(do.call("rbind",diffs_y))
  
  names(bindeddiffs_z)= c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  names(bindeddiffs_X)= c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  names(bindeddiffs_y)= c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  
  #nam = rownames(bindeddiffs_z)
  ext = paste(argTask,"_str_",arg_hd_tr,sep="")
  
  transformed_binded_z = data.frame(t(bindeddiffs_z))
  transformed_binded_X = data.frame(t(bindeddiffs_X))
  transformed_binded_y = data.frame(t(bindeddiffs_y))
  
  write.table(transformed_binded_z, file = paste("differences_z_",ext,".csv",sep=""), sep=";", row.names = FALSE)
  write.table(transformed_binded_X, file = paste("differences_X_",ext,".csv",sep=""), sep=";", row.names = FALSE)
  write.table(transformed_binded_y, file = paste("differences_y_",ext,".csv",sep=""), sep=";", row.names = FALSE)
  
  return("done")
}

computeDifferences("crazy","tr")
computeDifferences("crazy","hd")
computeDifferences("month","tr")
computeDifferences("month","hd")

### loading variables

diff_z_month_str_hd = read.csv("differences_z_month_str_hd.csv",head=TRUE,sep=";") 
diff_X_month_str_hd = read.csv("differences_X_month_str_hd.csv",head=TRUE,sep=";") 
diff_y_month_str_hd = read.csv("differences_y_month_str_hd.csv",head=TRUE,sep=";") 
diff_z_month_str_tr = read.csv("differences_z_month_str_tr.csv",head=TRUE,sep=";") 
diff_X_month_str_tr = read.csv("differences_X_month_str_tr.csv",head=TRUE,sep=";") 
diff_y_month_str_tr = read.csv("differences_y_month_str_tr.csv",head=TRUE,sep=";") 
diff_z_crazy_str_hd = read.csv("differences_z_crazy_str_hd.csv",head=TRUE,sep=";") 
diff_X_crazy_str_hd = read.csv("differences_X_crazy_str_hd.csv",head=TRUE,sep=";") 
diff_y_crazy_str_hd = read.csv("differences_y_crazy_str_hd.csv",head=TRUE,sep=";") 
diff_z_crazy_str_tr = read.csv("differences_z_crazy_str_tr.csv",head=TRUE,sep=";") 
diff_X_crazy_str_tr = read.csv("differences_X_crazy_str_tr.csv",head=TRUE,sep=";") 
diff_y_crazy_str_tr = read.csv("differences_y_crazy_str_tr.csv",head=TRUE,sep=";") 

##test code
t.test(diff_z_month_str_hd$Co2,diff_z_month_str_tr$Co2,paired = T)
library("lsr", lib.loc="~/R/win-library/3.1")

cohensD(diff_z_month_str_hd$Co2,diff_z_month_str_tr$Co2,method="paired")

##loop for t.tests and effect sizes
numberColumns = ncol(diff_z_month_str_hd)
resultT = vector("list",24)
resultD = vector("list",24)
testResults = data.frame(participant=character(24),axis=character(24),statistic=numeric(24),pvalue=numeric(24),cohenD=numeric(24),estimate=numeric(24),stringsAsFactors = F)

for(i in 1:numberColumns){
  #print("----------")
  #print(colnames(diff_z_month_str_hd)[i])
  #print(t.test(diff_z_month_str_hd[,i],diff_z_month_str_tr[,i],paired = T))
  #print("Cohen D effect size:")
  #print(cohensD(diff_z_month_str_hd[,i],diff_z_month_str_tr[,i],method="paired"))
  #resultT[[i]]=t.test(diff_z_month_str_hd[,i],diff_z_month_str_tr[,i],paired = T)
  #resultD[[i]]=cohensD(diff_z_month_str_hd[,i],diff_z_month_str_tr[,i],method="paired")
  # constructing testResults data frame
  test = t.test(diff_z_month_str_hd[,i],diff_z_month_str_tr[,i],paired = T)
  cohen = cohensD(diff_z_month_str_hd[,i],diff_z_month_str_tr[,i],method="paired")
  testResults$participant[i]=colnames(diff_z_month_str_hd)[i]
  testResults$axis = "z"
  testResults$statistic[i]=test$statistic
  testResults$pvalue[i]=format(test$p.value,scientific=FALSE)
  testResults$cohenD[i]=cohen
  testResults$estimate[i]=test$estimate
  
}

testForDifferences = function(dataVar,dataVar2,axis,task){
  assign("temp",dataVar)
  assign("temp2",dataVar2)
  
  testResults = data.frame(participant=character(24),axis=character(24),task=character(24),statistic=numeric(24),pvalue=numeric(24),cohenD=numeric(24),estimate=numeric(24),stringsAsFactors = F)
  
  if(task=="month"){participants = colnames(diff_z_month_str_hd)}
  else if(task=="crazy"){participants = colnames(diff_z_crazy_str_hd)}
  
  for(i in 1:length(participants)){
    
    test = t.test(temp[,i],temp2[,i],paired = T)
    cohen = cohensD(temp[,i],temp2[,i],method="paired")
    
    testResults$participant[i]=participants[i]
    testResults$axis = axis
    testResults$task = task
    testResults$statistic[i]=test$statistic
    testResults$pvalue[i]=format(test$p.value,scientific=FALSE)
    testResults$cohenD[i]=cohen
    testResults$estimate[i]=test$estimate
  }
  return(testResults)
}

tResultsMonthX = testForDifferences(diff_X_month_str_hd,diff_X_month_str_tr,"X","month")
tResultsMonthz = testForDifferences(diff_z_month_str_hd,diff_z_month_str_tr,"z","month")
tResultsMonthy = testForDifferences(diff_y_month_str_hd,diff_y_month_str_tr,"y","month")

tResultsCrazyX = testForDifferences(diff_X_crazy_str_hd,diff_X_crazy_str_tr,"X","crazy")
tResultsCrazyz = testForDifferences(diff_z_crazy_str_hd,diff_z_crazy_str_tr,"z","crazy")
tResultsCrazyy = testForDifferences(diff_y_crazy_str_hd,diff_y_crazy_str_tr,"y","crazy")


listDifframes = list(tResultsMonthz,tResultsMonthX,tResultsMonthy,tResultsCrazyz,tResultsCrazyX,tResultsCrazyy)
wholeDifferenceTests = do.call("rbind.data.frame",listDifframes)

#hack! removing 3 empty slots
wholeDifferenceTests = wholeDifferenceTests[-c(96,120,144),]

write.table(wholeDifferenceTests, file = "wholeDifferenceTests.csv", sep=";", row.names = FALSE)

length(which(wholeDifferenceTests$pvalue<0.05))
length(which(wholeDifferenceTests$pvalue<0.01))
length(which(wholeDifferenceTests$pvalue<0.001))


# testing assign function
assignTest = function(data){
  
  assign("temp",data)
  return(temp$Co2)
}
assignTest(diff_z_month_str_hd)


####### computing xz angular shits 
# test - take values from Co1, head, jan

# helper function 
rad2deg = function(rad) {
  return((180 * rad) / pi)
}



angularSimulation = function(sim_originz){
  originX=10
  originz=sim_originz
  strX=10
  strz=2000
  shiftX=
    shiftz=10
  
  distOriginStr = sqrt((originX-strX)^2+(originz-strz)^2)
  distOriginShift = sqrt((originX-shiftX)^2+(originz-shiftz)^2)
  distStrShift = sqrt((strX-shiftX)^2+(strz-shiftz)^2)
  
  angularShift = acos((distOriginStr^2+distOriginShift^2-distStrShift^2)/(2*distOriginStr*distOriginShift))
  angularShift = rad2deg(angularShift)
  angularShift
  
}

sim = data.frame(assumedPos=numeric(100),angle=numeric(100))
looper = seq(2050,2150,by=1)
for(i in 1:100){
  sim$assumedPos[i] = looper[i]
  sim$angle[i] = angularSimulation(looper[i])
}
write.table(sim, file = "simulation.csv", sep=";", row.names = FALSE)

## for plotting the simulations - temp code
x = c(0,0.7046729,15.4133580,0)
z = c(2100,2102.250,2046.750,2100)
type = c("partic","str","hd","partic")
lines = c("y","y","y","y")
test = data.frame(x,z,type)
ggplot(test,aes(x=x,y=z,color=type,group=lines))+coord_equal()+geom_point()+geom_path()

### compute angular shift function block
originX = 0
originz = 2160

computeAngularShift = function(m,task,participant,strX,strz,shiftX,shiftz){
  
  distOriginStr = sqrt((originX-strX)^2+(originz-strz)^2)
  distOriginShift = sqrt((originX-shiftX)^2+(originz-shiftz)^2)
  distStrShift = sqrt((strX-shiftX)^2+(strz-shiftz)^2)
  
  angularShift = acos((distOriginStr^2+distOriginShift^2-distStrShift^2)/(2*distOriginStr*distOriginShift))
  angularShift = format(rad2deg(angularShift),scientific=FALSE)
  
  returnFrame = data.frame(m,task,participant,angularShift)
}

mstr = subset(means,condition=="str" & task!="mini")
mhd = subset(means,condition=="hd" & task!="mini")
mtr = subset(means,condition=="tr" & task!="mini")
row.names(mstr)=NULL
row.names(mhd)=NULL
row.names(mtr)=NULL

angularShiftsHd = do.call(function(m,task,participant,strX,strz,shiftX,shiftz,...) computeAngularShift(mstr$m,mstr$task,mstr$participant,mstr$X,mstr$z,mhd$X,mhd$z),mstr)

angularShiftsTr = do.call(function(m,task,participant,strX,strz,shiftX,shiftz,...) computeAngularShift(mstr$m,mstr$task,mstr$participant,mstr$X,mstr$z,mtr$X,mtr$z),mstr)
#debug line
angularShiftsDebug = computeAngularShift("jan","crazy","Co1",mstr$X[13],mstr$z[13],mtr$X[13],mtr$z[13])

write.table(angularShiftsHd, file = "angularshiftshd2160.csv", sep=";", row.names = FALSE)
write.table(angularShiftsTr, file = "angularshiftstr2160.csv", sep=";", row.names = FALSE)
## for unity
write.table(angularShiftsHd, file = "angularshiftshdcoma.csv", sep=",", row.names = FALSE)
write.table(angularShiftsTr, file = "angularshiftstrcoma.csv", sep=",", row.names = FALSE)

## computing assumed position shifted 22.5deg  - to finish
theta = 22.5
theta = pi * 22.5 / 180      # convert to radians.
radius = 60
py = 2100 + radius * cos(theta)
px = 0 - radius * sin(theta)
py
px
###


angularShiftsHd = read.csv("angularshiftshd.csv",head=TRUE,sep=";")

angularShiftsHd$angularShift = as.numeric(as.character(angularShiftsHd$angularShift)) #for some reason had to reformat this in such strange way
angularShiftsTr$angularShift = as.numeric(as.character(angularShiftsTr$angularShift))

## computing correct sign
signs = mstr$X-mhd$X
which_signs = which(signs>0)
neg_signs = angularShiftsHd$angularShift[which_signs]*-1
angularShiftsHd$angularShift = replace(angularShiftsHd$angularShift,which_signs,neg_signs)

signs2 = mstr$X-mtr$X
which_signs2 = which(signs2>0)
neg_signs2 = angularShiftsTr$angularShift[which_signs2]*-1
angularShiftsTr$angularShift = replace(angularShiftsTr$angularShift,which_signs2,neg_signs2)
##


meanAngularHd = aggregate(angularShiftsHd[c("angularShift")],angularShiftsHd[c("task","participant")],mean,na.rm=T)
meanAngularTr = aggregate(angularShiftsTr[c("angularShift")],angularShiftsTr[c("task","participant")],mean,na.rm=T)
sdAngularTr = aggregate(angularShiftsTr[c("angularShift")],angularShiftsTr[c("task","participant")],sd,na.rm=T)
sdAngularHd = aggregate(angularShiftsHd[c("angularShift")],angularShiftsHd[c("task","participant")],sd,na.rm=T)
write.table(meanAngularHd, file = "meanAngularHd.csv", sep=";", row.names = FALSE)
write.table(meanAngularTr, file = "meanAngularTr.csv", sep=";", row.names = FALSE)
write.table(sdAngularTr, file = "sdAngularTr.csv", sep=";", row.names = FALSE)
write.table(sdAngularHd, file = "sdAngularHd.csv", sep=";", row.names = FALSE)
medianAngularHd = aggregate(angularShiftsHd[c("angularShift")],angularShiftsHd[c("task","participant")],median,na.rm=T)
medianAngularTr = aggregate(angularShiftsTr[c("angularShift")],angularShiftsTr[c("task","participant")],median,na.rm=T)
write.table(medianAngularHd, file = "medianAngularHd.csv", sep=";", row.names = FALSE)
write.table(medianAngularTr, file = "medianAngularTr.csv", sep=";", row.names = FALSE)



########
# means of str-hd differences for each participant in month (or crazy- depending on earlier setting) condition 
meandif = apply(bindeddiffs,1,mean)
meandif = as.data.frame(meandif)
meandif$participant = rownames(meandif)
colnames(meandif)[1]="str_hd" #changing column name
#adding type column and filling it // improve this part
meandif["type"]=NA
meandif$type[1:10]="control"
meandif$type[11:23]="synaesth"  ## this is 11:24 for months but 11:23 for crazy


# computing for str-tr
diffs2 = lapply(meansspl, function(func) func$z[func$condition=="str"]-func$z[func$condition=="tr"])
bindeddiffs2 = as.data.frame(do.call("rbind",diffs2))
names(bindeddiffs2)= c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")


meandif2 = apply(bindeddiffs2,1,mean)
meandif2 = as.data.frame(meandif2)
# adding to maindif dataframe, and reordering columns to make it look nicer 

meandif$str_tr = meandif2$meandif2
meandif = meandif[c("participant","type","str_hd","str_tr")]

#adding a column that tells which subjects did all: months crazy and mini
meandif$did_all = 0
temp_didall = which(meandif$participant %in% c("Sa4","Sa5","Sa11","Sa14")) # for crazy it is: c("Sa1","Sa13","Sa8")
meandif$did_all[temp_didall]=1


#write this - above diffs and mean diffs
bindeddiffs$participant = rownames(bindeddiffs)
bindeddiffs2$participant = rownames(bindeddiffs2)
write.table(bindeddiffs, file = "str-hd-difference-z-crazy.csv", sep=";", row.names = FALSE)
write.table(bindeddiffs2, file = "str-tr-difference-z-crazy.csv", sep=";", row.names = FALSE)
write.table(meandif, file = "mean-differences-z-crazy.csv", sep=";", row.names = FALSE)




# graphing with ggplot2 package -------------------------------------------
library("ggplot2", lib.loc="C:/Users/andrzej/Documents/R/win-library/3.0")

gg = ggplot(meandif, aes(x=participant,y=str_tr, fill=meandif$type))+geom_bar(stat="identity")
#gg = gg+ scale_y_continuous(limits=c(-200, 200))
gg

gg = ggplot(meandif, aes(x=participant,y=str_hd, fill=meandif$type))+geom_bar(stat="identity")
#gg = gg+ scale_y_continuous(limits=c(-200, 200))
gg 

library("reshape2", lib.loc="~/R/win-library/3.0")
meandif_melt = melt(meandif, id=c("participant","type"))
ggplot(meandif_melt, aes(x=participant,y=value, fill=variable))+geom_bar(stat="identity",position="dodge")

ggplot(meandif, aes(x=str_hd,y=str_tr,colour=type))+geom_point(size=5)

gg = ggplot(meandif, aes(x=str_hd,y=str_tr,colour=type))+geom_point(size=5)+coord_equal() #+geom_point(data=meandif[temp_didall,],aes(x=str_hd,y=str_tr),colour="black",size=3)
#gg = gg +scale_y_continuous(limits=c(-200, 200))
#gg = gg +scale_x_continuous(limits=c(-200, 200))
gg = gg + geom_text(aes(label=participant),hjust=0, vjust=0)
gg



### model fit ------
#testing block
originX=0
originz=2100
deg = -45*(pi/180)
currentX = means$X[13]
currentz = means$z[13]
distFromSeat = sqrt((currentX)^2+(currentz-2100)^2)
#modelX =  currentX*cos(deg)+currentz*sin(deg)
#modelz = currentX*sin(deg)-currentz*cos(deg)
modelX = originX + (currentX - originX)*cos(deg) - (currentz - originz)*sin(deg)
modelz = originz + (currentX - originX)*sin(deg) + (currentz - originz) *cos(deg) 
forplotX = c(0,currentX,modelX)
forplotz = c(2100,currentz,modelz)
plot(forplotX,forplotz)
## end of testing block

# 2000-distFromSeat //validation of distance
distModel = sqrt((modelX)^2+(modelz-2100)^2)
distThird = sqrt((modelX-currentX)^2+(modelz-currentz)^2)

# Angle = arccos ( (A^2+B^2-C^2) / 2AB )  //validation of angle
acos((distFromSeat^2+distModel^2-distThird^2)/(2*distFromSeat*distModel))


#adding type column and filling it // improve this part
#fitAllRF["type"]=NA
#fitAllRF$type[1:9]="con"
#fitAllRF$type[10:19]="syn" #10:23 months, but 10:19 for crazy
#adding a coulmn of our visual inspection from quest3d
#fitAllRF["quest"]=c("room","head","head","room","room","trunk","head","head","ambig","head",
#"ambig","trunk","head","room","trunk","trunk","trunk","ambig","head","NA","NA","NA","NA")