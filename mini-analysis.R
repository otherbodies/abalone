wholedout_mini = subset(wholedata, outlier !="1" & condition !="sys" & task == "mini")

#wrongly coded condtition order in mini - correct is: trunk, head-down, str, eyes
# correcting this
for (i in 1:nrow(wholedout_mini)){
  if(wholedout_mini$condition[i]=="str"){
    wholedout_mini$condition[i]="hd"
  }
 else if(wholedout_mini$condition[i]=="hd"){
    wholedout_mini$condition[i]="str"
  }
}



#export to Unity
write.table(wholedout_mini, file = "wholedout-mini-unity.csv", sep=",", row.names = FALSE)

## mini angles
# here -  mini regression lines

load("data.Rda")

library(plyr)
library(reshape2)

meansM = wholedout_mini
#names(means2) = c("m","condition","task","participant","z","x","y")
meansM$X = meansM$X*-1

computeSlopAnglesM = function(subs,plane)
{
  if(plane=="head"){
    slop = lm(subs$z~subs$y)
  }
  else if(plane=="trunk"){
    slop = lm(subs$z~subs$X)
  }
  
  
  angle = format(atan(slop$coef[2])*360/2/pi, digits = 3)
  return(angle)
}


angleSlopeMyz = ddply(meansM,.(participant,condition),computeSlopAnglesM,plane="head")
angleSlopeMxz = ddply(meansM,.(participant,condition),computeSlopAnglesM,plane="trunk")
#angleSlope = subset(angleSlope, task != "mini")

angSlopWideMyz = dcast(angleSlopeMyz, participant ~ condition)
angSlopWideMxz = dcast(angleSlopeMxz, participant ~ condition)

write.table(angSlopWide, file = "angles-slopes.csv", sep=";", row.names = FALSE)


## binding mini and means from months for inter session consistency

interconsist1 = subset(wholedout_mini,condition=="str") 
interconsist2 = subset(means,condition=="str" & task=="month" & rounds=="both")
inter_total = merge(interconsist1,interconsist2,by=c("participant","m","type","rounds","condition"))

inter_total$dist = with(inter_total,sqrt((z.x-z.y)^2+(X.x-X.y)^2+(y.x-y.y)^2))

inter_means = aggregate(inter_total[c("dist")],inter_total[c("type")],mean)

# bootstrap effect size with CI  - with bootES package
library("bootES", lib.loc="~/R/win-library/3.1")

test = inter_total[,c("type","dist")]


bootES(data=test,R=20000,data.col="dist",group.col="type",contrast=c("control","synaesthete"),effect.type="r",plot=T)

