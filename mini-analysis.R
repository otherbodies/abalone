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

interconsist1 = subset(wholedout_mini,condition=="tr") 
interconsist2 = subset(means,condition=="tr" & task=="month" & rounds=="both")
inter_total = merge(interconsist1,interconsist2,by=c("participant","m","type","rounds","condition"))

inter_total$dist = with(inter_total,sqrt((z.x-z.y)^2+(X.x-X.y)^2+(y.x-y.y)^2))

inter_means = aggregate(inter_total[c("dist")],inter_total[c("participant","type")],mean)



inter_lengths = aggregate(inter_total[c("dist")],inter_total[c("participant")],length)
# bootstrap effect size with CI  - with bootES package
library("bootES", lib.loc="~/R/win-library/3.2")

test = inter_total[,c("type","dist")]


boo = bootES(data=test,R=2000,data.col="dist",group.col="type",contrast=c("control","synaesthete"),effect.type="r",plot=F)

bootES(data=inter_means,R=20000,data.col="dist",group.col="type",contrast=c("synaesthete","control"),effect.type="cohens.d",plot=F)



write.table(inter_total, file = "inter.csv", sep=";", row.names = FALSE)
write.table(inter_means, file = "inter_str_means.csv", sep=";", row.names = FALSE)



## MONTHS ANALYSIS - PART OF MAIN ANALYSIS
## Testing hypothesis that preferred FoR is different for syn and for controls
## group comparisons 10/10 

mt = subset(mainTable,rounds=="both")
b1 = bootES(data=mt,R=20000,data.col="head_fit_mixed",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b2 = bootES(data=mt,R=20000,data.col="trunk_fit_mixed",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b3 = bootES(data=mt,R=20000,data.col="room_fit_mixed",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)

b4 = bootES(data=mt,R=20000,data.col="head_fit_month",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b5 = bootES(data=mt,R=20000,data.col="trunk_fit_month",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b6 = bootES(data=mt,R=20000,data.col="room_fit_month",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)

b7 = bootES(data=mt,R=20000,data.col="head_fit_fur",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b8 = bootES(data=mt,R=20000,data.col="trunk_fit_fur",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b9 = bootES(data=mt,R=20000,data.col="room_fit_fur",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)

b10 = bootES(data=mt,R=20000,data.col="head_fit_reg",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b11 = bootES(data=mt,R=20000,data.col="trunk_fit_reg",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b12 = bootES(data=mt,R=20000,data.col="room_fit_reg",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)

b_list=list(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12)

es_table = data.frame(es=numeric(12),ci_low=numeric(12),ci_high=numeric(12))
measure = c("head_fit_mixed","trunk_fit_mixed","room_fit_mixed","head_fit_month","trunk_fit_month","room_fit_month","head_fit_fur","trunk_fit_fur","room_fit_fur",
         "head_fit_reg","trunk_fit_reg","room_fit_reg")
es_table$measure = measure

for (i in 1:12){
  es = b_list[[i]]$t0
  ci_low = b_list[[i]]$bounds[1]
  ci_high = b_list[[i]]$bounds[2]
  es_table[i,]$es = es
  es_table[i,]$ci_low = ci_low
  es_table[i,]$ci_high = ci_high
}

##plotting es with ci 
gg = ggplot(es_table,aes(y=measure,x=es))+geom_point()
gg = gg+geom_errorbarh(aes(xmin=ci_low, xmax=ci_high),height=0.5)
gg

#t-tests for group comparisons 10/10
t.test(mt$head_fit_mixed~mt$type)
t.test(mt$trunk_fit_mixed~mt$type)
t.test(mt$room_fit_mixed~mt$type)

t.test(mt$head_fit_month~mt$type)
t.test(mt$trunk_fit_month~mt$type)
t.test(mt$room_fit_month~mt$type)

t.test(mt$head_fit_fur~mt$type)
t.test(mt$trunk_fit_fur~mt$type)
t.test(mt$room_fit_fur~mt$type)

t.test(mt$head_fit_reg~mt$type)
t.test(mt$trunk_fit_reg~mt$type)
t.test(mt$room_fit_reg~mt$type)


##consistency CI ES
conTableM$type = NA
conTableM[1:10,]$type = "control"
conTableM[11:20,]$type = "synaesthete"

bootES(data=conTableM,R=20000,data.col="overall_hd_m",group.col="type",contrast=c("synaesthete","control"),effect.type="cohens.d",plot=F)
bootES(data=conTableM,R=20000,data.col="overall_hd_cr",group.col="type",contrast=c("synaesthete","control"),effect.type="cohens.d",plot=F)
bootES(data=conTableM,R=20000,data.col="overall_str_m",group.col="type",contrast=c("synaesthete","control"),effect.type="cohens.d",plot=F)
bootES(data=conTableM,R=20000,data.col="overall_str_cr",group.col="type",contrast=c("synaesthete","control"),effect.type="cohens.d",plot=F)
bootES(data=conTableM,R=20000,data.col="overall_tr_m",group.col="type",contrast=c("synaesthete","control"),effect.type="cohens.d",plot=F)
bootES(data=conTableM,R=20000,data.col="overall_tr_cr",group.col="type",contrast=c("synaesthete","control"),effect.type="cohens.d",plot=F)


## CRAZY ANALYSIS - PART OF MAIN ANALYSIS
## Testing hypothesis that preferred FoR is different for syn and for controls
## group comparisons 10/10 

mt = subset(mainTable,rounds=="both")

b1_cr = bootES(data=mt,R=20000,data.col="head_fit_horse",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b2_cr = bootES(data=mt,R=20000,data.col="trunk_fit_horse",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b3_cr = bootES(data=mt,R=20000,data.col="room_fit_horse",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)

b4_cr = bootES(data=mt,R=20000,data.col="head_fit_fur_cr",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b5_cr = bootES(data=mt,R=20000,data.col="trunk_fit_fur_cr",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b6_cr = bootES(data=mt,R=20000,data.col="room_fit_fur_cr",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)

b7_cr = bootES(data=mt,R=20000,data.col="head_fit_reg_cr",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b8_cr = bootES(data=mt,R=20000,data.col="trunk_fit_reg_cr",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)
b9_cr = bootES(data=mt,R=20000,data.col="room_fit_reg_cr",group.col="type",contrast=c("synaesthete","control"),effect.type="r",plot=F)

b_list_cr=list(b1_cr,b2_cr,b3_cr,b4_cr,b5_cr,b6_cr,b7_cr,b8_cr,b9_cr)

es_table_cr = data.frame(es=numeric(9),ci_low=numeric(9),ci_high=numeric(9))
measure_cr = c("head_fit_horse","trunk_fit_horse","room_fit_horse","head_fit_fur_cr","trunk_fit_fur_cr","room_fit_fur_cr",
            "head_fit_reg_cr","trunk_fit_reg_cr","room_fit_reg_cr")
es_table_cr$measure = measure_cr

for (i in 1:9){
  es = b_list_cr[[i]]$t0
  ci_low = b_list_cr[[i]]$bounds[1]
  ci_high = b_list_cr[[i]]$bounds[2]
  es_table_cr[i,]$es = es
  es_table_cr[i,]$ci_low = ci_low
  es_table_cr[i,]$ci_high = ci_high
}

##plotting es with ci 
gg = ggplot(es_table_cr,aes(y=measure,x=es))+geom_point()
gg = gg+geom_errorbarh(aes(xmin=ci_low, xmax=ci_high),height=0.5)
gg

#t-tests for group comparisons 10/10

t.test(mt$head_fit_horse~mt$type)
t.test(mt$trunk_fit_horse~mt$type)
t.test(mt$room_fit_horse~mt$type)

t.test(mt$head_fit_fur_cr~mt$type)
t.test(mt$trunk_fit_fur_cr~mt$type)
t.test(mt$room_fit_fur_cr~mt$type)

t.test(mt$head_fit_reg_cr~mt$type)
t.test(mt$trunk_fit_reg_cr~mt$type)
t.test(mt$room_fit_reg_cr~mt$type)
