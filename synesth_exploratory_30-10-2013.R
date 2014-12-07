

# functions ---------------------------------------------------------------



drawPlots = function(numberOfPlots){
  par(mfrow=c(numberOfPlots,3))
  plot(dat$X[dat$condition=="hd"],dat$X[dat$condition=="tr"])
  plot(dat$z[dat$condition=="hd"],dat$z[dat$condition=="tr"])
  plot(dat$y[dat$condition=="hd"],dat$y[dat$condition=="tr"])
  par(mfrow=c(1,1))
  
  #means per condition
  meanspercond = aggregate(dat[c("z","X","y")],dat[c("m","condition")],mean)
  plot(meanspercond$X[meanspercond$condition=="hd"],meanspercond$X[meanspercond$condition=="tr"])
  regline=lm(meanspercond$X[meanspercond$condition=="tr"]~meanspercond$X[meanspercond$condition=="hd"])
  abline(regline)
  
  #correlation
  cor.test(meanspercond$X[meanspercond$condition=="hd"],meanspercond$X[meanspercond$condition=="tr"])
  #anova
  anova(lm(meanspercond$X ~ meanspercond$condition))

  
}


# main --------------------------------------------------------------------


Co2_months <- read.csv("C:/OTHERBODIES/bergen-research-data/bergen-new-participants/Co2_months.csv", sep=";")
dat = Co2_months
factor(dat$m)
aggregate(dat[c("z","X","y")],dat["m"],sd)
by (dat,dat["m"],summary)
z.january = dat$z[dat$m==0]


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

jan.head = subset(dat,m=="0" & condition=="hd")

aggregate(dat[c("z","X","y")],dat[c("m","condition")],mean)
means = aggregate(wholedata[c("z","X","y")],wholedata[c("m","condition","task","participant")],mean)
write.table(means,"c://allfiles2//means.csv",sep = ";", row.names=FALSE)

write.table(wholedout,"c://allfiles2//wholedout.csv",sep = ";", row.names=FALSE)
##
subset(means,condition !="sys" & task !="mini")


##


#
meanspercond$m = factor(meanspercond$m,labels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","sys1","sys2","sys3","sys4"))
pairwise.t.test(meanspercond$z, meanspercond$condition, p.adj="bonferroni")

attach(meanspercond)
interaction.plot(condition,m,z, col=rainbow(16))


splitted = split(wholedata, wholedata$participant)
factorlist = list(wholedata$participant,wholedata$task)
splitted = split(wholedata, factorlist)
lapply(splitted, function(x) length(x$outlier[x$outlier==1]))


# example of manipulate ---------------------------------------------------


manipulate(interaction.plot(condition,m,z, col=col, legend=legend),col=slider(0,10,label="colors"), legend = checkbox(TRUE,"Show legend"))

# wholedata2 without outliers and irrelevant trials
wholedout = subset(wholedata2, outlier !="1" & condition !="sys" & task != "mini")
means = aggregate(wholedout[c("z","X","y")],wholedout[c("m","condition","task","participant")],mean)
medians = aggregate(wholedout[c("z","X","y")],wholedout[c("m","condition","task","participant")],median)
sds = aggregate(wholedout[c("z","X","y")],wholedout[c("m","condition","task","participant")],sd)
avofsds = aggregate(sds[c("z","X","y")],sds[c("condition","task","participant")],mean)
sdofsds = aggregate(sds[c("z","X","y")],sds[c("condition","task","participant")],sd)
aa = factor(wholedout$condition)

# advanced outlier removal procedure
wholedoutsplit = split(wholedout,list(factor(wholedout$m),factor(wholedout$condition),factor(wholedout$task),wholedout$participant))
outlierMeasure = lapply(wholedoutsplit, function(x) {(x$X - means$X[means$m==x$m & means$condition==x$condition & means$task==x$task & means$participant==x$participant])/avofsds$X[avofsds$condition==x$condition & avofsds$task==x$task & avofsds$participant==x$participant]})
outlierCount = lapply(outlierMeasure, function(x){x>2})
bindedOutlierCount = as.data.frame(do.call("rbind",outlierCount))             

#temporary removal of co1months because of missed september
means_temp = means[-c(37:71),]

meansspl = split(means_temp,list(means_temp$participant,means_temp$task))
diffs = lapply(meansspl, function(x) x$X[x$condition=="str"]-x$X[x$condition=="hd"])
bindeddiffs = as.data.frame(do.call("rbind",diffs))

names(bindeddiffs)=c("apr","aug","dec","feb","jan","jul","jun","mar","may","nov","oct","sep")
# bindeddiffs$participant=rownames(bindeddiffs)


meandif = apply(bindeddiffs,1,mean)
meandif = as.data.frame(meandif)
meandif$participant = rownames(meandif)
plot(meandif$meandif,1:34)
text(meandif$meandif,1:34,labels=meandif$participant)

#adding str-tr dimension to plot
diffs2 = lapply(meansspl, function(x) x$X[x$condition=="str"]-x$X[x$condition=="tr"])
bindeddiffs2 = as.data.frame(do.call("rbind",diffs2))
meandif2 = apply(bindeddiffs2,1,mean)
meandif2 = as.data.frame(meandif2)
meandif2$participant = rownames(meandif2)
plot(meandif$meandif,meandif2$meandif2,xlab="str-hd",ylab="str-tr")
text(meandif$meandif,meandif2$meandif2,labels=meandif$participant)

#checking sample count in means (if all months included)
aggregate(means[c("z","X","y")],means[c("condition","task","participant")],length)
