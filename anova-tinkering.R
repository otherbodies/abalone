datafilename="path/to/file.csv" # path to CSV data file
mydata=read.csv(datafilename,header=TRUE,colClasses=c("numeric","factor","factor","factor")) # read CSV data file, specify data type
mydata.aov = aov(DV~IV1*IV2*IV3,data=mydata)  # do the analysis of variance
mydata.anova <- anova.lm(mydata.aov) # calculate type I ANOVA table
mydata.ss <- mydata.anova$"Sum Sq" # sums of squares
mydata.pes <- mydata.ss/(mydata.ss+mydata.ss[length(mydata.ss)]) # partial eta squared
mydata.pes[length(mydata.pes)] <- "" # clear effect size for residual
mydata.anova$"Partial Eta Sq" <- mydata.pes # add effect size data to ANOVA table
mydata.anova # display Type I ANOVA table with effect size column
r <- summary.lm(mydata.aov) # summary of linear model
r$"r.squared" # display R squared
r$"adj.r.squared" # display Adjusted R squared

#########
options(scipen=999)#switching off scientific notation

sdsanova = aov(overall ~ type*task*condition +Error(participant/(task*condition)), pooledsds4)
summary(sdsanova)


TukeyHSD(sdsanova)

#########
sdsanova2 = ezANOVA(data=pooledsds4,dv=overall,wid=participant,within=.(task,condition),between=type)
sdsanova2


#########

stest = function(x,y){
  sub = subset(pooledsds4,task==x & condition==y)
  with(sub,t.test(overall~type))

}

stest("month","hd")


###### simulating uniform distrubution of model fits - to check how manova and clustering behave
library("rgl", lib.loc="~/R/win-library/3.1")

xunif = runif(24,0,120)
yunif = runif(24,0,120)
zunif = runif(24,0,120)
uniform = data.frame(xunif,yunif,zunif)
uniform[1,]

with(uniform,plot3d(xunif,yunif,zunif,type="s",radius=5))
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), 
                                      width=700,height=700), sep=""))


clu_unif = hclust(dist(uniform[c("xunif","yunif","zunif")]),method="ward.D")
plot(clu_unif,main="hierarchical cluster ward")
groups = cutree(clu_unif,3)
rect.hclust(clu_unif, k=3, border="red")

kmeans_unif = kmeans(uniform[c("xunif","yunif","zunif")],3)
uniform$kmeans = kmeans_unif$cluster

fitmanova_unif = manova(cbind(uniform$xunif,uniform$yunif,uniform$zunif)~factor(uniform$kmeans))
summary.manova(fitmanova_unif, test="Wilks")

summary.aov(fitmanova_unif)

summary(manova(cbind(zunif,yunif,zunif) ~ kmeans, data=uniform, subset = kmeans %in% c("1","2") ))
summary(manova(cbind(zunif,yunif,zunif) ~ kmeans, data=uniform, subset = kmeans %in% c("1","3") ))
summary(manova(cbind(zunif,yunif,zunif) ~ kmeans, data=uniform, subset = kmeans %in% c("3","2") ))


## testing meaningfulness of clusters with bootstrap simulation
sim.xy <- function(n, mean, sd) cbind(rnorm(n, mean[1], sd[1]),
                                      rnorm(n, mean[2],sd[2]))
xy <- rbind(sim.xy(100, c(0,0), c(.2,.2)),
            sim.xy(100, c(2.5,0), c(.4,.2)),
            sim.xy(100, c(1.25,.5), c(.3,.2)))
library(fpc)
km.boot <- clusterboot(xy, B=20, bootmethod="boot",
                       clustermethod=kmeansCBI,
                       krange=3, seed=15555)

##the same but with uniform data
xunif = runif(240,0,120)
yunif = runif(240,0,120)
zunif = runif(240,0,120)
uniform = data.frame(xunif,yunif,zunif)
uniform[1,]

km.boot.unif <- clusterboot(uniform, B=100, bootmethod="boot",
                       clustermethod=kmeansCBI,
                       krange=3)
km.boot.unif

#the same with our real data
km.boot.real <- clusterboot(fitClu[c("trunk_fit_fur","head_fit_fur","room_fit_fur")], B=100, bootmethod="boot",
                            clustermethod=kmeansCBI,
                            krange=3)
km.boot.real