
# computing the difference between best fit and next best fit
fitAllRF2 = fitAllRF
fitAllRF2["diff_from_bestfit"]=NA


for(i in 1:nrow(fitAllRF2)){
  vect = c(fitAllRF2[i,3],fitAllRF2[i,4],fitAllRF2[i,5])
  sorted_vect = sort(vect)
  bestfit_dif = sorted_vect[2]-sorted_vect[1]
  fitAllRF2$diff_from_bestfit[i]=bestfit_dif 
  
  #ratios
  fitAllRF2["ratio"]=NA
  bf_ratio = sorted_vect[2]/sorted_vect[1]
  fitAllRF2$ratio[i]=bf_ratio
  
}
ggplot(fitAllRF2, aes(x=participant,y=diff_from_bestfit, fill=largest))+geom_bar(stat="identity",position="dodge")


#sorting dataframe to nicely plot groups: which fit was the largest
sortedfit = with(fitAllRF2, fitAllRF2[order(largest),])
s2 = as.character(sortedfit$participant)
sortedfit$participant = factor(sortedfit$participant,levels=s2)


# plotting of difference between best fit and next best - in absolute values
gg=ggplot(sortedfit, aes(x=participant,y=diff_from_bestfit, fill=largest))+geom_bar(stat="identity",position="dodge")
gg = gg+ scale_fill_discrete(labels=c("trunk","room","head"))
gg
ggsave(gg, file="fitAllRF-dif_from-best.png", scale=2)

# plotting of difference between best fit and next best - in ratios
gg=ggplot(sortedfit, aes(x=participant,y=ratio, fill=largest))+geom_bar(stat="identity",position="dodge")
gg = gg+ scale_fill_discrete(labels=c("trunk","room","head"))
#ggsave(gg, file="fitAllRF-dif_from-best-ratio.png", scale=2)
gg



##difference between head and trunk fit
fitAllRF3 = fitAllRF2
library("plyr", lib.loc="C:/Users/andrzej/Documents/R/win-library/3.0")
fitAllRF3["hd_tr_diff"]=NA
bb = ddply(fitAllRF3, "participant", function(x){
  aa = x$fitHead-x$fitTrunk
  data.frame(hd_tr_diff = aa)
})
fitAllRF3["hd_tr_diff"] = bb$hd_tr_diff

#sorting dataframe 
sortedfit2 = with(fitAllRF3, fitAllRF3[order(hd_tr_diff),])
s3 = as.character(sortedfit2$participant)
sortedfit2$participant = factor(sortedfit2$participant,levels=s3)
#plotting this
ggplot(sortedfit2,aes(x=participant,y=hd_tr_diff,fill=quest))+geom_bar(stat="identity")

ggplot(sortedfit2,aes(x=hd_tr_diff,fill=quest))+geom_bar(stat="bin",binwidth = 90)







##computing form size - largest 3d distance
# split means into separate lists by participant/condition

# for each list - compute distance from each point to all others 
# write it to a vector, and take max value from the vector
# write max value for each participant to fitAll2 dataframe



#consistency testing
# pooled sd
pooledtest = subset(wholedout, participant=="Co1" & condition=="str")


##--- just some examples and working space
gg=ggplot(sortedfit, aes(x=participant,y=diff_from_bestfit, fill=largest))+geom_bar(stat="identity",position="dodge")
gg = gg+ scale_fill_manual(values=c("red","blue","green"),labels=c("trunk","room","head"))
gg

fit = lm(diff_from_bestfit~largest,data=sortedfit)
anova(fit)

