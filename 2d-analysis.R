sds2d = aggregate(data2d[c("mouse_x","mouse_y")],data2d[c("month_no","user_id")],sd)
lengths2d = aggregate(data2d[c("mouse_x","mouse_y")],data2d[c("month_no","user_id")],length)
means_sds2d = aggregate(sds2d[c("mouse_x","mouse_y")],sds2d[c("user_id")],mean)



write.table(means_sds2d, file = "sds2d.csv", sep=";", row.names = FALSE)

library(xlsx)
write.xlsx(means_sds2d, "sds2d.xlsx")

#means_sds2d manually corrected

didall2d = subset(means_sds2d,didall=="yes")
t.test(didall2d$mouse_x ~ didall2d$type)
t.test(didall2d$mouse_y ~ didall2d$type)


write.xlsx(mainTable, "maintable.xlsx")


#visualising 2d data
library("ggplot2", lib.loc="~/R/win-library/3.1")

data2d$month_no=as.factor(data2d$month_no)
data2d$month_no = factor(data2d$month_no,labels=c("dec","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov"))
data2d$month_no = factor(data2d$month_no,levels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
save(data2d,file="dat2d.Rda")

temp2d = subset(data2d,user_id=="Sa7_2DD")

gg = ggplot(temp2d,aes(x=mouse_x,y=mouse_y,colour=month_no,shape=month_no))+coord_equal()+geom_point(size=4)+ylim(1050,0)+xlim(0,1250)
gg = gg+ scale_shape_manual(values=c(15,16,17,18,19,8,15,16,17,18,19,8))
gg = gg+ scale_colour_brewer(palette="Paired")
gg


##
