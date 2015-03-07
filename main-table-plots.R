## main table scatterplots etc

library("ggplot2", lib.loc="~/R/win-library/3.1")

gg = ggplot(mainTable, aes(y=as.numeric(ratio_month),x=type,colour=bestfit_month))+scale_y_continuous(limits=c(1,4))+geom_point(size=2,na.rm=T)
gg = gg+geom_text(aes(label=participant,size=1),hjust=0, vjust=0,na.rm=T)
gg = gg+ylab("ratio best to next - month")
gg

ggsave(gg, file="ratio_scatterplot_all_month.jpg", scale=2)

gg = ggplot(mainTable, aes(y=as.numeric(ratio_crazy),x=type,colour=bestfit_horse))+scale_y_continuous(limits=c(1,4))+geom_point(size=2,na.rm=T)
gg = gg+geom_text(aes(label=participant,size=1),hjust=0, vjust=0,na.rm=T)
gg = gg+ylab("ratio best to next - crazy")
gg

ggsave(gg, file="ratio_scatterplot_all_crazy.jpg", scale=2)

##
gg = ggplot(mainTable, aes(y=rel_trunk,x=type,colour=bestfit_month))+scale_y_continuous(limits=c(0,3))+geom_point(size=2,na.rm=T)
gg = gg+geom_text(aes(label=participant,size=1),hjust=0, vjust=0,na.rm=T)
gg = gg+ylab("relative trunk = (trunk / mean of head and room")
gg

ggsave(gg, file="relative_trunk31.jpg", scale=2)


gg = ggplot(mainTable, aes(y=rel_head,x=type,colour=bestfit_month))+scale_y_continuous(limits=c(0,3))+geom_point(size=2,na.rm=T)
gg = gg+geom_text(aes(label=participant,size=1),hjust=0, vjust=0,na.rm=T)
gg = gg+ylab("relative trunk = (trunk / mean of head and room")
gg

ggsave(gg, file="relative_head31.jpg", scale=2)


gg = ggplot(mainTable, aes(y=rel_room,x=type,colour=bestfit_month))+scale_y_continuous(limits=c(0,3))+geom_point(size=2,na.rm=T)
gg = gg+geom_text(aes(label=participant,size=1),hjust=0, vjust=0,na.rm=T)
gg = gg+ylab("relative trunk = (trunk / mean of head and room")
gg

ggsave(gg, file="relative_room31.jpg", scale=2)


gg = ggplot(mainTable, aes(y=trunk_fit_reg,x=head_fit_reg,colour=bestfit_month))+geom_point(size=2,na.rm=T)
gg = gg+geom_text(aes(label=participant,size=1),hjust=0, vjust=0,na.rm=T)
gg = gg+ylab("trunk fit from regression lines")
gg

ggsave(gg, file="head_vs_trunk_fit_reg.jpg", scale=2)

