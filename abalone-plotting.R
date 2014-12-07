

library("ggplot2", lib.loc="C:/Users/andrzej/Documents/R/win-library/3.0")

#ggplot plotting------------

#fitAllRF 2d - with quest inspection as shapes
gg = ggplot(fitAllRF,aes(x=fitHead,y=fitTrunk,colour=type,shape=quest))+coord_equal()+geom_point(size=4)
gg = gg +geom_text(aes(label=participant),hjust=0, vjust=1, size=3)
ggsave(gg, file="fitAllRF2d.png", scale=2)


#fitAllRF 2d - with linear model and correlation
gg = ggplot(fitAllRF,aes(x=fitHead,y=fitTrunk))+coord_equal()+geom_point(aes(size=4,colour=type)) # this add as quest inspection #,shape=quest))
#gg = gg +geom_text(aes(label=participant),hjust=0, vjust=1, size=3)
gg = gg + geom_smooth(method="lm",se=FALSE)
gg
ggsave(gg, file="fitAllRF2d-lm.png", scale=2)

cor.test(fitAllRF$fitHead,fitAllRF$fitTrunk)


#fitAllRF 2d - with room fit mapped as colour
gg = ggplot(fitAllRF,aes(x=fitHead,y=fitTrunk,colour=fitRoom))+coord_equal()+geom_point(size=4)
gg = gg +geom_text(aes(label=participant),hjust=0, vjust=1, size=3)
gg = gg + scale_colour_gradient(low="red", high="blue")
gg
ggsave(gg, file="fitAllRF2d-room-colour.png", scale=2)