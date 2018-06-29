library("plot3D", lib.loc="~/R/win-library/3.2")

x=mainTable_syn_mon$trunk_fit_mixed
y=mainTable_syn_mon$head_fit_mixed
z=mainTable_syn_mon$room_fit_mixed


scatter3D(x=80,y=10,z=10, bty = "g",pch = 20, cex = 2, colkey = FALSE, main ="synaesthete - AMF - months")

scatter3D(x=10,y=80,z=10, bty = "g",pch = 20, cex = 2, colkey = FALSE, main ="synaesthete - AMF - months",add = TRUE)

scatter3D(x=10,y=10,z=80, bty = "g",pch = 20, cex = 2, colkey = FALSE, main ="synaesthete - AMF - months",add = TRUE,theta = 20, phi = 20)




d <- data.frame(x=c(200,400,600), y=c(1200,2000,1500), z=c(1800,2700, 3000))
plot3d(d[,1:3], type="n")
bbox3d(color=c("gray50","black"), emission="gray30", specular="gray30", shininess=2, alpha=0.8)
grid3d(c("x+", "y+", "z+"), lty=1)


## tests with updated 3d plotting to web
library(rgl)
plotids <- with(iris, plot3d(Sepal.Length, Sepal.Width, Petal.Length, 
                             type="s", col=as.numeric(Species)))
rglwidget(elementId = "plot3drgl")



library(rgl)
#rgl.open()
#rgl.bg(color="white")
plot3d(mainTable_didall_syn$trunk_fit_mixed,mainTable_didall_syn$head_fit_mixed,mainTable_didall_syn$room_fit_mixed,type="s",size=2,col=as.integer(mainTable_didall_syn$bestfit_mixed)+1,
       xlab="trunk",ylab="head",zlab="room" 
       )
shapelist3d(tetrahedron3d(),mainTable_didall_con$trunk_fit_mixed,mainTable_didall_con$head_fit_mixed,mainTable_didall_con$room_fit_mixed,size=2,color=as.integer(mainTable_didall_con$bestfit_mixed)+1)

#rgl.points(x=c(0,0,0),color="black")
#axes3d()
title3d(main="months - spheres=syn, triangles=con, colour=bestfit") #,xlab="trunk",ylab="head",zlab="room")
#mtext3d("trunk",edge="x",pos=c(0,0,-50))
#mtext3d("head",edge="y",pos=c(-40,0,0))
#mtext3d("room",edge="z",pos=c(-10,-40,0))
#browseURL(paste("file://", writeWebGL(dir=file.path("c://BERGEN//rgl", "webGL"), 
#aa = scene3d()                                  #   width=800,height=800), sep=""))

rglwidget(width=800,height=800)
