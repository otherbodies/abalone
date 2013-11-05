#functions

detectOutliers = function(x,y)
{

notoutliers = which(y==0)
without_outliers = x[notoutliers]
return(without_outliers)

}

#

getStatistics = function(readfilename,writefilename,command,sample_n=TRUE)
{

sa4 = read.csv(readfilename,head=TRUE,sep=";")
 
str = c(28:39,93:104,119:130,145:156)
hd = c(54:65,80:91,106:117,158:169)
tr = c(41:52,67:78,132:143,171:182)

straight = sa4[str,]
head = sa4[hd,]
trunk = sa4[tr,]



dz_straight=c()
dx_straight=c()
dy_straight=c()
dz_head=c()
dx_head=c()
dy_head=c()
dz_trunk=c()
dx_trunk=c()
dy_trunk=c()
sample_n_x_straight=c()
sample_n_y_straight=c()
sample_n_z_straight=c()
sample_n_x_head=c()
sample_n_y_head=c()
sample_n_z_head=c()
sample_n_x_trunk=c()
sample_n_y_trunk=c()
sample_n_z_trunk=c()

#debugg = which(straight$m==0)

#computing statistic per condition

for (i in 0:11){
a = which(straight$m==i)

b = straight[a,]

	#outlier removal
	xout = detectOutliers(b$X,b$outlier)
	yout = detectOutliers(b$y,b$outlier)
	zout = detectOutliers(b$z,b$outlier)
	##NB: to be refactored

cx=command(xout)
cy=command(yout)
cz=command(zout)
nx=length(xout)
ny=length(yout)
nz=length(zout)

dz_straight=append(dz_straight,cz)
dx_straight=append(dx_straight,cx)
dy_straight=append(dy_straight,cy)
sample_n_x_straight=append(sample_n_x_straight,nx)
sample_n_y_straight=append(sample_n_y_straight,ny)
sample_n_z_straight=append(sample_n_z_straight,nz)

}

for (i in 0:11){
a = which(head$m==i)
b = head[a,]

	#outlier removal
	xout = detectOutliers(b$X,b$outlier)
	yout = detectOutliers(b$y,b$outlier)
	zout = detectOutliers(b$z,b$outlier)
	##NB: to be refactored



cx=command(xout)
cy=command(yout)
cz=command(zout)
nx=length(xout)
ny=length(yout)
nz=length(zout)

dz_head=append(dz_head,cz)
dx_head=append(dx_head,cx)
dy_head=append(dy_head,cy)
sample_n_x_head=append(sample_n_x_head,nx)
sample_n_y_head=append(sample_n_y_head,ny)
sample_n_z_head=append(sample_n_z_head,nz)

}

for (i in 0:11){
a = which(trunk$m==i)
b = trunk[a,]

#outlier removal
	xout = detectOutliers(b$X,b$outlier)
	yout = detectOutliers(b$y,b$outlier)
	zout = detectOutliers(b$z,b$outlier)
	##NB: to be refactored


cx=command(xout)
cy=command(yout)
cz=command(zout)
nx=length(xout)
ny=length(yout)
nz=length(zout)

dz_trunk=append(dz_trunk,cz)
dx_trunk=append(dx_trunk,cx)
dy_trunk=append(dy_trunk,cy)
sample_n_x_trunk=append(sample_n_x_trunk,nx)
sample_n_y_trunk=append(sample_n_y_trunk,ny)
sample_n_z_trunk=append(sample_n_z_trunk,nz)


}



#data display and write to file

dx_straight
dy_straight
dz_straight
dx_head
dy_head
dz_head
dx_trunk
dy_trunk
dz_trunk
sample_n_x_straight
sample_n_y_straight
sample_n_z_straight
sample_n_x_head
sample_n_y_head
sample_n_z_head
sample_n_x_trunk
sample_n_y_trunk
sample_n_z_trunk

if(sample_n=="TRUE")
{
data = data.frame(dx_straight,dy_straight,dz_straight,dx_head,dy_head,dz_head,
dx_trunk,dy_trunk,dz_trunk,sample_n_x_straight,sample_n_y_straight,sample_n_z_straight,
sample_n_x_head,sample_n_y_head,sample_n_z_head,sample_n_x_trunk,sample_n_y_trunk,
sample_n_z_trunk)}
else{
data = data.frame(dx_straight,dy_straight,dz_straight,dx_head,dy_head,dz_head,
dx_trunk,dy_trunk,dz_trunk)}

write.csv(data,file=writefilename)
return(data)
}


computeAll = function(participant_no){

txt_sd = paste("c://R//datafirstpass//",participant_no,"data_sd.csv",sep="")
txt_mean = paste("c://R//datafirstpass//",participant_no,"data_mean.csv",sep="")
#txt_median = paste("c://R//sa",participant_no,"data_median.csv",sep="")
read_txt = paste("c://R//datafirstpass//",participant_no,"outliers.csv",sep="")


sd_all = getStatistics(read_txt,txt_sd,sd,sample_n=TRUE)
mean_all = getStatistics(read_txt,txt_mean,mean,sample_n=FALSE)
#median_all = getStatistics(read_txt,txt_median,median,sample_n=FALSE)


return("ok")
}

#computeAll(10)

ccc=c(1,2,3,4,5,6,7,8,9,10,11,12)
for(i in 1:12){
computeAll(ccc[i])
}


