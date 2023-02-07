# Change the finall year
final<-2022

# downloading data #############################################################################################
if(FALSE){
  yy<-1948:(final-1)
  for(i in 1:length(yy)){
      curl<-paste("ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/air.",yy[i],".nc",sep="")
      cdestfile<-paste("NCEP_Temperature/air.",yy[i],".nc",sep="")
      download.file(curl,cdestfile,mode="wb") 
  }
}

curl<-paste("ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/air.",final,".nc",sep="")
cdestfile<-paste("NCEP_Temperature/air.",final,".nc",sep="")
download.file(curl,cdestfile,mode="wb") 

# quick views #############################################################################################
library(RColorBrewer)
library(fields)
library(ncdf4)
ncin <- nc_open("NCEP_Temperature/air.2019.nc")
#print(ncin)
lon<-ncvar_get(ncin,"lon")
lat<-ncvar_get(ncin,"lat")
level<-ncvar_get(ncin,"level")
ix<-which(lon>=40 & lon<=100)
iy<-which(lat>=5 & lat<=35)
t<-ncvar_get(ncin,"time")
u<-ncvar_get(ncin,"air", start=c(min(ix),min(iy),5,1), count=c(length(ix),length(iy),5,length(t))) # u[lon,lat,level,time]
x<-apply(u,4,mean)
nc_close(ncin)

# Pradhan et al. 2017 #############################################################################################################

yy<-1948:(final-1)
xn<-numeric(0) # north box
xs<-numeric(0) # south box
day<-numeric(0)
year<-numeric(0)
#ix<-which(lon>=40 & lon<=100) # Xavier et al. 2007  
ix<-which(lon>=30 & lon<=110) # Xavier et al. 2007 # Pradhan 2017 -> ix<-which(lon>=30 & lon<=110) 
iy<-which(lat>=(-15) & lat<=35) # length(iy)=21, lat[iy[1:13]]=35-5, lat[iy[13:21]]
#w<-matrix(cos(2*pi*lat[iy]/360),length(ix),length(iy),byrow=T)
#w<-array(w,dim=c(length(ix),length(iy),366)) 
#w<-w/mean(w)
for(i in 1:length(yy)){
  cdestfile<-paste("NCEP_Temperature/air.",yy[i],".nc",sep="")
  ncin <- nc_open(cdestfile)
  t<-ncvar_get(ncin,"time")
  u<-ncvar_get(ncin,"air", start=c(min(ix),min(iy),5,1), count=c(length(ix),length(iy),6,length(t))) # u[lon,lat,level,time]
  xn<-c(xn,apply(u[, 1:13,-5,],4,mean)) # -5 to remove 250-hpa level, lat[iy][1:13]=35-5N
  xs<-c(xs,apply(u[,13:21,-5,],4,mean)) # lat[iy][13:21]=5N-15S
  day<-c(day,1:length(t))
  year<-c(year,rep(yy[i],length(t)))
  nc_close(ncin)
}

#
cdestfile<-paste("NCEP_Temperature/air.",final,".nc",sep="")
if(final%%4==0){
    dayl<-366
} else{
    dayl<-365
}
ncin <- nc_open(cdestfile)
t<-ncvar_get(ncin,"time")
u<-ncvar_get(ncin,"air", start=c(min(ix),min(iy),5,1), count=c(length(ix),length(iy),6,length(t))) # u[lon,lat,level,time]
xn<-c(xn,apply(u[, 1:13,-5,],4,mean)) # -5 to remove 250-hpa level, lat[iy][1:13]=35-5N
xs<-c(xs,apply(u[,13:21,-5,],4,mean)) # lat[iy][13:21]=5N-15S
day<-c(day,1:dayl)
year<-c(year,rep(final,dayl))
nt<-length(t)
#xn<-c(xn,xn[(nt+1):dayl])
#xs<-c(xs,xs[(nt+1):dayl])
nc_close(ncin)


dat<- data.frame(xn-xs, year, day, xn, xs)
#write.table(dat, file = "dTT_latest_xavier.txt", col.names=F, row.names=F)
write.table(dat, file = "dTT_2023Wschool.txt", col.names=F, row.names=F)
