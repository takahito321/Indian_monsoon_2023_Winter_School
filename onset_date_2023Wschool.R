library(Hmisc)
tab<-read.table("dTT_2023Wschool.txt") 
dTT<-tab[,1]
year<-tab[,2]
day<-tab[,3]
t1<-which(day==1)
m<-length(t1)    # the number of total years that we deal with          

# identify onset dates (and "withdrawal")
n<-length(dTT)  # total number of days
t<-1:n          # days from 1948 January 1st
cnd<-0          # cnd=1 for monsoon and 0 for non-monsoon (cnd=0 on Jan 1st)
tm<-numeric(0)  # onset and withdrawal days from 1948 January 1st
for(i in 1:n){
  if(cnd==0){
    if(dTT[i]>0 && day[i]<200){
      tm<-c(tm,i)
      cnd<-1
    }
  } else{
    if(dTT[i]<=0 && day[i]>=200){
      tm<-c(tm,i)
      cnd<-0
    }
  }
}
ton<-tm[seq(1,length(tm),by=2)]   # ton only onset days
toff<-tm[seq(2,length(tm),by=2)]  # toff only withdrawal days
day_on<-day[ton]                  # onset dates in year
day_off<-day[toff]                # onset dates in year


dat<- data.frame(1948:2022, day_on, day_off, ton, toff)
write.table(dat, file = "onset_date_2023Wschool.txt", col.names=F, row.names=F)

# prediction ##################
x<-read.table("69_2022_1.dat")
x<-rbind(x, read.table("69_2022_2.dat"))
x<-rbind(x, read.table("69_2022_3.dat"))
x<-rbind(x, read.table("69_2022_4.dat"))
x<-rbind(x, read.table("69_2022_5.dat"))
# data
x<-as.matrix(x)
xx<-x[,-(m+1)]
y<-matrix(day_on,nrow(xx),m,byrow=TRUE)
RMSE<-x[,(m+1)]  # RMSE of proximity

# top ensemble average
x2<-xx[RMSE<=sort(RMSE)[100],]
POD2<-colMeans(x2)

at<-1981:max(year)
yu<-length(at)
yp<-(max(year)-yu+1):max(year)
POD2_used<-POD2[(length(POD2)-yu+1):length(POD2)]

postscript(file="onset_date_2023Wschool.eps", horizontal=TRUE, encoding="WinAnsi.enc")
mat <- matrix(c(1,2), 2, 1, byrow = TRUE) # 30 panels
layout(mat)
par(mar=c(0,0,0,0))                     # graphic parameters
par(oma=c(0,0,0,0))
par(mai = c(0.7, 1, 0.1, 0.2))
cx<-1.5

at<-c(1,50,100,150,200,250,300,350)
lab<-at

plot(day[year==2019],dTT[year==2019],xlim=c(0,365),ylim=c(-6,4.5),type="l",ylab=expression(paste(Delta,"TT (",degree,"C)")),xlab="",bty = "n",xaxt="n",xaxs = "i",cex.lab=cx,cex.axis=cx,lwd=2)
abline(v=t1[yp-1]+151,col="gray") # JUN 1 in 2018, JAN1+(30+28+31+30+31+1)=JAN1+151=152
for(i in 1948:2019) lines(day[year==i],dTT[year==i],col="gray")
abline(h=0)
lines(day[year==2019],dTT[year==2019],col=1,lwd=2)
#lines(day[year==2021],dTT[year==2021],col=5,lwd=3)
lines(day[year==2022],dTT[year==2022],col=2,lwd=4)
abline(v=day_on[72],col=1,lty=2,lwd=2)
abline(v=day_off[72],col=1,lty=2,lwd=2)
abline(v=day_on[75],col=2,lty=2,lwd=2)
abline(v=day_off[75],col=2,lty=2,lwd=2)
#abline(v=152,col=1,lty=2,lwd=2)
axis(side=1, at=at,labels=lab,cex.axis=cx)
text(174,-1.8,"onset",col=1,cex=1.5)
text(265,-1.8,"end",col=1,cex=1.5)
text(217,4.4,"summer monsoon",col=1,cex=1.4)
text(70,-6,"March 11th",col=4,cex=1.4)
arrows(70,-5.6,70,-4.5,length=0.2,angle=30,code=2,col=4,lty=1,lwd=2)
legend(35,4,c(expression(paste(Delta," TT in 2019"))),col=c(1),lwd=3,cex=1.4,lty=c(1),bty="n")
#legend(35,4,c(expression(paste(Delta," TT in 2021"))),col=c(5),lwd=3,cex=1.4,lty=c(1),bty="n")
legend(35,5.2,c(expression(paste(Delta," TT in 2022"))),col=c(2),lwd=3,cex=1.4,lty=c(1),bty="n")
legend(35,2.8,c(expression(paste("Other years"))),col=c("gray"),lwd=3,cex=1.4,lty=c(1),bty="n")
mtext("Day of year", side=1, line=2.2, cex=cx)
mtext("(a)", side = 3, adj = 0.01, line = -1.0, cex=cx)
minor.tick(nx=5, ny=2, tick.ratio=0.4)  

years<-1948:2022 # already observed years
plot(years,day_on,type="b",col=1,lwd=2,cex=cx,xlim=c(1947,2027),ylim=c(135,170.5),xlab="",ylab="Onset date",bty = "n",cex.lab=cx,cex.axis=cx,xaxs = "i",pch=5)
polygon(c(years,rev(years)),c(day_on[-m]-7,rev(day_on[-m])+7),border = NA, col="gray90")
lines(years,day_on,type="b",col=1,lwd=2,cex=cx,pch=5)
#lines(years.MOK,MOK,type="b",xlim=c(1948,2020),ylim=c(130,175),lwd=1.5,lty=2,col=3,xlab="",ylab="Onset date",bty = "n",cex=1,cex.lab=cx,cex.axis=cx,pch=4) # MOK
lines(yp,POD2_used,type="b",col=6,lwd=2,cex=cx) # predicton
legend(1960,140,c(expression(paste(Delta,"TT-based (actual)"))),col=c(1),pch=c(5),lwd=1.5,cex=1.3,lty=c(1),bty="n")
legend(2000,140,c("Prediction"),col=c(6),pch=c(1),lwd=1.5,cex=1.3,lty=c(2),bty="n")
#legend(1957,139,c("Monsoon Onset over Kerala"),col=c(3),pch=c(4),lwd=1.5,cex=1.3,lty=c(2),bty="n")
mtext("(b)", side = 3, adj = 0.01, line = -1.0, cex=cx)
mtext("Year",side=1,line=2.4,cex=1.5)
minor.tick(nx=10, ny=5, tick.ratio=0.4)
text(2022,167,"2022",cex=1.5,lwd=2,col=2)
arrows(2022,165,2022,152,length=0.2,angle=30,code=2,col=2,lty=1,lwd=2)
#text(2025,MOK[m],"5/29",cex=1.5,lwd=2,col=3)
text(2025,POD2[m],"5/26",cex=1.5,lwd=2,col=6)
text(2025,day_on[m]-2,"5/25",cex=1.5,lwd=2,col=1)
grid()
dev.off()


# 31+28+11 = 70 = March 11
# 31+28+31+30+31+1 = 152 = June 1st
# 151 = 31 5
# 150 = 30 5
# 149 = 29 5
# 148 = 28 5
# 147 = 27 5
# 146 = 26 5
# 145 = 25 5
# 144 = 24 5

quantile(x2[,75])
#  0%  25%  50%  75% 100% 
# 139  144  146  148  154 

#plot(dTT[year==2022],type="b",xlim=c(140,148),ylim=c(-0.6,0.6))
#abline(h=0)
# 145 dTT-based actual onset date

T1<-dTT[year==2022][144]
T2<-dTT[year==2022][145]
# (-T1)/(T2-T1) # 0.16
