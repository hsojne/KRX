library(quantreg)
library(lattice)

#######################################################################
# Data

getwd()
setwd('/Users/82108/Desktop/KRW/')

#######################################################################
data <- read.csv("PCA_test.csv",header=TRUE)
head(data)
No <- c(dim(data)[2]-2)
Noq <- 10  # number of quantiles
retQMVHR = matrix(0, No,(Noq+1))
plotting<-matrix(0,Noq*No,3)
nnames<-matrix("tt",Noq*No,1)

#######################################################################
# Functions
#######################################################################
# Quantile minimum variance hedge ratio, for freq and quantiles in taus
# The last element of numeric array is the MV hedge ratio
QMVHR<-function(XX,taus,freq){
	nn<-nrow(XX)
	x <- XX[(0:floor(nn/freq))*freq,]   # freq = daily(1), weekly(5), 2-week(10), 4-week 
  	nn <- nrow(x)
	  first.ret <- x[,1]#log(x[2:nn,1]/x[1:(nn-1),1])
  	second.ret <- x[,2] #log(x[2:nn,2]/x[1:(nn-1),2])
  	est1 <- rq(first.ret~second.ret,tau=taus)    
  	estmv <- lm(first.ret~second.ret)					 
	vec <- c(est1$coef[2,],estmv$coef[2])
	names(vec)[length(vec)]<-"MV Hedge Ratio"
	return(vec)
}

# Plot
plot_three<-function(d_ata,No,fnames){
plotting[,1]<-rep(taus,No)
plotting[,2]<- c(d_ata[1,1:Noq],d_ata[2,1:Noq],d_ata[3,1:Noq])
plotting[1:Noq,3]<-d_ata[1,Noq+1]
plotting[(Noq+1):(2*Noq),3]<-d_ata[2,Noq+1]
plotting[((2*Noq)+1):(3*Noq),3]<-d_ata[3,Noq+1]



nnames[1:Noq,]<-fnames[1]
nnames[(Noq+1):(2*Noq),]<-fnames[2]
nnames[((2*Noq)+1):(3*Noq),]<-fnames[3]



pl<-data.frame(plotting,nnames)
names(pl)<-c("Quantile","QHR","MVHR","Variable")
xyplot(QHR+MVHR~Quantile | Variable,data=pl,ylim=c(min(d_ata[,1:5]),max(d_ata[,1:5])),type=c("l"),col=c("black","black"),ylab = "",lwd=c(1,3),lty=c(1,3),layout=c(1,3),as.table=TRUE)
}



#######################################################################
# Estimation
#######################################################################
head(cbind(data[,c(2+1)],data[,2]))
# you give the file name below to store the results 
file="daily.csv"

for (j in 1:No)
{
    xx <- data  # reads the file with header "TRUE"
    kesxx <- cbind(xx[,c(2+j)],xx[,2])    
    taus <- seq(0.01,0.99,length.out=Noq)
    Freq <- 1
    retQMVHR[j,] <- QMVHR(kesxx,taus,Freq)
}    

fnames=names(data)[-c(1:2)]
cat("Variable, ",file=file,append=FALSE,sep=",")
cat(taus,file=file,append=T,sep=",")
cat(format(", MVHR \n"),file=file,append=T,sep=",")
for (i in 1:No) {
      cat(format(fnames[i]),file=file,append=T)
      cat(", ",file=file,append=T,sep=",")
      cat(retQMVHR[i,],file=file,append=T,sep=",")
      cat(format("\n"),file=file,append=T,sep=",")
}   

d_ata <- retQMVHR
pdf(file="DailyfivepointMV.pdf",width=6,height = 4)
plot_three(retQMVHR,No,fnames)
dev.off()




