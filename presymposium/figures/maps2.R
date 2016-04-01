setwd("/Users/niemi/Dropbox/measles/code")
# read in
nc.long <- read.table("newcases.txt", header=T)
nc <- reshape(nc.long, v.names="new.cases", timevar="date", idvar="district", direction="wide", times="date")
nc[is.na(nc)] <- 0
ordr <- order(nc$district)
nc <- nc[ordr,]
row.names(nc) <- 1:nrow(nc)

dates <- as.Date(substr(names(nc)[-1], 11,20))
y <- t(apply(nc[,-1], 1, cumsum))


######### spatial spread ###################

# Evolving maps
library(maptools)
getinfo.shape("../data/map/gadm/ZWE_adm2")
zz <- readShapePoly("../data/map/gadm/ZWE_adm2")

# There are 61 districts in the csv file and only 60 in the map
ordr <- match(tolower(zz$NAME_2),nc$district)
ordr[14] <- which(nc$district=="mt. darwin")
ordr[24] <- which(nc$district=="uzumba-maramba-pfungwe")
ordr[25] <- which(nc$district=="hwedza")
ordr[47] <- which(nc$district=="mangwe")
ordr[50] <- which(nc$district=="mangwe") # Yes these are both Mangwe
ordr[53] <- which(nc$district=="chirumanzu")
# Chitungwiza is never used, it is a suburb of Harare

clr.f1 <- function(ar, ordr=1:length(ar), mn=min(ar), mx=max(ar)) { 
  return(1-(ar[ordr]-mn)/(mx-mn))
}



# Final map
dat.pop <- read.table("../data/measles/20101212.txt", header=T)[-64,c(1,8,10)]
dat.pop$pop <- dat.pop$total/dat.pop$attack.rate
dat.pop[60,-1] <- colSums(dat.pop[c(58,60),-1])
pop <- dat.pop$pop[-58]
y2 <- y/pop


setwd("/Users/niemi/Dropbox/fluControl/new/presentations/figures")
for (i in 1:ncol(y2)) {
	if (i%%4==0) {
	    pdf(paste("attackRatePoster-",i/4-1,".pdf",sep=""))
	    par(mar=c(0,0,0,0), mfrow=c(1,1))
	    clr <- clr.f1(y2[,i], ordr, 0, max(y2))
	    plot(zz, col=rgb(1,clr,clr))
	    dev.off()
	}
}

# Time series
xx <- as.Date(c("2010-05-24","2010-06-02"))
yy <- c(-100,10000)
#pdf("test.pdf", 8,20)
#par(mar=rep(0,4), mfcol=c(16,4))

library(ggplot2)
pdf("casesFeb.pdf", width=2.25, height=2/7*6)
qplot(dates[1:11], colSums(y)[1:11], xlim = range(dates), cex=2,
       xlab="", ylab="Confirmed cases", yaxt="n", cex.axis=2)+opts(legend.position = "none")+
       scale_y_continuous(limits=c(0,max(colSums(y)[1:11])),breaks=c(0,100,200))+
       scale_x_date(major="months",minor="weeks",format="%b")
#rect(xx[1],yy[1],xx[2],yy[2], col="green", border=NA)
dev.off()

pdf("casesApr.pdf", width=3.33, height=4/7*6)
qplot(dates[1:21], colSums(y)[1:21], xlim = range(dates), cex=2,
       xlab="", ylab="Confirmed cases", yaxt="n", cex.axis=2)+opts(legend.position = "none")+
       scale_y_continuous(limits=c(0,max(colSums(y)[1:21])))+
       scale_x_date(major="months",minor="weeks",format="%b")
#rect(xx[1],yy[1],xx[2],yy[2], col="green", border=NA)
dev.off()


pdf("casesDec.pdf", width=7, height=6)
qplot(dates, colSums(y), cex=2,
       xlab="", ylab="Confirmed cases", ylim=c(0,max(colSums(y))))+
       opts(legend.position = "none") +
       geom_rect(aes(NULL,NULL, xmin=xx[1], xmax=xx[2], fill=T, alpha=0.5), ymin=yy[1], ymax=yy[2])+
       scale_fill_manual(values="green", 0.2)+
       scale_x_date(major="months",minor="weeks",format="%b")
dev.off()

library(Hmisc)
wh <- which(y[,40]>=10)
length(wh)
for (j in 1:length(wh)) {
  i = wh[j]
  png(paste("ts",nc$district[i],".png",sep=""),4,2, units="in", res=600)
  q<-qplot(dates, y[i,], main=capitalize(as.character(nc$district[i])),xlab="", ylab="")+
       opts(legend.position = "none", axis.text.x=theme_blank()) +
       geom_rect(aes(NULL,NULL, xmin=xx[1], xmax=xx[2], fill=T, alpha=0.5), ymin=yy[1], ymax=yy[2])+
       scale_fill_manual(values="green")+
       scale_x_date(major="months",minor="weeks",format="%b")
  print(q)
  dev.off()
}

write.csv(data.frame(d=dates,y=colSums(y)), file="temp.csv", row.names=F)




