
# In the stats data the nostats exe has an id appended
# 1 N-M
# 2 N (size of board)
# 3 M (number of queens to place)
# 4 instance id 
# 5 problem eprime file
# 6 instance param file
# 7 if x cse switched on
# 8 x cse heuristic
# 9 not used
# 10 minion solve time 
# 11 solver total time
# 12 minion setup time
# 13 solver nodes
# 14 solver timeout bool
# 15 solver satisfiable bool
# 16 savile row time
# 17 ?? savile row clauseout
# 18 solver memout bool
# 19 savile row timeout 
# 20 numsat vars
# 21 numsat clauses 

# install.packages("data.table",repos="https://cloud.r-project.org/")
require(data.table)

colnames <- c('delta','n','m','id',
              'eprime', 'param','c3','c4','c5',
              'solvetime','totaltime',
              'setuptime','nodes',
              'timeout', 'sat',
              'srtime','c13',
              'srmemout','srtimeout',
              'numvars','numclauses')

numbercols <- c('totaltime','setuptime','nodes','sat','srtime','n','m','delta')


# colnames.shifttime = c('delta','n','m','id','c5','sat','c7','nodes','c9','timeout','c11','solvetime')

colnames.shift = c('delta','n','m','id','c5','sat','c7','nodes','c9','timeout')

shiftdata <- read.table('lingeling-processed-all.txt',col.names=colnames,header=FALSE)
sd <- data.table(shiftdata)

sd.out <- sd[,list(scaled=mean(n/((delta**2))),predictiond=mean(n-(0.19*delta*delta)),psat=mean(sat),nodesmean=mean(nodes),timemean=mean(totaltime+srtime),timemedian=median(totaltime+srtime),med=as.double(median(nodes)),max=max(nodes)),by=c("delta","n","m")]
sd3 <- sd.out[order(delta,n)]

pdf("placedqueens-lingeling-prob.pdf",width=6,height=6,useDingbats=FALSE)

plot(sd3$psat ~ sd3$n,ann=FALSE,type="n",xlim=c(0,400),ylim=c(0,1))

for (i in c(5,10,15,20,25,30)) {
    lines(sd3$psat[sd3$delta==i] ~ sd3$n[sd3$delta==i],lwd=1,lty=i/5,pch=i/5,col=i/5,type='b')
}
legend('topright',c('5','10','15','20','25','30'),lty=c(1,2,3,4,5,6),pch=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6))
title(ylab="Probability solution exists")
title(xlab="n")
grid()
dev.off()

pdf("placedqueens-lingeling-mean.pdf",width=6,height=6,useDingbats=FALSE)
plot(sd3$nodesmean ~ sd3$n,ann=FALSE,type="n",log='y')
for (i in c(5,10,15,20,25,30)) {
    lines(sd3$nodesmean[sd3$delta==i] ~ sd3$n[sd3$delta==i],lwd=1,lty=i/5,pch=i/5,col=i/5,type='b')
}
legend('topright',c('5','10','15','20','25','30'),lty=c(1,2,3,4,5,6),pch=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6))
title(ylab="Mean search nodes")
title(xlab="n")
grid()
dev.off()

quit()

pdf("lingeling-rescaled-median.pdf",width=6,height=6,useDingbats=FALSE)
plot(sd3$med+1 ~ sd3$scaled,ann=FALSE,type="n",log='y',xlim=c(0.00,0.45),ylab="Median search nodes+1")
for (i in c(5,10,15,20,25,30)) {
    lines(sd3$med[sd3$delta==i]+1 ~ sd3$scaled[sd3$delta==i],lwd=1,lty=i/5,pch=i/5,col=i/5,type='b')
}
legend('topright',c('5','10','15','20','25','30'),lty=c(1,2,3,4,5,6),pch=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6))
title(ylab="Median search nodes+1")
grid()
dev.off()

pdf("lingeling-rescaled-max.pdf",width=6,height=6,useDingbats=FALSE)
plot(sd3$max ~ sd3$scaled,ann=FALSE,type="n",log='y',xlim=c(0.00,0.45))
for (i in c(5,10,15,20,25,30)) {
  lines(sd3$max[sd3$delta==i] ~ sd3$scaled[sd3$delta==i],lwd=1,lty=i/5,pch=i/5,col=i/5,type='b')
}
legend('topright',c('5','10','15','20','25','30'),lty=c(1,2,3,4,5,6),pch=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6))
grid()
dev.off()



pdf("lingeling-rescaled-mean-time.pdf",width=6,height=6,useDingbats=FALSE)
plot(sd3$timemean ~ sd3$scaled,ann=FALSE,type="n",log='y',xlim=c(0.00,0.45))
for (i in c(5,10,15,20,25,30)) {
  lines(sd3$timemean[sd3$delta==i] ~ sd3$scaled[sd3$delta==i],lwd=1,lty=i/5,pch=i/5,col=i/5,type='b')
}
legend('topright',c('5','10','15','20','25','30'),lty=c(1,2,3,4,5,6),pch=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6))
grid()
dev.off()


pdf("lingeling-rescaled-median-time.pdf",width=6,height=6,useDingbats=FALSE)
plot(sd3$timemedian ~ sd3$scaled,ann=FALSE,type="n",log='y',xlim=c(0.00,0.45))
for (i in c(5,10,15,20,25,30)) {
  lines(sd3$timemedian[sd3$delta==i] ~ sd3$scaled[sd3$delta==i],lwd=1,lty=i/5,pch=i/5,col=i/5,type='b')
}
legend('topright',c('5','10','15','20','25','30'),lty=c(1,2,3,4,5,6),pch=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6))
grid()
dev.off()























plot(sd3$psat ~ sd3$scaled,ann=FALSE,type="n",ylim=c(0.35,0.7),xlim=c(0.18,0.20))
for (i in c(5,10,15,20,25,30)){
    lines(sd3$psat[sd3$delta==i] ~ sd3$scaled[sd3$delta==i],lwd=1,lty=1,pch=i-7,col=i-7,type='b')
}
legend('bottomleft',as.character(c(8:25)),xpd=TRUE,lty=1,pch=c(1:18),col=c(1:18),ncol=2)
grid()

plot(sd3$psat ~ sd3$predictiond,ann=FALSE,type="n",ylim=c(0.2,0.8),xlim=c(-2.5,2.5))
for (i in c(8:25)){
    lines(sd3$psat[sd3$delta==i] ~ sd3$predictiond[sd3$delta==i],lwd=1,lty=1,pch=i-7,col=i-7,type='b')
}
legend('bottomleft',as.character(c(8:25)),xpd=TRUE,lty=1,pch=c(1:18),col=c(1:18),ncol=4)
grid()


plot(sd3$psat ~ sd3$scaled,ann=FALSE,type="n",ylim=c(0.35,0.9),xlim=c(0.18,0.22))
for (i in c(8:25)){
    lines(sd3$psat[sd3$delta==i & sd3$psat>0.5] ~ sd3$scaled[sd3$delta==i & sd3$psat>0.5],lwd=1,lty=1,pch=i-7,col=1,type='b')
    lines(sd3$psat[sd3$delta==i & sd3$psat<=0.5] ~ sd3$scaled[sd3$delta==i & sd3$psat<=0.5],lwd=1,lty=1,pch=i-7,col=2,type='b')
}
legend('topright',as.character(c(8:25)),lty=1,pch=c(1:18),col=2)


plot(dt3$psat[dt3$delta==25] ~ dt3$n[dt3$delta==25],lwd=1,type='b',ylim=c(0.3,0.7))





points(dt3$psat ~ dt3$scaled,col=dt3$delta/5,pch=dt3$delta/5)


dt.10 =dt.out[dt.out$delta==10]
dt.35 =dt.out[dt.out$delta==35]

