

# 1 N (size of board)
# 2 M (number of diagonals to exclude)
# 3 instance id 
# 4 result status text
# 5 sat boolean
# 6 timed out boolean
# 7 conflicts
# 8 conflicts per sec
# 9 nodes
# 10 nodes per sec
# 11 propagations
# 12 Megapropagations per sec
# 13 solve time
# 14 total time
# 15 total system time
# 16 total wall time
# 17 filename for result data

# if not already done then do following
# install.packages("data.table",repos="https://cloud.r-project.org/")

require(data.table)

diags_xlim = c(0,1.75)
diags_ns = c('21','20','19','18','17','16','15','14','13','12','11','10')
diags_types = c(12,11,10,9,8,7,6,5,4,3,2,1)

diags_timelim=c(0.001,200)
diags_meanlim=c(1,2.5e8)
plotheight=5
plotwidth=6


# following for block2dimacs expts 

# n numblocks id status satisfiable timedout conflicts conflicts-p-s decisions decisions-p-s propagations Mprops-p-s lingeling-time total-usertime total-systemtime total-elapsedtime filename

filename <- "run-diags-lingeling-raw.txt"

colnames <- c(
              'n', 'm', 'id',
              'status', 'sat', 'timeout', 
              'conflicts', 'conflictsps',
              'nodes', 'nodesps',
              'props', 'Mpropsps',
              'solvetime',
              'totaltime', 'totalsystime','totalwalltime',
              'filename'
              )

numbercols <- c('totaltime', 'nodes','sat','n','m','conflicts','props','solvetime')



lingelingdata <- read.table(filename,col.names=colnames,header=FALSE)
ld <- data.table(lingelingdata)



ld.out <- ld[,list(x=(m)/n,psat=mean(sat),ptimeout=mean(timeout),nodesmean=mean(nodes),timemean=mean(totaltime),totaltimemean=mean(totaltime+totalsystime),totaltimemedian=median(totaltime+totalsystime),med=as.double(median(nodes)),max=max(nodes)),by=c("n","m")]
xlabel="Number of disallowed diagonals / n"

ld3 <- ld.out[order(n,m)]
plotdata=ld3

pdf("diags-lingeling-prob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$psat ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
    lines(plotdata$psat[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
legend('topright',ncol=2,diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Probability solution exists")
# title(xlab=xlabel)
grid()
dev.off()

pdf("diags-lingeling-timeoutprob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$ptimeout ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
    lines(plotdata$ptimeout[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Probability solution exists")
# title(xlab=xlabel)
grid()
dev.off()

pdf("diags-lingeling-mean.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$nodesmean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=diags_meanlim,xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
#for (i in c(10,15,20,25,30,35,40,45,50,55,60)) {
    lines(plotdata$nodesmean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Mean search nodes")
# title(xlab=xlabel)
grid()
dev.off()

pdf("diags-lingeling-time.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$timemean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=diags_timelim,xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
    lines(plotdata$timemean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Mean time taken")
# title(xlab=xlabel)
grid()
dev.off()

# following for shiftqueens expts 

# n numdiags mininrow id filename numsols text nodes text timeout text time(s)

filename <- "run-diags-shiftqueens-processed.txt" 

colnames <- c(
              'n', 'm', 'mininrow', 'id',
              'filename', 'sat', 
              'c7', 'nodes',
              'c8', 'timeout',
              'c9', 'totaltime'
              )

numbercols <- c('totaltime', 'nodes','sat','n','m','mininrow','timeout')


shiftqueensdata <- read.table(filename,col.names=colnames,header=FALSE)
sd <- data.table(shiftqueensdata)


sd.out <- sd[,list(x=(m)/n,psat=mean(sat),ptimeout=mean(timeout),nodesmean=mean(nodes),timemean=mean(totaltime),totaltimemean=mean(totaltime),totaltimemedian=median(totaltime),med=as.double(median(nodes)),max=max(nodes)),by=c("n","m")]
xlabel="Number of disallowed diagonals / n"

sd3 <- sd.out[order(n,m)]
plotdata=sd3



pdf("diags-shiftqueens-prob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$psat ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
    lines(plotdata$psat[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Probability solution exists")
# title(xlab=xlabel)
grid()
dev.off()

pdf("diags-shiftqueens-timeoutprob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$ptimeout ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
    lines(plotdata$ptimeout[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Probability solution exists")
# title(xlab=xlabel)
grid()
dev.off()

pdf("diags-shiftqueens-med.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$med ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=diags_meanlim,xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
        lines(plotdata$med[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Median search nodes")
# title(xlab=xlabel)
grid()
dev.off()

pdf("diags-shiftqueens-max.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$max ~ plotdata$x,ann=FALSE,type="n",log='y',xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
        lines(plotdata$max[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Max search nodes")
# title(xlab=xlabel)
grid()
dev.off()


pdf("diags-shiftqueens-mean.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$nodesmean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=diags_meanlim,xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
#for (i in c(10,15,20,25,30,35,40,45,50,55,60)) {
    lines(plotdata$nodesmean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Mean search nodes")
# title(xlab=xlabel)
grid()
dev.off()

pdf("diags-shiftqueens-time.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$timemean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=diags_timelim,xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
    lines(plotdata$timemean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Mean time taken")
# title(xlab=xlabel)
grid()
dev.off()

# following for minion 


filename="run-diags-minion-strong-processed.txt"

# In the stats data the nostats exe has an id appended
# 1 N (size of board)
# 2 M (number of queens to place)
# 3 D (min number in row)
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

mcolnames <- c('n','m','d','id',
              'eprime', 'param','c3','c4','c5',
              'solvetime','totaltime',
              'setuptime','nodes',
              'timeout', 'sat',
              'srtime','c13',
              'srmemout','srtimeout',
              'numvars','numclauses')

numbercols <- c('totaltime','setuptime','nodes','sat','srtime','n','m','delta')



# colnames.shifttime = c('delta','n','m','id','c5','sat','c7','nodes','c9','timeout','c11','solvetime')

miniondata <- read.table(filename,col.names=mcolnames,header=FALSE)
md <- data.table(miniondata)

md.out <- md[,list(x=(m)/n,psat=mean(sat),ptimeout=mean(timeout),nodesmean=mean(nodes),timemean=mean(totaltime),totaltimemean=mean(totaltime+srtime),totaltimemedian=median(totaltime+srtime),med=as.double(median(nodes)),max=max(nodes)),by=c("n","m")]
md3 <- md.out[order(n,m)]
plotdata=md3

pdf("diags-minion-prob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$psat ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
    lines(plotdata$psat[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Probability that solution exists")
# title(xlab=xlabel)
grid()
dev.off()

pdf("diags-minion-timeoutprob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$ptimeout ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
    lines(plotdata$ptimeout[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Probability solution exists")
# title(xlab=xlabel)
grid()
dev.off()

pdf("diags-minion-mean.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$nodesmean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=diags_meanlim,xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
    lines(plotdata$nodesmean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Mean search nodes")
# title(xlab=xlabel)
grid()
dev.off()

pdf("diags-minion-time.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$timemean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=diags_timelim,xlim=diags_xlim)
for (i in c(10,11,12,13,14,15,16,17,18,19,20,21)) {
    lines(plotdata$timemean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i-9,pch=i-9,col=i-9,type='b')
}
# legend('topright',diags_ns,lty=diags_types,pch=diags_types,col=diags_types)
# title(ylab="Mean time taken")
# title(xlab=xlabel)
grid()
dev.off()

quit()

