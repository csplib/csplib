# Lingeling blocks

# 1 N (size of board)
# 2 M (number of blocks to place)
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

blocks_xlim = c(3.9,12)
blocks_ns_nums = c(10,15,20,25,30,35,40,45,50,55,60)
blocks_ns = c('60','55','50','45','40','35','30','25','20','15','10')
blocks_types = c(11,10,9,8,7,6,5,4,3,2,1)

plotwidth=6
plotheight=5

blocks_timelim=c(0.001,7400)
blocks_meanlim=c(1,1e10)
xlabel="Mean number of unblocked squares per row/col"

kappa_size <- function(n) {
    0.5 * (1 + log(pi ,2) + log(n, 2) ) + n*(log(n,2)-log(2.54,2)-log(exp(1),2)) 
}

kappa_psol <- function(n,b) {
    n * (log(n-b/n,2)-log(n,2))
}

kappa_expectedsol <- function(n,b) { 
    kappa_psol(n,b) + kappa_size(n)
}

kappa <- function(n,b) { 
    1 - (kappa_expectedsol(n,b) / kappa_size(n))
}



# following for block2dimacs expts 

# n numblocks id status satisfiable timedout conflicts conflicts-p-s decisions decisions-p-s propagations Mprops-p-s lingeling-time total-usertime total-systemtime total-elapsedtime filename

filename <- "run-blocks-lingeling-raw.txt"

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



ld.out <- ld[,list(x=n-m/n,psat=mean(sat),ptimeout=mean(timeout),nodesmean=mean(nodes),timemean=mean(totaltime),totaltimemean=mean(totaltime+totalsystime),totaltimemedian=median(totaltime+totalsystime),med=as.double(median(nodes)),max=max(nodes)),by=c("n","m")]


ld3 <- ld.out[order(n,m)]
plotdata=ld3

pdf("blocks-lingeling-prob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$psat ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$psat[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
legend('bottomright',ncol=2,blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Probability solution exists")
# title(xlab=xlabel)
grid()
dev.off()

pdf("blocks-lingeling-timeoutprob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$ptimeout ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$ptimeout[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
# legend('topright',blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Probability timeout occurs")
# title(xlab=xlabel)
grid()
dev.off()

pdf("blocks-lingeling-mean.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
#plot(plotdata$nodesmean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=blocks_meanlim,xlim=blocks_xlim)
plot(plotdata$nodesmean ~ plotdata$x,ann=FALSE,type="n",log='y',xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$nodesmean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
# legend('topleft',ncol=2,blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Mean search nodes")
# title(xlab=xlabel)
grid()
dev.off()

pdf("blocks-lingeling-time.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$timemean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=blocks_timelim,xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$timemean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
# legend('topleft',blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Mean time taken")
# title(xlab=xlabel)
grid()
dev.off()

#### lingeling with kappa

require(data.table)

ldkappa.out <- ld[,list(x=kappa(n,m),psat=mean(sat),ptimeout=mean(timeout),nodesmean=mean(nodes),timemean=mean(totaltime),totaltimemean=mean(totaltime+totalsystime),totaltimemedian=median(totaltime+totalsystime),med=as.double(median(nodes)),max=max(nodes)),by=c("n","m")]

ldkappa3 <- ldkappa.out[order(n,m)]
plotdata=ldkappa3
xlabel = "Constrainedness parameter kappa"

pdf("blocks-kappa-lingeling-prob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$psat ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=c(0.5,1.4))
for (i in blocks_ns_nums) {
    lines(plotdata$psat[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
legend('bottomleft',ncol=2,blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Probability solution exists")
# title(xlab=xlabel)
grid()
dev.off()

pdf("blocks-kappa-lingeling-probzoom.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$psat ~ plotdata$x,ann=FALSE,type="n",ylim=c(0.5,1),xlim=c(0.9,1))
for (i in blocks_ns_nums) {
    lines(plotdata$psat[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
legend('bottomleft',ncol=2,blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
title(ylab="Probability solution exists")
title(xlab=xlabel)
grid()
dev.off()

pdf("blocks-kappa-lingeling-mean.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$nodesmean ~ plotdata$x,ann=FALSE,type="n",log='y',xlim=c(0.5,1.5))
for (i in blocks_ns_nums) {
    lines(plotdata$nodesmean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
# legend('topleft',blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Mean search nodes")
# title(xlab=xlabel)
grid()
dev.off()


# following for shiftqueens expts 

# n numblocks mininrow id filename numsols text nodes text timeout text time(s)
xlabel="Mean number of unblocked squares per row/col"

filename <- "run-blocks-shiftqueens-processed.txt" 

colnames <- c(
              'n', 'm', 'mininrow', 'id',
              'filename', 'sat', 
              'c7', 'nodes',
              'c8', 'timeout',
              'c9', 'totaltime'
              )

numbercols <- c('totaltime', 'nodes','sat','n','m','mininrow','timeout')

blocks_ns_nums = c(10,15,20,25,30,35)
blocks_ns = c('35','30','25','20','15','10')
blocks_types = c(6,5,4,3,2,1)


shiftqueensdata <- read.table(filename,col.names=colnames,header=FALSE)
sd <- data.table(shiftqueensdata)


sd.out <- sd[,list(x=n-m/n,psat=mean(sat),ptimeout=mean(timeout),nodesmean=mean(nodes),timemean=mean(totaltime),totaltimemean=mean(totaltime),totaltimemedian=median(totaltime),med=as.double(median(nodes)),max=max(nodes)),by=c("n","m")]
sd3 <- sd.out[order(n,m)]
plotdata=sd3



pdf("blocks-shiftqueens-prob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$psat ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$psat[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
legend('topright',blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Probability solution exists")
# title(xlab=xlabel)
grid()
dev.off()

pdf("blocks-shiftqueens-timeoutprob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$ptimeout ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$ptimeout[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
# legend('topright',blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Probability timeout occurs")
# title(xlab=xlabel)
grid()
dev.off()


pdf("blocks-shiftqueens-mean.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$nodesmean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=blocks_meanlim,xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$nodesmean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
# legend('topright',blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Mean search nodes")
# title(xlab=xlabel)
grid()
dev.off()

pdf("blocks-shiftqueens-time.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$timemean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=blocks_timelim,xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$timemean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
# legend('topright',blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Mean time taken")
# title(xlab=xlabel)
grid()
dev.off()

# following for minion 


filename="run-blocks-minion-strong-processed.txt"

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

blocks_ns_nums = c(10,15,20,25,30,35,40)
blocks_ns = c('40','35','30','25','20','15','10')
blocks_types = c(7,6,5,4,3,2,1)


# colnames.shifttime = c('delta','n','m','id','c5','sat','c7','nodes','c9','timeout','c11','solvetime')

miniondata <- read.table(filename,col.names=mcolnames,header=FALSE)
md <- data.table(miniondata)

md.out <- md[,list(x=n-m/n,psat=mean(sat),ptimeout=mean(timeout),nodesmean=mean(nodes),timemean=mean(totaltime),totaltimemean=mean(totaltime+srtime),totaltimemedian=median(totaltime+srtime),med=as.double(median(nodes)),max=max(nodes)),by=c("n","m")]
md3 <- md.out[order(n,m)]
plotdata=md3

pdf("blocks-minion-prob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$psat ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$psat[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
legend('topright',blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Probability solution exists")
# title(xlab=xlabel)
grid()
dev.off()

pdf("blocks-minion-timeoutprob.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$ptimeout ~ plotdata$x,ann=FALSE,type="n",ylim=c(0,1),xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$ptimeout[plotdata$n==i] ~ plotdata$x[plotdata$n==i],lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
# legend('topright',blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Probability timeout occurs")
# title(xlab=xlabel)
grid()
dev.off()

pdf("blocks-minion-mean.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$nodesmean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=blocks_meanlim,xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$nodesmean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
# legend('topright',blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Mean search nodes")
# title(xlab=xlabel)
grid()
dev.off()

pdf("blocks-minion-time.pdf",width=plotwidth,height=plotheight,useDingbats=FALSE)
plot(plotdata$timemean ~ plotdata$x,ann=FALSE,type="n",log='y',ylim=blocks_timelim,xlim=blocks_xlim)
for (i in blocks_ns_nums) {
    lines(plotdata$timemean[plotdata$n==i] ~ plotdata$x[plotdata$n==i],
          lwd=1,lty=i/5-1,pch=i/5-1,col=i/5-1,type='b')
}
# legend('topright',blocks_ns,lty=blocks_types,pch=blocks_types,col=blocks_types)
# title(ylab="Mean time taken")
# title(xlab=xlabel)
grid()
dev.off()

# print some useful info

length(ld$n[ld$timeout > 0])
length(md$n[md$timeout > 0])
length(sd$n[sd$timeout > 0])

max(ld3$timemean)
max(md3$timemean)
max(sd3$timemean)

max(ld3$nodesmean)
max(md3$nodesmean)
max(sd3$nodesmea)

max(ld3$x[ld3$psat < 1])


instructions <- "R --no-save < experiments-resubmission-blocks.R ; R --no-save < experiments-resubmission-diags.R ; for i in *mean.pdf *prob.pdf; do pdfcrop -margins 10 $i ;  done ; cp *mean-crop.pdf *prob-crop.pdf ~/git/papers/WIP/nQueensCompletion/graphs/provisional-graphs/"
quit()

