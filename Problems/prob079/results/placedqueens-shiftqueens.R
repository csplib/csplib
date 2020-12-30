
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

shiftdata <- read.table('shiftqueens-paper-processed.txt',col.names=colnames.shift,header=FALSE)
sd <- data.table(shiftdata)

sd.out <- sd[,list(scaled=mean(n/((delta**2))),predictiond=mean(n-(0.19*delta*delta)),psat=mean(sat),nodesmean=mean(nodes),med=as.double(median(nodes)),max=max(nodes)),by=c("delta","n","m")]
sd3 <- sd.out[order(delta,n)]

pdf("placedqueens-shiftqueens-prob.pdf",width=6,height=6,useDingbats=FALSE)

plot(sd3$psat ~ sd3$n,ann=FALSE,type="n",ylim=c(0,1))

for (i in c(5,10,15,20,25,30)) {
    lines(sd3$psat[sd3$delta==i] ~ sd3$n[sd3$delta==i],lwd=1,lty=i/5,pch=i/5,col=i/5,type='b')
}
legend('topright',c('5','10','15','20','25','30'),lty=c(1,2,3,4,5,6),pch=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6))
title(ylab="Probability solution exists")
title(xlab="n")
grid()
dev.off()

pdf("placedqueens-shiftqueens-mean.pdf",width=6,height=6,useDingbats=FALSE)
plot(sd3$nodesmean ~ sd3$n,ann=FALSE,type="n",log='y')
for (i in c(5,10,15,20,25,30)) {
    lines(sd3$nodesmean[sd3$delta==i] ~ sd3$n[sd3$delta==i],lwd=1,lty=i/5,pch=i/5,col=i/5,type='b')
}
legend('topright',c('5','10','15','20','25','30'),lty=c(1,2,3,4,5,6),pch=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6))
title(ylab="Mean nodes searched")
title(xlab="n")
grid()
dev.off()

quit()


pdf("fig-rescaled-prob.pdf",width=6,height=6,useDingbats=FALSE)

plot(sd3$psat ~ sd3$scaled,ann=FALSE,type="n",xlim=c(0,0.45),ylim=c(0,1))
for (i in c(5,10,15,20,25,30)) {
    lines(sd3$psat[sd3$delta==i] ~ sd3$scaled[sd3$delta==i],lwd=1,lty=i/5,pch=i/5,col=i/5,type='b')
}
legend('topright',c('5','10','15','20','25','30'),lty=c(1,2,3,4,5,6),pch=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6))
grid()
dev.off()

pdf("fig-rescaled-mean.pdf",width=6,height=6,useDingbats=FALSE)
plot(sd3$nodesmean ~ sd3$scaled,ann=FALSE,type="n",log='y',xlim=c(0.00,0.45))
for (i in c(5,10,15,20,25,30)) {
    lines(sd3$nodesmean[sd3$delta==i] ~ sd3$scaled[sd3$delta==i],lwd=1,lty=i/5,pch=i/5,col=i/5,type='b')
}
legend('topright',c('5','10','15','20','25','30'),lty=c(1,2,3,4,5,6),pch=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6))
grid()
dev.off()

pdf("fig-rescaled-median.pdf",width=6,height=6,useDingbats=FALSE)
plot(sd3$med ~ sd3$scaled,ann=FALSE,type="n",log='y',xlim=c(0.00,0.45))
for (i in c(5,10,15,20,25,30)) {
    lines(sd3$med[sd3$delta==i] ~ sd3$scaled[sd3$delta==i],lwd=1,lty=i/5,pch=i/5,col=i/5,type='b')
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




alldata <- read.table('res40-lingeling-partial.txt',col.names=colnames,header=FALSE)

boxplot( dt$nodes[dt$nodes>0] ~ dt$m[dt$nodes>0],log="y")


dt2 =dt[dt$timeout == 0 & dt$totaltime < 1800]

dt.out <- dt2[, list(psat=mean(sat),nodesmean=mean(nodes),med=as.double(median(nodes))),by=c("delta","n")]
plot(dt.out$n/(dt.out$delta*log(dt.out$delta)),dt.out$psat)


plot(dt.out$m,dt.out$psat)



attach(all)
 
nfactor <- list(factor(n))
mfactor <- list(factor(m))


means <- aggregate(list(sat,nodes,solvetime,totaltime,setuptime,srtime),list(factor(n),factor(m)),mean)






meds <- aggregate(cpu,list(factor(noStatsExe),factor(inst)),median)

manymeds <- aggregate(list(cpu,cpuStats,confs,props,intProps,unblockedProps,totallyUnblockedProps,noWatchProps,newWatchProps,totalVarChecks,varChecksFailed,failedChecksWatchProps),list(factor(noStatsExe),factor(inst)),median)

colnames(meds) <- c("exe","inst","medcpu")
colnames(manymeds) <- c('exe','inst','cpu','cpuStats','confs',
              'props', 'intProps','unblockedProps','totallyUnblockedProps','noWatchProps',
              'newWatchProps','totalVarChecks','varChecksFailed','failedChecksWatchProps'
              )

levs <- levels(noStatsExe)
mininame <- levs[1]
twolitname <- levs[2]
looponename <- levs[3]

detach(all)
attach(manymeds)
o <- manymeds[which(exe == looponename),]
b <- manymeds[which(exe == mininame),]
t <- manymeds[which(exe == twolitname),]

baseloop <- merge(o,t,by=2,suffixes=c('.onepointer','.twopointer'))
baselooptwo <- merge(b,baseloop,by.x=2,by.y=1,sort=TRUE)

write.table(baselooptwo,"propagationMedians.txt",quote=FALSE,row.names=FALSE,col.names=TRUE)


detach(manymeds)
attach(all)

#meds[3] is median of cpu, meds[1] is exe

aggregate(meds[3],meds[1],summary)

#                                    Group.1   x.Min. x.1st Qu. x.Median   x.Mean
# 1       base_nostats_parednolearn_68b5_mac    1.886    32.570   78.130  182.600
# 2 twolitonly_nostats_parednolearn_2012_mac    2.007    33.000   81.080  155.900
# 3 wl_nostats_loopone_parednolearn_97f1_mac    1.899    30.290   73.100  141.600
#   x.3rd Qu.   x.Max.
# 1   125.900 4344.000
# 2   130.800 3920.000
# 3   115.000 3744.000

#  base vs oneloop
# each measure is better, with mean a bit more than 20% faster.


varchecksPerSec <- aggregate(totalVarChecks/cpu,list(factor(noStatsExe),factor(inst)),median)
aggregate(varchecksPerSec[3],varchecksPerSec[1],summary)

intProps <- aggregate(intProps/cpu,list(factor(noStatsExe),factor(inst)),median)
aggregate(intProps[3],intProps[1],summary)

confsPerSec <- aggregate(confs/cpu,list(factor(noStatsExe),factor(inst)),median)
aggregate(confsPerSec[3],confsPerSec[1],summary)

medsbase <- meds[which(meds[1]=="base_nostats_parednolearn_68b5_mac"),]
medsloop <- meds[which(meds[1]=="wl_nostats_loopone_parednolearn_97f1_mac"),]

baseloop <- merge(medsbase,medsloop,by=2)

# divide median time for base propagation by median time for oneloop
summary(baseloop[3]/baseloop[5])
      # x.x        
 # Min.   :0.8527  
 # 1st Qu.:0.9486  
 # Median :0.9875  
 # Mean   :1.1651  
 # 3rd Qu.:1.0568  
 # Max.   :9.5356  

# i.e. best for base is 15% faster.
# median behavior is base is 1.25% faster [interestingly]
# mean is that onleoop is 16% faster
# best for one loop is 9.5 TIMES faster.

# note that median is that base runs slightly faster than oneloop
# but median of raw times is better for oneloop
# Sounds contradictory!   

# Because oneloop is often much faster, where it is faster those instances can undershoot 
# the ones where base is faster [not very clear, sorry!]

# The advantage of being slightly faster on many instances is not enough to outweight being much 
# slower on some instances.

medstwo <- meds[which(meds[1]=="twolitonly_nostats_parednolearn_2012_mac"),]

basetwo <- merge(medsbase,medstwo,by=2)
summary(basetwo[3]/basetwo[5])

 # Min.   :0.7818  
 # 1st Qu.:0.8652  
 # Median :0.9013  
 # Mean   :1.0519  
 # 3rd Qu.:0.9740  
 # Max.   :7.6958  

# median 10% faster for base but mean still 5% faster for twolit

twoloop <- merge(medstwo,medsloop,by=2)
summary(twoloop[3]/twoloop[5])

# Min.   :0.8749  
# 1st Qu.:1.0739  
# Median :1.1030  
# Mean   :1.1016  
# 3rd Qu.:1.1291  
# Max.   :1.3459  

# only varies from 13% better to 34% worse, with both mean and median at 10% worse.


b <- baseloop[which(baseloop[3]>=2*baseloop[5]),]

# find the instances where oneloop is twice as fast 
# processed separately into file muchfaster_instances 
# minisat results grepped into file muchfaster_minisat.txt


colnames <- c('inst','exe','runID','resInt','cpu','confs','props','propsPerSec',
              'resString','vars','clauses','parseCpu','restarts','decisions',
              'conflictLiterals','mem','dataFileName') 

detach(all)
all <- read.table('muchfaster_minisat.txt',col.names=colnames)
attach(all)

ifactor <- list(factor(inst))
efactor <- list(factor(exe))
resIntMean <- aggregate(resInt,list(inst),mean)
allExesFailed <- resIntMean[which(resIntMean[2] == 3),][1]
failedFactor <- factor(allExesFailed$Group.1)
meds <- aggregate(cpu,(list(exe,inst)),median)

detach(all)
colnames(meds) <- c("exe","inst","medcpu")
attach(meds)

medsNotAllFail <- meds[which(!(inst %in% failedFactor)),]

detach(meds)
attach(medsNotAllFail)

efactor <- list(factor(exe))
aggregate(medcpu,efactor,summary)

#                      Group.1  x.Min. x.1st Qu. x.Median  x.Mean x.3rd Qu.
#1             minisat_2_2_mac   95.58    293.70   683.20  659.80   1010.00
#2 twolitonly_minisat_ade3_mac   11.05    153.20   604.40  633.70   1194.00
#3 wl_loopone_minisat_70de_mac   25.47    354.80   959.70  772.80   1196.00
   #x.Max.
#1 1198.00
#2 1197.00
#3 1197.00


# Only 14 instances and really not much to learn.   



#### Read from HERE to ... 

obpu <- (o$intProps-o$unblockedProps)/o$unblockedProps
tbpu <- (t$intProps-o$unblockedProps)/t$unblockedProps
bbpu <- (b$intProps-b$unblockedProps)/b$unblockedProps
ocps <- (o$confs/o$cpu)
tcps <- (t$confs/t$cpu)
bcps <- (b$confs/b$cpu)
cpsratio2 <- tcps/bcps
cpsratio <- ocps/bcps
bpuratio <- obpu/bbpu
bpuratio2 <- tbpu/bbpu

ofpnw <- (o$failedChecksWatchProps/ o$newWatchProps)
bfpnw <- (b$failedChecksWatchProps/ b$newWatchProps)
fpnwratio <- ofpnw/bfpnw

less <- which(bpuratio2 < 1.2)
more2 <- which(bpuratio2 >= 1.2)
length(more2)
# 122


two <- which(bpuratio<=1.20)
more <- which(bpuratio > 1.2)
lmra <- lm(cpsratio[more]~bpuratio[more])
lmra2 <- lm(cpsratio2[more2]~bpuratio2[more2])
lmrb2 <- lm(cpsratio2[less]~bpuratio2[less])

summary(lmra)
# Call:
# lm(formula = cpsratio[more] ~ bpuratio[more])
# 
# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.83210 -0.18617 -0.04759  0.09639  2.72261 
# 
# Coefficients:
               # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     0.52289    0.05945   8.795 1.18e-14 ***
# bpuratio[more]  0.54808    0.01871  29.299  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# Residual standard error: 0.423 on 121 degrees of freedom
# Multiple R-squared: 0.8765,	Adjusted R-squared: 0.8754 
# F-statistic: 858.4 on 1 and 121 DF,  p-value: < 2.2e-16 

lmrb <- lm(cpsratio[two]~bpuratio[two])
summary(lmrb)

#Call:
#lm(formula = cpsratio[two] ~ bpuratio[two])
#
#Residuals:
     #Min       1Q   Median       3Q      Max 
#-0.10741 -0.04002 -0.01404  0.02695  0.48142 
#
#Coefficients:
              #Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    0.87754    0.04504  19.485   <2e-16 ***
#bpuratio[two]  0.10203    0.04313   2.366   0.0184 *  
#---
###Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#
#Residual standard error: 0.06352 on 469 degrees of freedom
#Multiple R-squared: 0.01179,	Adjusted R-squared: 0.009684 
#F-statistic: 5.596 on 1 and 469 DF,  p-value: 0.01841 

# ... HERE
# What we see is that...
# Where blocks per unblocked is > 1.2 times better or more in OnePointer,
#   there is a very strong linear correlation between bpu-ratio and cps-ratio.
#   r^2 = 0.88
#   meaning that improved bpu explains most of improved cpu time

# Where blocks per unblocked ratio is <= 1.2, 
#   there is no correlation between bpu ratio and cps ratio
#   r^2 = 0.01

summary(lmra2)

# Call:
# lm(formula = cpsratio2[more2] ~ bpuratio2[more2])
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -0.9800 -0.1601 -0.0500  0.1025  2.3641 
# 
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.57194    0.05127   11.16   <2e-16 ***
# bpuratio2[more2]  0.43701    0.01595   27.40   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# Residual standard error: 0.3641 on 120 degrees of freedom
# Multiple R-squared: 0.8622,	Adjusted R-squared: 0.861 
# F-statistic: 750.7 on 1 and 120 DF,  p-value: < 2.2e-16 

summary(lmrb2)

# Call:
# lm(formula = cpsratio2[less] ~ bpuratio2[less])
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.11177 -0.04046 -0.01327  0.02772  0.52979 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      1.00393    0.04725  21.246   <2e-16 ***
# bpuratio2[less] -0.10191    0.04534  -2.248   0.0251 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# Residual standard error: 0.06558 on 470 degrees of freedom
# Multiple R-squared: 0.01063,	Adjusted R-squared: 0.00853 
# F-statistic: 5.052 on 1 and 470 DF,  p-value: 0.02506 

summary(cpsratio2[more2])
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.860   1.079   1.328   1.648   1.939   7.696 

summary(cpsratio2[less])
# 0.7818  0.8594  0.8833  0.8979  0.9238  1.4120 


# Same story with TwoPointer.   r^2 = .86 where bpu ratio >= 1.2
# but only 0.07 if bpuratio < 1.2. 

# Of 122 instances with >= 1.2 bpu ratio, mean 65% faster 
# Of other 472, mean is 10% (slower).  

