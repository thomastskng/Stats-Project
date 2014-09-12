setwd("/Users/thomastskng/Google Drive/RehabProject/data")
x<-read.csv(file="fromMYSQL.csv",header=T)

## Eliminate the bad cases
x<-x[x$PID!=16777215,]
y<-x[x$PID<50001 | x$PID>50004 ,]
y<-y[y$PID!=53598,]
y<-y[y$PID<100000,]

## Eliminate the TPs and select only those that have been tested 12 times 
sy<-y[y$Condition=="cp1"|y$Condition=="cp2"|y$Condition=="cp3"|y$Condition=="cp4",]
sy$Condition<-as.character(sy$Condition)
tx<-table(sy$PID)
ntx<-names(tx[tx==12])  ## Pick the most frequent one.
ntx<-as.single(ntx)

ind<-1:nrow(sy)
k<-0;
for (i in 1:length(ntx)){k<-c(k,ind[sy$PID==ntx[i]]);}
k<-k[k>0];
sx<-sy[k,];  ## sx consists of the selected devices with 12 tests. 

jnd<-c(1:6,14)
sr<-sx[,jnd]  ## sr consists of only the time stamp, the binary 8-bit measurements and the condition
v<-substring(sr$Condition,3,3)
v<-as.single(v)
m<-rep(0,length(v))
for (i in 1:length(v)){m[i]<-sr[i,v[i]+2]}
b<-v*2*0.01
sr$m<-m
sr$b<-b
sr$x<-log(b,base=10)		# log_{10}(BAC)
sr$y<-log(m/(232-m),base=10)		# log_{10}(RLRS)

ts<-strptime(as.character(sr[,1]),format="%Y-%m-%d %H:%M:%S")
tx<-table(sr$PID)
ntx<-names(tx[tx==12])
tk<-0;
for (i in 1:length(ntx)){
	rnd<-(i-1)*12+1:12
	tts<-ts[rnd];
	d<-difftime(tts[-1],tts[1],unit="days")	#time1 (1st argument) - time2 (2nd argument)
	tk<-c(tk,c(0,d))
}
tk<-tk[-1]
sr$days<-tk

tx<-table(sr$PID)
ntx<-as.single(names(tx))
 ## Pick those only with zero initals at CP2, CP3 and CP4
k<-0;
for (i in 1:length(ntx)){
	jnd<-(i-1)*12+1:12
	tr<-sr[jnd,]
	v<-c(tr$CP2[1],tr$CP3[1],tr$CP4[1])
	if (sum(v==0)!=3){k<-c(k,jnd)}
}
k<-k[-1]
vr<-sr[-k,] 
tx<-table(vr$PID)
ntx<-as.single(names(tx))
 ## Remove those with abnormal entries
k<-0;
lab<-paste("cp",c(1,4,1,4,2,3),sep="")
lab<-c(lab,lab)
for (i in 1:length(ntx)){
	jnd<-(i-1)*12+1:12
	tr<-vr[jnd,]
	if (min(tr$m)<=1 | max(tr$m)>=232|sum(tr$Condition==lab)!=12) {k<-c(k,jnd)}
}
k<-k[-1]
vrr<-vr[-k,]

## Fit regression by using 6 data points and ignoring the first two. Keep the last 4 as outsample
tx<-table(vrr$PID)
ntx<-as.single(names(tx))
regr<-matrix(0,nrow=length(ntx),ncol=7)
vx<-matrix(0,nrow=length(ntx),ncol=12)
shx<-vx
for (ik in 1:length(ntx)){
	jnd<-(ik-1)*12+1:12
	tr<-vrr[jnd,]
	xt<-tr[3:8,]
	fit<-lm(y~x,data=xt)
	aa<-summary(fit)
	#illustrative purpose
	#plot(xt$x, xt$y)
	#abline(fit)
	#uni.opt.nw(xt$y, xt$b,xt$days,3,coef(aa)[1,1],coef(aa)[2,1])
	regr[ik,1:2]<-coef(fit)
	regr[ik,3]<-aa$sigma
	regr[ik,4]<-aa$r.squared
	regr[ik,5]<-aa$coefficients[2,4]				# p value of slope
	vx[ik,]<-(tr$y-regr[ik,1])/regr[ik,2]		# x from REGRESSION (from 12 data points)
	stb<-(tr$y[4]-tr$y[3])/(tr$x[4]-tr$x[3])		# slope from 2nd set of cp1 and cp4
	sta<-tr$y[3]-stb*tr$x[3]						# intercept
	#abline(sta,stb)
	shx[ik,]<-(tr$y-sta)/stb						# x	from standard line equation
	regr[ik,6:7]<-c(sta,stb)	
}
regs<-cbind(ntx,regr)  	# Merge all summary statistics
shx1<-cbind(ntx,shx)	 	# predicted standard line log(BAC) for 12 input values (col = 12)
vx1<-cbind(ntx,vx)		# predicted log(BAC) from regression for 12 input values (col = 12)




# select those with good fit and significantly positive slope -- a form of validation
ksel<-(1:nrow(regs))[regs[,5]>0.9 & regs[,3]>0 & regs[,6]<0.05] #rsquared > 0.9, regs[,3] >0 means +ve slope, regs[,6] p value < 0.05  --> return row number
sreg<-regs[ksel,]
shx2<-shx1[ksel,]
vx2<-vx1[ksel,]
# for tom ng 
nntx <- unique(vx2[,1])
## Compare the prediction
pdf(file = "comparePrediction.pdf", width = 11.69, height = 8.27)
par(mfrow=c(2,2))
lab<-c("st.line", "regression")
ri<-(1:nrow(sreg))[shx2[,10]<0]	# slice 
boxplot(cbind(10^shx2[ri,10],10^vx2[ri,10]),names=lab)	#CP1	#power of 10 to transform back from log scale
abline(h=0.02,col=2)
title ("Outsample CP1")
boxplot(cbind(10^shx2[ri,11],10^vx2[ri,11]),names=lab)	#CP2
abline(h=0.08,col=2)
title ("Outsample CP4")

boxplot(cbind(10^shx2[ri,12],10^vx2[ri,12]),names=lab)	#CP3
abline(h=0.04,col=2)
title ("Outsample CP2")

boxplot(cbind(10^shx2[ri,13],10^vx2[ri,13]),names=lab)	#CP4
abline(h=0.06,col=2)
title ("Outsample CP3")
dev.off()
## Compare prediction errors
pdf(file = "PredictionErrors.pdf", width = 11.69, height = 8.27)
par(mfrow=c(1,1))
lab<-paste("CP", 1:4, sep="")
d1<-abs(10^shx2[ri,10]-0.02)-abs(10^vx2[ri,10]-0.02)
d2<-abs(10^shx2[ri,12]-0.04)-abs(10^vx2[ri,12]-0.04)
d3<-abs(10^shx2[ri,13]-0.06)-abs(10^vx2[ri,13]-0.06)
d4<-abs(10^shx2[ri,11]-0.08)-abs(10^vx2[ri,11]-0.08)
boxplot(d1,d2,d3,d4,names=lab)
title("OutSample Prediction Abs. Err. Diff. (St. Line - Regression)")
abline(h=0,col=2)
dev.off()

## Evaluate the performance on the first 2 observations
pdf(file = "Training.pdf", width = 11.69, height = 8.27)
par(mfrow=c(2,2))
lab<-c("st.line", "regression")
boxplot(cbind(10^shx2[ri,4],10^vx2[ri,4]),names=lab)
abline(h=0.02,col=2)
title ("Reverse sample CP1")
boxplot(cbind(10^shx2[ri,6],10^vx2[ri,6]),names=lab)
abline(h=0.04,col=2)
title ("Reverse sample CP2")


boxplot(cbind(10^shx2[ri,7],10^vx2[ri,7]),names=lab)
abline(h=0.06,col=2)
title ("Reverse sample CP3")
boxplot(cbind(10^shx2[ri,5],10^vx2[ri,5]),names=lab)
abline(h=0.08,col=2)
title ("Reverse sample CP4")
dev.off()


## Compare corresponding training errors
pdf(file = "TrainingErrors.pdf", width = 11.69, height = 8.27)
par(mfrow=c(1,1))
lab<-paste("CP", c(1:4), sep="")
d1<-abs(10^shx2[ri,4]-0.02)-abs(10^vx2[ri,4]-0.02)
d4<-abs(10^shx2[ri,5]-0.08)-abs(10^vx2[ri,5]-0.08)
d2<-abs(10^shx2[ri,6]-0.04)-abs(10^vx2[ri,6]-0.04)
d3<-abs(10^shx2[ri,7]-0.06)-abs(10^vx2[ri,7]-0.06)
boxplot(d1,d2,d3,d4,names=lab)
#boxplot(d2,d3,names=lab)
title("Reverse Sample Prediction Abs. Err. Diff. (St. Line - Regression)")
abline(h=0,col=2)
dev.off()
