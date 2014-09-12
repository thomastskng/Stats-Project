#####################################################################
#																	#
#	Combine and compare 												#
#		(1) standard line, 											#
#		(2) linear regression, 										#
#		(3) regression with decay (univariate optimisation),			#
#		(4) regression with decay (multivariate optimisation)		#
#																	#
#####################################################################
setwd("/Users/thomastskng/Google Drive/RehabProject/data")
x<-read.csv(file="fromMYSQL.csv",header=T)
setwd("/Users/thomastskng/Google Drive/RehabProject/R_code")
source("newton_method_multivariate.R")
setwd("/Users/thomastskng/Google Drive/RehabProject/data")


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
sr$x<-log(b,base=10)
sr$y<-log(m/(232-m),base=10)

## Compute the time stamp differences in days.
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


## Fit regression WITH DECAY by using 6 data points and ignoring the first two. Keep the last 4 as outsample

tx<-table(vrr$PID)
ntx<-as.single(names(tx))

# standard line
stl <- matrix(0, nrow = length(ntx), ncol = 2)
stl_x <- matrix(0,nrow=length(ntx),ncol=12)

# linear regression
regr <- matrix(0, nrow = length(ntx), ncol = 5)
regr_x <- matrix(0,nrow=length(ntx),ncol=12)

# newton method ( univariate )
regr_nw_uni <- matrix(0,nrow=length(ntx),ncol=6)
regr_nw_uni_x <- matrix(0,nrow=length(ntx),ncol=12)
# newton method ( multivariate )
regr_nw_multi <- matrix(0, nrow = length(ntx), ncol = 6)
regr_nw_multi_x <- matrix(0,nrow=length(ntx),ncol=12)

# remove bad ones (cant optimize on them)
dont_want <- c(428,580,588,924,1074,1079,1156,1507,1572,1826,2115,2202,2305,2411)
target_vec <- c(1:length(ntx))[-dont_want]

for (ik in target_vec){	
	jnd<-(ik-1)*12+1:12
	tr<-vrr[jnd,]
##	xt<-tr[1:6,]
	xt<-tr[3:8,]
	fit<-lm(y~x,data=xt)
	aa<-summary(fit)
	cat(ik,"\t","PID: ",ntx[ik],"\n")
	# standard line
	stb<-(tr$y[4]-tr$y[3])/(tr$x[4]-tr$x[3])		# slope from 2nd set of cp1 and cp4
	sta<-tr$y[3]-stb*tr$x[3]						# intercept
	stl_x[ik,]<-(tr$y-sta)/stb						# x	from standard line equation (12 data points)
	stl[ik,]<-c(sta,stb)	

	# linear regression
	regr[ik,1:2]<-coef(fit)
	regr[ik,3]<-aa$sigma
	regr[ik,4]<-aa$r.squared
	regr[ik,5]<-aa$coefficients[2,4]				# p value of slope
	regr_x[ik,]<-(tr$y-regr[ik,1])/regr[ik,2]		# x from REGRESSION (from 12 data points)

	# newton method (multivariate)
	nw_multi <- newton.raphson(xt$y, xt$b,xt$days,3,coef(aa)[1,1],coef(aa)[2,1])
	regr_nw_multi[ik,1:2]<-nw_multi[1:2]
	regr_nw_multi[ik,3]<-nw_multi[3]
	regr_nw_multi[ik,4]<-nw_multi[4]	# r^2
	regr_nw_multi[ik,5]<-nw_multi[5]	# p value of slope
	regr_nw_multi[ik,6]<- nw_multi[6]
}


# insert column PID
stl <- cbind(ntx,stl)
regr <- cbind(ntx,regr)  	# Merge all summary statistics
regr_nw_multi <- cbind(ntx, regr_nw_multi)

stl_x <-cbind(ntx, stl_x)	 	# predicted standard line log(BAC) for 12 input values (col = 12)
regr_x <- cbind(ntx,regr_x)		# predicted log(BAC) from regression for 12 input values (col = 12)

# select those with good fit and significantly positive slope -- a form of validation
# ksel<-(1:nrow(regr))[regr[,5]>0.9 & regr[,3]>0 & regr[,6]<0.05]
# ksel1 <- (1:nrow(regr_nw_multi))[regr_nw_multi[,5]>0.9 & regr_nw_multi[,3] > 0 & regr_nw_multi[,6] < 0.05]
# ksel_final <- intersect(ksel, ksel1)
# write.csv(file = "ksel_final_multi.csv", ksel_final, row.names = F)
# ksel_uni <- read.csv(file = "ksel_final_uni.csv")
# ksel_multi <- read.csv(file = "ksel_final_multi.csv")

# ksel_uni <- as.vector(ksel_uni$x)
# ksel_multi <- as.vector(ksel_multi$x)
# ksel_final <- intersect(ksel_uni, ksel_multi)
# write.csv(file = "ksel_final_FINAL.csv", ksel_final, row.names = F)
ksel_final <- read.csv(file = "ksel_final_FINAL.csv")
ksel_final <- ksel_final$x

stl <- stl[ksel_final,]
regr <- regr[ksel_final,]
k_regr_nw_multi <- data.frame(regr_nw_multi[ksel_final,])

stl_x <- stl_x[ksel_final,]
regr_x <- regr_x[ksel_final,]

# Compute the haty for newton's method (multivariate) + dataframe  -> stored in "vrs"
for (ik in ksel_final){
	jnd<-(ik-1)*12+1:12
	tr<-vrr[jnd,]
	xt<-tr[3:12,]		# 3rd - 12th data points
	tp<-10^(xt$x)
	tx<-rep(0,10)
	tx[1]<-xt$x[1]
	r<-regr_nw_multi[ik,7]
	for (i in 2:10){ fac<-xt$days[i]-xt$days[(i-1):1]; tfac<-c(1,exp(-r*fac));
			temp<-tp[(i:1)]; tx[i]<-log(sum(temp*tfac),base=10)
	} 
	# same as tx <- xi(xt$b, xt$days, r)[1,]
	xt$tx<-tx
	xt$hy<-regr_nw_multi[ik,2]+ regr_nw_multi[ik,3]*tx
	xt$res<-xt$y-xt$hy	# residual 
	if (ik == ksel_final[1]) {vrs<-xt} else {vrs<-rbind(vrs,xt)}	
}

## Save the data for future use.
# contain devices wif good fit
# col1 = PID, 
# col2 = intercept, 
# col3 = slope, 
# col4= sigma, 
# col5 = r.squared, 
# col6 = pvalue of slope coefficient,
# col7 = optimal r 
write.csv(file="k_regr_nw_multi.csv", k_regr_nw_multi,row.names=F)

#contain devices with good fit (decay)
# col1= timestamp, 
# col2 = PID, 
# col3:6 = CP1:4 measurement,
# col7 = Condition, 
# col8 = m(current measurement),
# col9 = b (CP level),
# col10 = log(b, base=10),
# col11 = y (log m/232-m),
# col12 = days, 
# col13 = log(w_i),
# col14 = hat{y},
# col15 = residuals (y - yhat)
write.csv(file="vrs_shift_nw_multi.csv",vrs,row.names=F)

colnames(k_regr_nw_multi)<-c("PID","a","b","s","r2","pv","r")
hb <-matrix(0,nrow=nrow(k_regr_nw_multi),ncol=4) 	#estimated BAC from decay model 

# computed hat{BAC lvl} for regression with decay (newton method univariate)
for (ik in 1:nrow(k_regr_nw_multi)){
	jnd<-(ik-1)*10+1:10
	tr<-vrs[jnd,]
	t2<-tr[7:10,]
	hx<-(t2$y-k_regr_nw_multi$a[ik])/k_regr_nw_multi$b[ik]	#estimate x (regression with decay approach)
	ehx<-10^(hx)				#estimated w_i
	dtx<-10^t2$tx-t2$b			#left with the decay terms
	hatb<-ehx-dtx				#remove decay terms thus, estimated BAC
	hb[ik,]<-hatb
}

write.csv(file = "beta_hat_multi.csv", hb, row.names = F)

hb_uni <- data.frame(read.csv(file = "beta_hat_uni.csv"))



#compare standard line BAC VS regression with decay BAC
pdf(file = "VS.pdf", width = 12.69, height = 8.27)
par(mfrow = c(2,2))
lab<-c("st.line", "linear regression", "regression with decay (multi)")				
ri<-(1:nrow(k_regr_nw_multi))[stl_x[,10]<0]
boxplot(cbind(10^stl_x[ri,10], 10^ regr_x[ri,10], hb[ri,1]),names=lab)
abline(h=0.02,col=2)
title ("Outsample CP1 with Decay")
boxplot(cbind(10^stl_x[ri,12], 10^ regr_x[ri,12], hb[ri,3]),names=lab)
abline(h=0.04,col=2)
title ("Outsample CP2 with Decay")
boxplot(cbind(10^stl_x[ri,13], 10^ regr_x[ri,13], hb[ri,4]),names=lab)
abline(h=0.06,col=2)
title ("Outsample CP3 with Decay")
boxplot(cbind(10^stl_x[ri,11], 10^ regr_x[ri,11], hb[ri,2]),names=lab)
abline(h=0.08,col=2)
title ("Outsample CP4 with Decay")
dev.off()









#compare standard line BAC VS regression VS  regression with decay BAC (uni VS multi)
pdf(file = "VS.pdf", width = 12.69, height = 8.27)
par(mfrow = c(2,2))
lab<-c("st.line", "linear regression", "regression with decay(uni)", "regression with decay (multi)")				
ri<-(1:nrow(k_regr_nw_multi))[stl_x[,10]<0]
boxplot(cbind(10^stl_x[ri,10], 10^ regr_x[ri,10], hb_uni[ri,1], hb[ri,1]),names=lab)
abline(h=0.02,col=2)
title ("Outsample CP1 with Decay")
boxplot(cbind(10^stl_x[ri,12], 10^ regr_x[ri,12], hb_uni[ri,3],hb[ri,3]),names=lab)
abline(h=0.04,col=2)
title ("Outsample CP2 with Decay")
boxplot(cbind(10^stl_x[ri,13], 10^ regr_x[ri,13], hb_uni[ri,4],hb[ri,4]),names=lab)
abline(h=0.06,col=2)
title ("Outsample CP3 with Decay")
boxplot(cbind(10^stl_x[ri,11], 10^ regr_x[ri,11], hb_uni[ri,2],hb[ri,2]),names=lab)
abline(h=0.08,col=2)
title ("Outsample CP4 with Decay")
dev.off()

pdf(file = "OutSample Prediction Abs Error difference.pdf", width = 9.69, height = 8.27,)
par(mfrow=c(1,1))
lab<-paste("CP", 1:4, sep="")
d1<-abs(10^stl_x[ri,10]-0.02)-abs(hb[ri,1]-0.02)
d4<-abs(10^stl_x[ri,11]-0.08)-abs(hb[ri,2]-0.08)
d2<-abs(10^stl_x[ri,12]-0.04)-abs(hb[ri,3]-0.04)
d3<-abs(10^stl_x[ri,13]-0.06)-abs(hb[ri,4]-0.06)
boxplot(d1,d2,d3,d4,names=lab)
title("Decay:OutSample Prediction Abs. Err. Diff. (St. Line - Regression with decay)")
abline(h=0,col=2)
dev.off()

pdf(file = "OutSample Prediction Abs Error difference.pdf", width = 9.69, height = 8.27,)
par(mfrow=c(1,1))
lab<-paste("CP", 1:4, sep="")
d1<-abs(10^ regr_x[ri,10]-0.02)-abs(hb[ri,1]-0.02)
d4<-abs(10^ regr_x[ri,11]-0.08)-abs(hb[ri,2]-0.08)
d2<-abs(10^ regr_x[ri,12]-0.04)-abs(hb[ri,3]-0.04)
d3<-abs(10^ regr_x[ri,13]-0.06)-abs(hb[ri,4]-0.06)
boxplot(d1,d2,d3,d4,names=lab)
title("Decay:OutSample Prediction Abs. Err. Diff. (Regression - Regression with decay)")
abline(h=0,col=2)
dev.off()

pdf(file = "OutSample Prediction Abs Error difference.pdf", width = 9.69, height = 8.27,)
par(mfrow=c(1,1))
lab<-paste("CP", 1:4, sep="")
d1<-abs(hb_uni[ri,1]-0.02)-abs(hb[ri,1]-0.02)
d4<-abs(hb_uni[ri,2]-0.08)-abs(hb[ri,2]-0.08)
d2<-abs(hb_uni[ri,3]-0.04)-abs(hb[ri,3]-0.04)
d3<-abs(hb_uni[ri,4]-0.06)-abs(hb[ri,4]-0.06)
boxplot(d1,d2,d3,d4,names=lab)
title("Decay:OutSample Prediction Abs. Err. Diff. (Regression with decay - Regression with decay)")
abline(h=0,col=2)
dev.off()


