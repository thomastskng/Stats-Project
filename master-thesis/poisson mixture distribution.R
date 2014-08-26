library(MASS)

#passing multiple objects from functions in R
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}

#importing COUNTS data
#SOURCE IP COUNTS
src_ips_counts <- read.table("~/Google Drive/ICproject/March/src_ips_counts.txt", quote="\"")

#convert counts to vector and verify they are vectors
source_counts <- as.vector(src_ips_counts[,1])
is.vector(source_counts)

#DESTINATION IP COUNTS
dst_ips_counts <- read.table("~/Google Drive/ICproject/March/dst_ips_counts.txt", quote="\"")
dest_counts <- as.vector(dst_ips_counts[,1])
is.vector(dest_counts)

#EDGE COUNTS
edge_counts <- read.table("~/Google Drive/ICproject/March/src_dst_ips_counts.txt", quote="\"")
edge_counts <- as.vector(edge_counts[,1])
is.vector(edge_counts)

library(igraph)
#Edges 
src_dst_ips <- read.table("~/Google Drive/ICproject/March/src_dst_ips.txt", header = F, colClasses =c("character", "character"))

col1 <- as.vector(src_dst_ips[,1])
col2 <- as.vector(src_dst_ips[,2])
#become matrix
src_dst_ips <- cbind(col1,col2)
edges <- cbind(col1,col2)

#indegree IS A COUNT
indegree <- read.table("~/Google Drive/ICproject/March/dst_ips_degrees.txt", quote="\"")
indegree <- as.vector(indegree[,1])
is.vector(indegree)

#all IP address that ever appear as a src + dst
all_ips <- read.table("~/Google Drive/ICproject/March/ips.txt", header = F, colClasses =c("character"))
all_ips <- as.vector(all_ips[,1])

all_ips_counts <- read.table("~/Google Drive/ICproject/March/ips_counts.txt", quote="\"")
all_ips_counts <- as.vector(all_ips_counts[,1])

y <- order(all_ips_counts, decreasing = T)[1:5]
all_ips[y]

#outdegree IS A COUNT
outdegree <- read.table("~/Google Drive/ICproject/March/src_ips_degrees.txt", quote="\"")
outdegree <- as.vector(outdegree[,1])
is.vector(outdegree)
###########################################################################
m1 <- dir(path = "~/Google Drive/ICproject/august/median/m1", pattern = "_.txt")
m2 <- dir(path = "~/Google Drive/ICproject/august/median/m2", pattern = "_.txt")
m3 <- dir(path = "~/Google Drive/ICproject/august/median/m3", pattern = "_.txt")
m4 <- dir(path = "~/Google Drive/ICproject/august/median/m4", pattern = "_.txt")
m5 <- dir(path = "~/Google Drive/ICproject/august/median/m5", pattern = "_.txt")
m6 <- dir(path = "~/Google Drive/ICproject/august/median/m6", pattern = "_.txt")

t1313 <- dir(path ="~/Google Drive/ICproject/august/1313/", pattern = "_.txt")
t1717 <- dir(path ="~/Google Drive/ICproject/august/1717/", pattern = "_.txt")
t2323 <- dir(path ="~/Google Drive/ICproject/august/2323/", pattern = "_.txt")
t3333 <- dir(path ="~/Google Drive/ICproject/august/3333/", pattern = "_.txt")
t5555 <- dir(path ="~/Google Drive/ICproject/august/5555/", pattern = "_.txt")
t6000 <- dir(path ="~/Google Drive/ICproject/august/6000/", pattern = "_.txt")
t7000 <- dir(path ="~/Google Drive/ICproject/august/7000/", pattern = "_.txt")

t7500 <- dir(path ="~/Google Drive/ICproject/august/7500/", pattern = "_.txt")
t9000 <- dir(path ="~/Google Drive/ICproject/august/9000/", pattern = "_.txt")
t9591 <- dir(path ="~/Google Drive/ICproject/august/9591/", pattern = "_.txt")




processing <- function(string,string2){

	d <- get.node(string, string2)
	#chopping up time
	list[nt, wkday,wkend] <- divide.time(d)
	# list[m,day31] <- divide.day(d)
	# list[nt1] <- get.nighttime(m)
	# list[wkday1] <- get.wkday(m)
	
	# nt <- nt1[2,]
	# nt_test <- nt1[2,]
	# wkday <- wkday1[2,]
	# wkday_test <- wkday1[2,]
	nt_test <- nt
	wkday_test <- wkday
	wkend_test <- wkend
	#########################
	###   NIGHT TIME   ######
	#########################
	#jpeg(filename = "nt3.jpeg",  height = 600, width = 900)
	if( (length(nt[nt != 0 ]) != 0)	&& (length(nt_test[nt_test != 0 ]) != 0)){
		#testing data
		#create.plot(nt,nt_test, "Night time")
	}
	#dev.off()
	
	##############################
	###   WEEKDAY DAYTIME   ######
	##############################
	# jpeg(filename = "wkday3.jpeg",  height = 600, width = 900)	
	if( (length(wkday[wkday != 0]) != 0) && (length(wkday_test[wkday_test != 0]) != 0)) {
		#dev.new()
		#testing data
		#create.plot(wkday, wkday_test, "Weekday Daytime")
	}
	# dev.off()
	
	###############################
	###   WEEKEND DAYTIME  ########
	###############################	
	#jpeg(filename = "wkend3.jpeg",  height = 600, width = 900)
	if( (length(wkend[wkend !=  0]) != 0)  && (length(wkend_test[wkend_test != 0]) != 0))	{
		#dev.new()
		# #training data
		#create.plot(wkend, wkend_test, "Weekend Daytime")
	}	
		cat ("Night time", "\n")
		transit(nt)
		cat ("\n\n","Weekday", "\n")
		transit(wkday)
		cat("\n\n","Weekend", "\n")
		transit(wkend)
	#browser()
}

create.plot <- function(v, t, string)	{
	#par(mfrow = c(1,1))	 
    #define values of x for later use in graph
    x <- seq(1,1000,1)
	vec <- v[v !=0] - 1
	#plot pmf
    plot(table(v[v!=0])/ sum(table(v[v!=0])), type = "h", xlab = "x (active counts)", ylab = "P(X = x)", main = paste("pmf ( ",string," )",sep = "" ), cex = 1.5)
      
###################################
###	EM algorithm	###############
###################################
	#list[counter , lam1, p1, phi1] <- em.algo.pois.geo(vec, 1,0.8, c(0.3,0.7))
	#list[counter , lam, phi] <- em.algo.pois(2,vec, c(5,20), c(0.3,0.7))	
    #lines(seq(1,100,1), pois.geom.mix(seq(1,100,1) -1, phi1, lam1,p1), col = "blue" , lwd = 1.5)    
	#lines(seq(1,100,1), pois.mix(seq(1,100,1) -1, phi, lam), col = "red" , lwd = 1.5)   
	#legend("topright", legend = c("Pois mix" , "Pois-Geom mix") , col = c("red","blue"), lty = 1, cex = 1.5, bty = "n")
	#legend("right", legend = c(   as.expression(substitute(paste(phi[1], "= ", prob1, ", ",phi[2], "= ", prob2 ,sep = " "), list(prob1 = phi[1], prob2 = phi[2]))),
	#							as.expression(substitute(paste(lambda[1], "= ", param1,", ",lambda[2], "= ", param2, sep =" "), list(param1 = lam[1], param2 = lam[2]))),
	#							as.expression(substitute(paste(phi[lambda], "= ", prob1, ", ",phi[p], "= ", prob2, sep = " "), list(prob1 = phi1[1], prob2 = phi1[2]))),
	#							as.expression(substitute(paste(lambda, "= ", param1,", ", p, "= ", param2, sep = " "), list(param1 = lam1, param2 = p1)))  
	
	#		), text.col = c("red", "red", "blue", "blue"), bty = "n", cex = 1.5
	#)
#################################################################################
#####      SIMPLE DISCRETE DISTRIBUTION - minimum chi square estimation		#####
#################################################################################

	#poisson
	#cat("\t\t\t\t\t\t\t\t\t" ,"Poisson","\n")
	#list[poi.param] <- cqt.poi(vec)
	#lines(x, dpois(x-1 , lambda = poi.param), col = "red")
	#geometric
	#cat("\t\t\t\t\t\t\t\t\t","Geometric" ,"\n")	
	#list[geom.param] <- cqt.geom(vec)
	#lines(x, dgeom(x-1 ,prob = geom.param), col = "blue")
	#negative binomial
	#cat("\t\t\t\t\t\t\t\t\t" , "Negative Binomial" ,"\n") 
	#list[nb.param] <- cqt.nb(vec)
	#lines(x , dnbinom(x-1, size = nb.param[1], prob = nb.param[2]), col = "green")	
	#legend("topright", legend = c("Poisson", "Geometric" , "Negative Binomial") , col = c("red","blue","green"), lty = 1 , cex = 1.5 )
	#browser()
	#chi square test
	#cat("\t\t\t\t\t\t\t\t\t" , string, "\n")

##################################
####	discrete p value	######
##################################

	#hist(1- ppois(v-1, lambda = poi.param), freq = F, main = paste("discrete p-values (", string, " )", sep = ""), xlab = "p-values", cex = 1.5)
	#hist(1 - pgeom(v-1, prob = unlist(x)[1]) , freq = F)
	#hist(1 - pnbinom(v, size = unlist(z)[1], mu = unlist(z)[2]) , freq = F)
	#p.poi <- c() 
	#p.geom <- c()
	#p.nbinom <- c()
	#for(i in 1:length(vec))	{
		#s_x
		#s_x.poi <- 1 - ppois(vec[i] - 1 , unlist(poi.param)[1])
		# s_x.geom <- 1 - pgeom(vec[i] - 1, unlist(geom.param)[1])
		# s_x.nb <- 1 - pnbinom(vec[i] - 1, size = unlist(nb.param)[1], mu = unlist(nb.param)[2])
		# #s_x1
		#s_x1.poi <- 1 - ppois(vec[i]  , unlist(poi.param)[1])
		# s_x1.geom <- 1 - pgeom(vec[i] , unlist(geom.param)[1])
		# s_x1.nb <- 1 - pnbinom(vec[i], size = unlist(nb.param)[1], mu = unlist(nb.param)[2])
		
		#if(s_x.poi >= s_x1.poi)	{
			# corrected.p.poi <- runif(1, s_x1.poi , s_x.poi)
		#}
		# if(s_x.geom >= s_x1.geom)	{
			# corrected.p.geom <- runif(1, s_x1.geom , s_x.geom)
		# }
		# if(s_x.nb >= s_x1.nb)	{
			# corrected.p.nb <- runif(1 ,s_x1.nb , s_x.nb)
		# }

		#p.poi <- append(p.poi, corrected.p.poi)
		# p.geom <- append(p.geom , corrected.p.geom)
		# p.nbinom <- append(p.nbinom , corrected.p.nb)

	# }
	#hist(p.poi, freq = F)
	#hist(p.geom , freq = F)
	#hist(p.nbinom, freq = F)
	#browser()	    
    #plot empirical cdf
   	plot(ecdf(v[v != 0]), ylab = "F(x) = P(X <= x)", xlab = "x", main = paste("Empirical CDF ( ",string," )",sep = "" ),verticals = T, do.points = T, , cex = 1.5)
   	
   	#if doing EM as well: pass parameters: v[v != 0],t[t != 0] , phi, lam, phi1, lam1, p1, string
   	
   	#list[pval, pval.poi, pval.geo]p.value(v[v != 0],t[t != 0] , phi, lam, phi1, lam1, p1, string)
   	list[pval, pval.poi , pval.geo] <- p.value(v[v != 0],t[t != 0] ,string)
   	#cat("\t\t\t\t\t\t\t\t\t\t\t\t",string, "\n")
    #cat( length(which(duplicated(p) == T)),"duplicates in length",length(p), "\n")   	
	#browser()
}


#poisson mixture
#if doing EM as well , the parameters : v1,v2, phi,lam, phi1, lam1, p1,string
p.value <- function(v1,v2, string) {
	pvalues.poi <- c()
	pvalues.geo <- c()
	pvalues <- c()
    #vector of p values
    #poisson mix
    Fn <- ecdf(v1)

    for(i in 1:length(v2))  {
        observed <- v2[i]
     	#s_x
        s_x <- 1 - Fn(observed - 1)
        #s_x.poi <- 1 - ppois.mix(observed - 1, phi, lam)
        #s_x.geo <- 1 - ppois.geo.mix(observed - 1, phi1, lam1, p1)

        #s_x+1
        s_x1 <- 1 - Fn(observed)
        #s_x1.poi <- 1 - ppois.mix(observed, phi, lam)
        #s_x1.geo <- 1 - ppois.geo.mix(observed , phi1, lam1, p1)
        
        corrected.p <- runif(1, min = s_x1 , max = s_x)
        #corrected.p.poi <- runif(1, min = s_x1.poi , max = s_x.poi)
        #corrected.p.geo <- runif(1, min = s_x1.geo, max = s_x.geo)
        pvalues <- append(pvalues, corrected.p)
        #pvalues.poi <- append(pvalues.poi, corrected.p.poi)
        #pvalues.geo <- append(pvalues.geo , corrected.p.geo)
    }
    empirical <- print(ks.test(pvalues,"punif"))
    #mixture <- print(ks.test(pvalues.poi,"punif"))
    #mixture.two <- print(ks.test(pvalues.geo, "punif"))
	hist(pvalues, freq = F, main = paste("p-values from ecdf ( ",string, " )", sep = ""), xlab = "p-values" ,nclass = 20, cex = 1.5)    
	legend("topright", legend = paste("K-S test p-value: ", round(empirical$p.value,digits = 2) ,sep ="" ) , cex = 1.5, bty = "n", text.col = "red")
    #hist(pvalues.poi, freq = F, xlab ="p-values",main = paste("corrected p-values from poisson mixture","\n","(",string, " )", sep = "" ), nclass = 20)
    #legend("topright", legend = paste("K-S test p-value: ", mixture$p.value ,sep ="" ) , cex = 1.5, bty = "n")
   # hist(pvalues.geo , freq = F, xlab ="p-values", main = paste("corrected p-values from poi-geom mixture","\n ","( ",string, " )", sep = ""), nclass = 20)
    #legend("topright", legend = paste("K-S test p-value: ", mixture.two$p.value ,sep ="" ) , cex = 1, bty = "n")

    return
    {
    	list(pvalues, pvalues.poi , pvalues.geo)
    }
}


seasonality <- function(string, string2, input)	{	
	d <-get.node(string, string2)
	l <- 0
	if(input == 1)	{
		#day
		l <- 60 * 24
	} else	{
		#mweek
		l <- 60 * 24 *7
	}
	testing <- c()
	for(i in 1:length(d))	{
		if(i <= 43260)	{
			wrap <- rep(i %% l, d[i])
		} else if(i > 43260)	{
			wrap <- rep(i + 60 , d[i])
		}		
	testing <- append(testing, wrap)
	}

	tt <- table(factor(testing, levels = seq(1,l,1))) 
	if(input == 1)	{
		#day
		plot(tt / sum(tt), type = "l", xaxt = "n", xlab = "Hours in a day ", main = "Density Plot of IP count by Day (March)", ylab = "density")
		axis(1, at = c(seq(1,l,120),l), labels = c("12am", seq(2,22,2),"12am"))
	} else	{
		#week
		plot(tt / sum(tt), type = "l", xaxt = "n", xlab = "Days in a week", main = "Density Plot of IP count by Week (March)",ylab = "density")
		axis(1, at = c(seq(1,l,l / 7), l), labels = c("Fri 12am","Sat" ,"Sun","Mon", "Tues", "Wed", "Thurs", "Fri 12am"))
	}
}




par(mfrow = c(1,3))
processing("median/m6", m6[2])	

pdf(file = "mixturepval.pdf", width = 11.69, height = 8.27)
par(mfrow = c(1,2))
processing("7500", t7500[7])	
dev.off()

pdf(file = "discretepval2.pdf", width  = 11.69, height = 8.27)
par(mfrow = c(1,2))
processing("7500",t7500[7])
dev.off()

pdf(file = "empiricaltraining.pdf", width = 11.69, height = 8.27)
par(mfrow = c(1,3))
processing("median/m6", m6[2])	
dev.off()

pdf(file = "trainingipONE.pdf", width = 11.69, height = 8.27)
par(mfrow = c(2,3))
processing("7500",t7500[7])
dev.off()

pdf(file = "trainingipTWO.pdf", width = 11.69, height = 8.27)
par(mfrow = c(2,3))
processing("5555", t5555[1])
dev.off()


processing("9000", t9000[10])

processing("1717", t1717[5])	
processing("5555", t5555[1])


processing("6000", t6000[15])
processing("6000", t6000[20])
processing("6000", t6000[33])
processing("6000", t6000[35])

processing("7000", t7000[4]) 
processing("7000", t7000[10])

#nighttime
#nighttimeMIXTURE
pdf(file = "nighttimeMIXRURE.pdf", width = 11.69, height = 8.27)
par(mfrow = c(2,2))
processing("median/m6", m6[5])
processing("6000", t6000[35])
processing("7500", t7500[7])	
processing("5555", t5555[1])
dev.off()

#wkday daytime
#wkdaydaytimeMIXTURE
pdf(file = "wkdaydaytimeMIXTURE.pdf", height = 8.27, width = 11.69)
par(mfrow = c(2,2))
processing("median/m6", m6[1])
processing("median/m6", m6[2])
processing("7500", t7500[7])	
processing("6000", t6000[35])
dev.off()

pdf(file = "wkenddaytime.pdf" , height = 900, width = 1050)
par(mfrow = c(2,2))
processing("6000", t6000[35])
processing("5555", t5555[1])
processing("median/m6", m6[1])
processing("7500", t7500[7])	
dev.off()



#no zeros then add zeros
y <- rpois(30000,4.33)
p1 <- dpois(min(y):max(y), 4.33)
p1 <- c(p1, 1-sum(p1))
names(p1) <- c(min(y):max(y), "more")


#seasonality
#why choose time segments like dis 
par(mfrow = c(2,1))
seasonality("median/m6", m6[4],1)
seasonality("median/m6", m6[4],2)

#normal behaviour
pdf(file = "", width = 11.69, height = 8.27)

dev.off()


#before 8pm
par(mfrow = c(2,1))
seasonality("median/m2", m2[8],1)
seasonality("median/m2", m2[8],2)
par(mfrow = c(2,1))
seasonality("median/m6", m6[4],1)
seasonality("median/m6", m6[4],2)
par(mfrow = c(2,1))
seasonality("median/m6", m6[8],1)
seasonality("median/m6", m6[8],2)
#before 10pm
pdf(file = "normalbehaviour1.pdf", height = 8.27, width = 11.69)
par(mfrow = c(2,1))
seasonality("median/m6", m6[6],1)
legend("topright", legend = "100.253.169.162", bty = "n")
seasonality("median/m6", m6[6],2)
legend("topright", legend = "100.253.169.162", bty = "n")
dev.off()
par(mfrow = c(2,1))
seasonality("median/m6", m6[9],1)
seasonality("median/m6", m6[9],2)
par(mfrow = c(2,1))
seasonality("median/m6", m6[3],1)
seasonality("median/m6", m6[3],2)
par(mfrow = c(2,1))
seasonality("7000", t7000[1],1)
seasonality("7000", t7000[1],2)
par(mfrow = c(2,1))
seasonality("7000", t7000[5],1)
seasonality("7000", t7000[5],2)
par(mfrow = c(2,1))
seasonality("7000", t7000[7],1)
seasonality("7000", t7000[7],2)
par(mfrow = c(2,1))
seasonality("7000", t7000[8],1)
seasonality("7000", t7000[8],2)

pdf(file = "normalbehaviour2.pdf", height = 8.27, width = 11.69)
par(mfrow = c(2,1))
seasonality("7000", t7000[10],1)
legend("topright", legend = "100.253.170.35", bty = "n")
seasonality("7000", t7000[10],2)
legend("topright", legend = "100.253.170.35", bty = "n")
dev.off()
par(mfrow = c(2,1))
seasonality("7000", t7000[11],1)
seasonality("7000", t7000[11],2)
par(mfrow = c(2,1))
seasonality("7000", t7000[13],1)
seasonality("7000", t7000[13],2)
par(mfrow = c(2,1))
seasonality("7000", t7000[14],1)
seasonality("7000", t7000[14],2)
par(mfrow = c(2,1))
seasonality("7000", t7000[21],1)
seasonality("7000", t7000[21],2)

pdf(file = "normalbehaviour3.pdf", height = 8.27, width = 11.69)
par(mfrow = c(2,1))
seasonality("7500", t7500[21],1)
legend("topright", legend = "126.253.151.115", bty = "n")
seasonality("7500", t7500[21],2)
legend("topright", legend = "126.253.151.115", bty = "n")
dev.off()
par(mfrow = c(2,1))
seasonality("7500", t7500[8],1)
seasonality("7500", t7500[8],2)

par(mfrow = c(2,1))
seasonality("7500", t7500[10],1)
seasonality("7500", t7500[10],2)

par(mfrow = c(2,1))
seasonality("7500", t7500[12],1)
seasonality("7500", t7500[12],2)
par(mfrow = c(2,1))
seasonality("7500", t7500[13],1)
seasonality("7500", t7500[13],2)
par(mfrow = c(2,1))
seasonality("7500", t7500[15],1)
seasonality("7500", t7500[15],2)

par(mfrow = c(2,1))
seasonality("7500", t7500[3],1)
seasonality("7500", t7500[3],2)
par(mfrow = c(2,1))
seasonality("9000", t9000[7],1)
seasonality("9000", t9000[7],2)
par(mfrow = c(2,1))
seasonality("9000", t9000[20],1)
seasonality("9000", t9000[20],2)
#before 11pm
par(mfrow = c(2,1))
seasonality("median/m6", m6[10],1)
seasonality("median/m6", m6[10],2)

par(mfrow = c(2,1))
seasonality("7000", t7000[20],1)
seasonality("7000", t7000[20],2)

par(mfrow = c(2,1))
seasonality("7500", t7500[19],1)
seasonality("7500", t7500[19],2)
par(mfrow = c(2,1))
seasonality("9000", t9000[18],1)
seasonality("9000", t9000[18],2)
par(mfrow = c(2,1))
seasonality("9000", t9000[17],1)
seasonality("9000", t9000[17],2)
#before 12pm
par(mfrow = c(2,1))
seasonality("9000", t9000[19],1)
seasonality("9000", t9000[19],2)

par(mfrow = c(2,1))
seasonality("9000", t9000[1],1)
seasonality("9000", t9000[1],2)

#weird behaving at morning
par(mfrow = c(2,1))
seasonality("7000", t7000[9],1)
seasonality("7000", t7000[9],2)
par(mfrow = c(2,1))
seasonality("7000", t7000[15],1)
seasonality("7000", t7000[15],2)
#hardworking
par(mfrow = c(2,1))
seasonality("7000", t7000[18],1)
seasonality("7000", t7000[18],2)
par(mfrow = c(2,1))
seasonality("7000", t7000[19],1)
seasonality("7000", t7000[19],2)
#always active
par(mfrow = c(2,1))
seasonality("7000", t7000[12],1)
seasonality("7000", t7000[12],2)
par(mfrow = c(2,1))
seasonality("9000", t9000[10],1)
seasonality("9000", t9000[10],2)
#why not median   m3[5], m3[1], m3[15]
pdf(file = "whynotmedian1.pdf", height = 8.27, width = 11.69)
par(mfrow = c(2,1))
seasonality("median/m3", m3[5],1)
legend("topright", legend = "IP 1", cex = 1,bty = "n")
seasonality("median/m3", m3[5],2)
legend("topright", legend = "IP 1", cex = 1, bty = "n")
processing("median/m3", m3[5])
dev.off()
pdf(file = "whynotmedian2.pdf", height = 8.27, width = 11.69)
par(mfrow = c(2,1))
seasonality("median/m3", m3[1],1)
legend("topright", legend = "IP 2", cex = 1,bty = "n")
seasonality("median/m3", m3[1],2)
legend("topright", legend = "IP 2", cex = 1,bty = "n")
dev.off()
pdf(file = "whynotmedian3.pdf", height = 8.27, width = 11.69)
par(mfrow = c(2,1))
seasonality("median/m3", m3[15],1)
legend("topright", legend = "IP 3", cex = 1,bty = "n")
seasonality("median/m3", m3[15],2)
legend("topright", legend = "IP 3", cex = 1,bty = "n")
dev.off()


par(mfrow = c(6,1))
seasonality("median/m1", m1[2],1)
seasonality("median/m1", m1[2],2)


par(mfrow = c(2,1))
seasonality("median/m1", m1[12],1)
seasonality("median/m1", m1[12],2)


#need4
par(mfrow = c(2,1))
seasonality("median/m4", m4[13],1)
seasonality("median/m4", m4[13],2)



par(mfrow = c(2,1))
seasonality("median/m2", m2[16],1)
seasonality("median/m2", m2[16],2)

par(mfrow = c(2,1))
seasonality("median/m1", m1[8],1)
seasonality("median/m1", m1[8],2)
par(mfrow = c(2,1))
seasonality("median/m1", m1[9],1)
seasonality("median/m1", m1[9],2)
par(mfrow = c(2,1))
seasonality("median/m1", m1[10],1)
seasonality("median/m1", m1[10],2)

par(mfrow = c(2,1))
seasonality("median/m1", m1[14],1)
seasonality("median/m1", m1[14],2)
par(mfrow = c(2,1))
seasonality("median/m2", m2[15],1)
seasonality("median/m2", m2[15],2)
par(mfrow = c(2,1))
seasonality("median/m2", m2[16],1)
seasonality("median/m2", m2[16],2)
par(mfrow = c(2,1))
seasonality("median/m2", m2[17],1)
seasonality("median/m2", m2[17],2)
par(mfrow = c(2,1))
seasonality("median/m2", m2[18],1)
seasonality("median/m2", m2[18],2)



par(mfrow = c(2,1))
seasonality("median/m5", m5[15],1)
seasonality("median/m5", m5[15],2)
par(mfrow = c(2,1))
seasonality("median/m5", m5[18],1)
seasonality("median/m5", m5[18],2)


#wierd behaving IP
par(mfrow = c(2,1))
seasonality("median/m2", m2[11],1)
seasonality("median/m2", m2[11],2)
par(mfrow = c(2,1))
seasonality("median/m2", m2[12],1)
seasonality("median/m2", m2[12],2)

par(mfrow = c(2,1))
seasonality("median/m2", m2[14],1)
seasonality("median/m2", m2[14],2)

par(mfrow = c(2,1))
seasonality("median/m4", m4[10],1)
seasonality("median/m4", m4[10],2)
par(mfrow = c(2,1))
seasonality("median/m6", m6[7],1)
seasonality("median/m6", m6[7],2)
par(mfrow = c(2,1))
seasonality("median/m6", m6[5],1)
seasonality("median/m6", m6[5],2)

# 3 IP address at the end
pdf(file = "behaviour7000", width = 11.69, height = 8.27)
par(mfrow = c(2,1))
seasonality("7000", t7000[4],1)
seasonality("7000", t7000[4],2)
dev.off()

pdf(file = "behaviour7500", width = 11.69, height = 8.27)
par(mfrow = c(2,1))
seasonality("7500", t7500[7],1)
seasonality("7500", t7500[7],2)
dev.off()

pdf(file = "behaviour5555", width = 11.69, height = 8.27)
par(mfrow = c(2,1))
seasonality("5555", t5555[1],1)
seasonality("5555", t5555[1],2)
dev.off()












# 1823018531 -> 108.169.14.35
#convertting decimal to dotted quad
to.dotted.quad <- function(v)	{
	int <- v
	vec <- c()
	#for(j in 1:length(v))	{
		#int <- as.integer( unlist(strsplit(fnn[v[j]], "[:_.txt:]"))[1] )
	
		four <- int %% 256
		three <- int %/% 256 %% 256
		two <- int %/% 256 %/% 256 %% 256
		one <- int %/% 256 %/% 256 %/% 256 %% 256
		vec <- append(vec, paste(one,".",two,".",three,".",four, sep = "" ))
		cat(paste(one,".",two,".",three,".",four, sep = "" ), "\n")
	#}
	return
	{
		list(vec)
	}
}









############################################################################
chisq.test(table(factor(y, levels = c(min(y):max(y), "more") )), p = p1)

y <- rpois(3000,3)
y <- y[y!=0]
p1 <- dpois(0:max(y), 3)
p1 <- c(p1, 1-sum(p1))
names(p1) <- c(0:max(y), "more")


chisq.test(table(factor(y, levels = c(0:max(y), "more") )), p = p1)

###################################################################
#get data
get.node <- function(l,string) {
	node.in <- read.table(paste("~/Google Drive/ICproject/august/",l,"/", string, sep = ""), quote="\"")
	node.in <- as.vector(node.in[,1])	
	# y <- unlist(strsplit(string, "[:_.txt:]"))	
	#if(grep("[0-9]", y) == 1) {
		# #outgoing
		# jpeg(filename = paste(y[grep("[0-9]", y)], "out.jpeg",sep = ""), height = 600, width = 900)
	# } else {
		# #ingoing
		# jpeg(filename = paste(y[grep("[0-9]", y)], "in.jpeg",sep = ""), height = 600, width = 900)
	# }	
	# par(mfrow = c(1,2))
	# plot(node.in, cex = 0.5)
	# hist(node.in[node.in != 0], nclass = 800)
	# dev.off()
	#print(table(node.in))
	return
	{
		node.in
	}
}

###################################################
#####TRANSITION MATRIX#############################
###################################################
#given current minute inactive , next minute active
ia <- function(x) {
	c(x[-length(x)] == 0 & x[-1]> 0, FALSE)
}


#given current minute inactive, next minute inactive
ii <- function(x) {
	c(x[-length(x)] == 0 & x[-1] == 0, FALSE)
}

#given current minute active, next minute inactive
ai <- function(x) {
	c(x[-length(x)] > 0 & x[-1] == 0, FALSE)
}

#given current minute active, next minute active
aa <- function(x) {
	c(x[-length(x)] > 0 & x[-1] > 0, FALSE)
}

# markov model transition matrix
transit <- function(vect) {
	t <- matrix (NA, nrow = 2, ncol = 2)
	colnames(t) = c("inactive", "active")
	rownames(t) = c("inactive", "active")
	#state 1 = inactive
	#state 2 = active
	t[1,1] <-  ( sum(ii(vect) == T) )/ length(vect[vect == 0])
	t[1,2] <- ( sum(ia(vect) == T) )/ length(vect[vect == 0])
	t[2,1] <- sum(ai(vect) == T) / length(vect[vect != 0])
	t[2,2] <- sum(aa(vect) == T) / length(vect[vect != 0])
	print(t)

	cat("row 1 = ", t[1,1] + t[1,2], "\n")
	cat("row 2 = ", t[2,1] + t[2,2], "\n")
}



###MARCH ONLY
#nighttime   10:00pm - 08:00am	
#daytime		 08:00am - 08:00pm 
#week by week comparison
divide.day <- function(d)	{
	#1st March -> 31st March
	day <- matrix(0, nrow = 30 , ncol = 1440)
	for(i in 0:29)	{
		j <- i * 1440
		day[i,] <- d[(j+1) : (j + 1440)]
	}
	j <- 30 * 1440
	day31 <- d[(j+1) : (j+1380)]
	return
	{
		list(day, day31)
	}
}

#spit night time
get.nighttime <- function(mat)	{
	#3rd Mar -> #8th Mar
	nt <- matrix(0, nrow = 4, ncol = 5040)
	nt_day <- c()
	day <- 1
	week <- 1
	#start from sunday 3th March to saturday 30st March: 4 weeks
	for(i in 3:30)	{
		#00:00am - 08:00am + 08:00pm - 00:00am
		nt_day <- append( nt_day, mat[i,c(1:480 , 1201:1440)] )
		if(day == 7)	{
			nt[week,] <- nt_day
			nt_day <- c()
			week <- week + 1
			day <- 1
		} else	{
			day <- day + 1		
		}
	}
	return
	{
		list(nt)
	}
}
#spit days
get.wkday <-function(mat) {
	#4 March - 29th March: 4 weeks
	wkday <- matrix(0, nrow = 4, ncol = 3600)
	daytime <- c()
	day <- 1
	week <- 1
	for(i in c(4:8,11:15,18:22, 25:29) )	{
		daytime <- append(daytime, mat[i, c(481:1200)])
		if(day == 5)	{
			wkday[week,] <- daytime
			daytime <- c()
			week <- week + 1
			day <- 1
		} else	{
			day <- day + 1
		}
	}
	return
	{
		list(wkday)
	}
}

###MARCH ONLY
#nighttime   08:00pm - 08:00am	
#daytime		 08:00am - 08:00pm 
divide.time <- function(d)	{
	
	#chopping up time 
	nt = c()
	wkend = c()
	wkday = c()
	day <- 5
	for(i in 0:29) {
		j <- i*1440
		nt <- append(nt, d[(j+1):(j + 480)])
		j <- j + 480
		if(day %% 6 == 0 || day %% 7 == 0) {
			wkend <- append(wkend, d[(j+1):(j + 840)])		
		} else {
			wkday <- append(wkday, d[(j+1):(j + 840)])		
		}
		j <- j + 840
		nt <- append(nt, d[(j+1):(j + 120)])
		if(day == 7) {
			day <- 1
		} else {
			day <- day + 1
		}	
	}
		#Change to summer time in March	
		j <- 30*1440
		nt <- append(nt, d[(j+1): (j + 420)])
		j <- j + 420
		if(day %% 6 == 0 || day %% 7 == 0) {
			wkend <- append(wkend, d[(j+1):(j + 840)])		
		} else {
			wkday <- append(wkday, d[(j+1):(j + 840)])		
		}
		j <- j + 840
		nt <- append(nt, d[(j+1):(j+120)])

		return
		{
			list(nt, wkday, wkend)
		}
}


#minimum Chi square test estimation

#poisson
cqt.poi <- function(vec)	{
	tx <- table(vec)
	#concatenate from the 12th cell to the end such that observed freq > 10
	ttx <- c(tx[1:13] , sum(tx[14:length(tx)]))
	#browser()
	#use newton's method in optim to estimate min.chisquare lambda	
	ksq <- function(lam)	{
		ep <- dpois(0:12, lambda = lam)
		eep <- c(ep, 1 - sum(ep))
		ei <- sum(ttx)*eep
		ts <- sum((ei - ttx)^2 / ei)
		return
		{
			ts
		}
	}
	ilam <- sum(tx*as.integer(names(tx))) / sum(tx)    #mean of x is a good initial estimate 
	minchi <- optim(par = ilam , fn = ksq)
	flam <- minchi$par   #minimum chisquare estimate 
	test <- minchi$value	#minimum chi square test statistics
	pvalue <- 1 - pchisq(test, df = length(ttx) - 2)   #the p -value
	cat("min chi square test statistics", test, "\n")
	cat("pvalue = " ,pvalue , "\n")
	cat("final lambda,", flam, "\n")
	return
	{
		list(flam)	
	}
}
#geometric
cqt.geom <- function(vec)	{
	tx <- table(vec)
	#concatenate from the 12th cell to the end such that observed freq > 10
	ttx <- c(tx[1:13] , sum(tx[14:length(tx)]))

	#use newton's method in optim to estimate min.chisquare lambda	
	ksq <- function(p)	{
		ep <- dgeom(0:12, prob = p)
		eep <- c(ep, 1 - sum(ep))
		ei <- sum(ttx)*eep
		ts <- sum((ei - ttx)^2 / ei)
		return
		{
			ts
		}
	}
	initial.p <- 0.3    #mean of x is a good initial estimate 
	minchi <- optim(par = initial.p , fn = ksq)
	final.p <- minchi$par   #minimum chisquare estimate 
	test <- minchi$value	#minimum chi square test statistics
	pvalue <- 1 - pchisq(test, df = length(ttx) - 2)   #the p -value
	cat("min chi square test statistics", test, "\n")
	cat("pvalue = " ,pvalue , "\n")
	cat("final geom p ,", final.p, "\n")
	return
	{
		list(final.p)	
	}
}
#negative binomial
cqt.nb <- function(vec)	{
	tx <- table(vec)
	#concatenate from the 12th cell to the end such that observed freq > 10
	ttx <- c(tx[1:13] , sum(tx[14:length(tx)]))

	#use newton's method in optim to estimate min.chisquare lambda	
	ksq <- function(param)	{
		ep <- dnbinom(0:12, size = param[1], prob = param[2])
		eep <- c(ep, 1 - sum(ep))
		ei <- sum(ttx)*eep
		ts <- sum((ei - ttx)^2 / ei)
		return
		{
			ts
		}
	}
	initial.p <- c(3,0.2)
	minchi <- optim(par = initial.p , fn = ksq)
	final.p <- minchi$par   #minimum chisquare estimate 
	test <- minchi$value	#minimum chi square test statistics
	pvalue <- 1 - pchisq(test, df = length(ttx) - 2)   #the p -value
	cat("min chi square test statistics", test, "\n")
	cat("pvalue = " ,pvalue , "\n")
	cat("final negative binomial ,", final.p, "\n")
	return
	{
		list(final.p)	
	}
}


#poisson mixture pmf
pois.mix <- function(s, pi, lam)	{
	pmf <- 0
	for(i in 1:length(pi))	{
		pmf <- pmf +   pi[i] *dpois(s,lambda = lam[i])
	}
	return
	{
		pmf
	}	
}
#poisson mixture CDF
ppois.mix <- function(s, pi, lam)	{
	return
	{
		pi[1] * ppois(s,lambda = lam[1]) + pi[2] * ppois(s, lambda = lam[2])
	}
}
#geometric mixture CDF
ppois.geo.mix <- function(s,pi,lam,p)	{
	return
	{
		pi[1] * ppois(s,lambda = lam) + pi[2] * pgeom(s, prob = p)
	}
}
#poisson + geometric mixture distribution
pois.geom.mix <- function(s,pi, lam, p)	{
	return
	{
		pi[1] * dpois(s, lambda = lam) + pi[2] * dgeom(s, prob = p)
	}
}




#poisson + geometric distribution
em.algo.pois.geo <- function(d,lam, p ,phi)	{
	counter <- 1
	#initialisation
	w.iter <- matrix(0, length(d), 2)
	lam.iter <- 0
	phi.iter <- c(0,0)
	p.iter <- 0
	#E-step
	for(i in 1:length(d))	{
		denom <- 0
		w.iter[i,1] <- phi[1] * dpois(d[i], lambda = lam)
		w.iter[i,2] <- phi[2] * dgeom(d[i], prob = p)
		denom <- w.iter[i,1] + w.iter[i,2]
		w.iter[i,] <- w.iter[i,] / denom
	}
	#M-step
	lam.iter <- sum(d * w.iter[,1]) / sum(w.iter[,1])
	p.iter <- sum(w.iter[,2]) / (sum(w.iter[,2]) + sum(w.iter[,2] * d))
	phi.iter[1] <- sum(w.iter[,1]) /length(d)
	phi.iter[2] <- sum(w.iter[,2]) / length(d)
	while( all(abs(phi.iter - phi) + abs(lam.iter - lam) + abs(p.iter - p) > 0.0001) == T)	{
		counter <- counter + 1
		lam <- lam.iter
		phi <- phi.iter
		p <- p.iter
		#E-step
		for(i in 1:length(d))	{
			denom <- 0
			w.iter[i,1] <- phi[1] * dpois(d[i], lambda = lam)
			w.iter[i,2] <- phi[2] * dgeom(d[i], prob = p)
			denom <- w.iter[i,1] + w.iter[i,2]
			w.iter[i,] <- w.iter[i,] / denom
		}
		#M-step
		lam.iter <- sum(d * w.iter[,1]) / sum(w.iter[,1])
		p.iter <- sum(w.iter[,2]) / (sum(w.iter[,2]) + sum(w.iter[,2] * d))
		phi.iter[1] <- sum(w.iter[,1]) /length(d)
		phi.iter[2] <- sum(w.iter[,2]) / length(d)
	}
	cat("iterations: ",counter, "\n")
    cat("lambda: ", lam.iter,"\n")
    cat("p ", p.iter, "\n")
    cat("phi", phi.iter, "\n")
	return
	{
		list(counter,lam.iter, p.iter, phi.iter)		
	}
}

#poisson EM
em.algo.pois <- function(k,d, lam, phi)	{
	counter <- 1
	#initialisation
	w.iter <- matrix(0,length(d),k)
	lam.iter <-c()
	phi.iter <- c()
	#E - step
	# w ^ (i) _ j
	for(i in 1:length(d)) {
			denom <- 0
			for(j in 1:k)   {
			w.iter[i,j] <- phi[j]*dpois(d[i], lambda = lam[j])
			denom <- denom + phi[j]*dpois(d[i], lambda = lam[j]) 
		}	
		w.iter[i,] <- w.iter[i,] / denom
	}
	#M-step
	# lambda_j & phi_j

	for(j in 1:k)
	{
		lam.iter[j] <- sum(d*w.iter[,j]) / sum(w.iter[,j]) 
		phi.iter[j] <- sum(w.iter[,j])/length(d)
	}
	#browser()
	while(all(abs(lam.iter - lam) + abs(phi.iter - phi) > 0.00001 ) == T)	{
		counter <- counter + 1
		lam <- lam.iter
		phi <- phi.iter
		for(i in 1:length(d)) {
			denom <- 0
			for(j in 1:k)   {
				w.iter[i,j] <- phi[j]*dpois(d[i], lambda = lam[j])
				denom <- denom + phi[j]*dpois(d[i], lambda = lam[j])
			}	
			w.iter[i,] <- w.iter[i,] / denom
		}
		#M-step
		# lambda_j & phi_j
		for(j in 1:k)	{
			lam.iter[j] <- sum(d*w.iter[,j]) / sum(w.iter[,j]) 
			phi.iter[j] <- sum(w.iter[,j])/length(d)
		}		
	}
	cat("iterations: ",counter, "\n")
    cat("lambda: ", lam.iter,"\n")
    cat("phi", phi.iter, "\n")
	return
	{
		list(counter, lam.iter, phi.iter)
	}
}


##################
##	unuseful EM	##
##################
#negative binomial EM
em.algo.nb <- function(k,d, r,p, phi, steps)	{
	counter <- 1
	#initialisation 
	w.iter <- matrix(0, length(d), k)
	r.iter <- c()
	p.iter <- c()
	phi.iter <- c()
	
	#E-step
	for(i in 1:length(d))	{
		denom <- 0
		for(j in 1:k)	{
			w.iter[i,j] <- phi[j] * dnbinom(d[i], size = r[j], prob = p[j])
			denom <- denom +  phi[j] * dnbinom(d[i], size = r[j], prob = p[j])
		}
		w.iter[i,] <- w.iter[i,] / denom
	}
	#M-step
	for(j in 1:k)	{
		p.iter[j] <- r[j] * sum(w.iter[,j]) / (r[j] * sum(w.iter[,j]) + sum(w.iter[,j] * d))
		phi.iter[j] <- sum(w.iter[,j]) / length(d)
		r.iter[j] <- r[j] + ( steps/length(d) ) * sum(w.iter[,j] * ( (digamma(d + r[j])) - digamma(r[j]) + log(p[j]) ) )
	}
	if(all(r.iter > r) == T)
	{
		print("Bigger")
	} else {
		print("Smaller")
	}
	
	while(all(abs(p.iter - p) + abs(phi.iter - phi) + abs(r.iter - r) > 0.00001) == T)	{
		counter <- counter + 1
		p <- p.iter
		phi <- phi.iter
		r <- r.iter
			#E-step
		for(i in 1:length(d))	{
			denom <- 0
			for(j in 1:k)	{
				w.iter[i,j] <- phi[j] * dnbinom(d[i], size = r[j], prob = p[j])
				denom <- denom +  phi[j] * dnbinom(d[i], size = r[j], prob = p[j])
			}
			w.iter[i,] <- w.iter[i,] / denom
		}
		#M-step
		for(j in 1:k)	{
			p.iter[j] <- r[j] * sum(w.iter[,j]) / (r[j] * sum(w.iter[,j]) + sum(w.iter[,j] * d))
			phi.iter[j] <- sum(w.iter[,j]) / length(d)
			r.iter[j] <- r[j] + ( steps/length(d) ) * sum(w.iter[,j] * ( (digamma(d + r[j])) - digamma(r[j]) + log(p[j]) ) )
		}
	if(all(r.iter > r) == T)
	{
		print("Bigger")
	} else {
		print("Smaller")
	}
		
	}
	cat("iterations: ",counter, "\n")
    cat("r: ", r.iter,"\n")
    cat("p: ", p.iter, "\n")
    cat("phi", phi.iter, "\n")
    return
    {
    	list(counter, r.iter, p.iter, phi.iter)
    }
}
#geometric EM
em.algo.geom <- function(k,d, p, phi)	{
	counter <- 1
	w.iter <- matrix(0, length(d) ,k)
	p.iter <- c()
	phi.iter <- c()
	#E-step
	for(i in 1:length(d))	{
		denom <- 0
		for(j in 1:k)	{
			w.iter[i,j] <- phi[j] *dgeom(d[i], prob = p[j])
			denom <- denom + phi[j] *dgeom(d[i], prob = p[j])
		}
		w.iter[i,] <- w.iter[i,] / denom
	}
	
	for(j in 1:k)	{
		p.iter[j] <- sum(w.iter[,j]) / (sum(w.iter[,j] * d) + sum(w.iter[,j]) )
		phi.iter[j] <- sum(w.iter[,j]) / length(d) 
	}
	while(all(abs(phi.iter - phi) + abs(p.iter - p) > 0.0001) == T)	{
		counter <- counter + 1
		p <- p.iter
		phi <- phi.iter
		for(i in 1:length(d))	{
			denom <- 0
			for(j in 1:k)	{
				w.iter[i,j] <- phi[j] *dgeom(d[i], prob = p[j])
				denom <- denom + phi[j] *dgeom(d[i], prob = p[j])
			}
			w.iter[i,] <- w.iter[i,] / denom
		}
	
		for(j in 1:k)	{
			p.iter[j] <- sum(w.iter[,j]) / (sum(w.iter[,j] * d) + sum(w.iter[,j]) )
			phi.iter[j] <- sum(w.iter[,j]) / length(d) 
		}
	}
	cat("iterations: ",counter, "\n")
    cat("p: ", p.iter,"\n")
    cat("phi", phi.iter, "\n")
	return
	{
		list(counter, p.iter, phi.iter)
	}
}
#Negative binomial mixture pmf
nb.mix <- function(s,pi, r, p )	{
	pmf <- 0 
	for(i in 1:length(pi))	{
		pmf <- pmf + pi[i] * dnbinom(s, size = r[i], prob = p[i])
	}
	return
	{
		pmf
	}
}
#Geometric mixture pmf
geom.mix <- function(s,pi,p)	{
	pmf <- 0 
	for(i in 1:length(pi))	{
		pmf <- pmf + pi[i] * dgeom(s, prob = p[i])
	}
	return
	{
		pmf
	}
}

