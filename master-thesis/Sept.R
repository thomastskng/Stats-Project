#August work
# focus on OUTGOING connections
######################################################################
###MONTH College IPS data


library(dgof)
library(mixtools)
library(xtable)
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
src_ips_counts <- read.table("~/Google Drive/Imperial College/ICproject/March/src_ips_counts.txt", quote="\"")

#convert counts to vector and verify they are vectors
source_counts <- as.vector(src_ips_counts[,1])
is.vector(source_counts)

#DESTINATION IP COUNTS
dst_ips_counts <- read.table("~/Google Drive/Imperial College/ICproject/March/dst_ips_counts.txt", quote="\"")
dest_counts <- as.vector(dst_ips_counts[,1])
is.vector(dest_counts)

#EDGE COUNTS
edge_counts <- read.table("~/Google Drive/Imperial College/ICproject/March/src_dst_ips_counts.txt", quote="\"")
edge_counts <- as.vector(edge_counts[,1])
is.vector(edge_counts)

library(igraph)
#Edges 
src_dst_ips <- read.table("~/Google Drive/Imperial College/ICproject/March/src_dst_ips.txt", header = F, colClasses =c("character", "character"))

col1 <- as.vector(src_dst_ips[,1])
col2 <- as.vector(src_dst_ips[,2])
#become matrix
src_dst_ips <- cbind(col1,col2)
edges <- cbind(col1,col2)

#indegree IS A COUNT
indegree <- read.table("~/Google Drive/Imperial College/ICproject/March/dst_ips_degrees.txt", quote="\"")
indegree <- as.vector(indegree[,1])
is.vector(indegree)

#all IP address that ever appear as a src + dst
all_ips <- read.table("~/Google Drive/Imperial College/ICproject/March/ips.txt", header = F, colClasses =c("character"))
all_ips <- as.vector(all_ips[,1])

all_ips_counts <- read.table("~/Google Drive/Imperial College/ICproject/March/ips_counts.txt", quote="\"")
all_ips_counts <- as.vector(all_ips_counts[,1])

y <- order(all_ips_counts, decreasing = T)[1:5]
all_ips[y]

#outdegree IS A COUNT
outdegree <- read.table("~/Google Drive/Imperial College/ICproject/March/src_ips_degrees.txt", quote="\"")
outdegree <- as.vector(outdegree[,1])
is.vector(outdegree)
###########################################################################

top5 <- c("0001694321290_.txt",
"0001694345781_.txt",
"0001694360912_.txt",
"0001823016148_.txt",
"0001823027854_.txt")

top <- c("0001694304020_.txt",
"0001694306440_.txt",
"0001694332817_.txt",
"0001694345396_.txt",
"0001823015992_.txt",
"0001823016560_.txt",
"0001823018562_.txt")

m1 <- dir(path = "~/Google Drive/Imperial College/ICproject/august/median/m1", pattern = "_.txt")
m2 <- dir(path = "~/Google Drive/Imperial College/ICproject/august/median/m2", pattern = "_.txt")
m3 <- dir(path = "~/Google Drive/Imperial College/ICproject/august/median/m3", pattern = "_.txt")
m4 <- dir(path = "~/Google Drive/Imperial College/ICproject/august/median/m4", pattern = "_.txt")
m5 <- dir(path = "~/Google Drive/Imperial College/ICproject/august/median/m5", pattern = "_.txt")
m6 <- dir(path = "~/Google Drive/Imperial College/ICproject/august/median/m6", pattern = "_.txt")


t1313 <- dir(path ="~/Google Drive/Imperial College/ICproject/august/1313/", pattern = "_.txt")
t1717 <- dir(path ="~/Google Drive/Imperial College/ICproject/august/1717/", pattern = "_.txt")
t2323 <- dir(path ="~/Google Drive/Imperial College/ICproject/august/2323/", pattern = "_.txt")
t3333 <- dir(path ="~/Google Drive/Imperial College/ICproject/august/3333/", pattern = "_.txt")
t5555 <- dir(path ="~/Google Drive/Imperial College/ICproject/august/5555/", pattern = "_.txt")
t6000 <- dir(path ="~/Google Drive/Imperial College/ICproject/august/6000/", pattern = "_.txt")
t7000 <- dir(path ="~/Google Drive/Imperial College/ICproject/august/7000/", pattern = "_.txt")

t7500 <- dir(path ="~/Google Drive/Imperial College/ICproject/august/7500/", pattern = "_.txt")
t9000 <- dir(path ="~/Google Drive/Imperial College/ICproject/august/9000/", pattern = "_.txt")
t9591 <- dir(path ="~/Google Drive/Imperial College/ICproject/august/9591/", pattern = "_.txt")

#verify count data
verify <- function(l,string)	{
	node.in <- read.table(paste("~/Google Drive/Imperial College/ICproject/august/",l,"/", string, sep = ""), quote="\"")
	node.in <- as.vector(node.in[,1])
	#sum all non-zero counts in March = all counts in March
	a <- sum(node.in[node.in != 0])
	
	y <- unlist(strsplit(string, "[:_.txt:]"))
	y <- y[grep("[0-9]",y)]
	#m <- match(y, all_ips)
  	m <- which(all_ips == y)
	b <- source_counts[m]
	cat("IP",y,"\n")

	if(a == b)	{
		print("Correct")
	} else {
		cat("INcorrect","\n")
	}
	cat("outgoing = ",a, "\n")
	cat("src_counts = ", b, "\n")
    cat("match position = ", m, "\n")

}

#l (depends where u put your data)
#string : file name

get.node <- function(l,string) {
	node.in <- read.table(paste("~/Google Drive/Imperial College/ICproject/august/",l,"/", string, sep = ""), quote="\"")
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

###################################################################
divide.time.term <- function(k,d)	{
	
	#chopping up time 
	nt = c()
	wkend = c()
	wkday = c()
	
	nt_later = c()
	wkday_later = c()
	wkend_later = c()
	
	day <- 5
	for(i in 0:20) {
		if(i <= k)		{
			j <- i*1440
			nt <- append(nt, d[(j+1):(j + 480)])
			j <- j + 480
			if(day %% 6 == 0 || day %% 7 == 0) {
				wkend <- append(wkend, d[(j+1):(j + 720)])		
			} else {
				wkday <- append(wkday, d[(j+1):(j + 720)])		
			}
			j <- j + 720 
			nt <- append(nt, d[(j+1):(j + 240)])
			if(day == 7) {
				day <- 1
			} else {
				day <- day + 1
			}
		} else if(i > k)	{
			j <- i*1440
			nt_later <- append(nt_later, d[(j+1):(j + 480)])
			j <- j + 480
			if(day %% 6 == 0 || day %% 7 == 0) {
				wkend_later <- append(wkend_later, d[(j+1):(j + 720)])		
			} else {
				wkday_later <- append(wkday_later, d[(j+1):(j + 720)])		
			}
			j <- j + 720 
			nt_later <- append(nt_later, d[(j+1):(j + 240)])
			if(day == 7) {
				day <- 1
			} else {
				day <- day + 1
			}							
		}
	}
	return
	{
		list(nt, wkday, wkend , nt_later, wkday_later, wkend_later)
	}
}


###MARCH ONLY
#nighttime   08:00pm - 08:00am	
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
#function to chop up the time into Night time / weekday daytime / weekend daytime
divide.time <- function(k,d)	{
	
	#chopping up time 
	nt = c()
	wkend = c()
	wkday = c()
	
	nt_later = c()
	wkday_later = c()
	wkend_later = c()
	
	day <- 5
	for(i in 0:29) {
		if(i <= k)		{
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
		} else if(i > k)	{
			j <- i*1440
			nt_later <- append(nt_later, d[(j+1):(j + 480)])
			j <- j + 480
			if(day %% 6 == 0 || day %% 7 == 0) {
				wkend_later <- append(wkend_later, d[(j+1):(j + 840)])		
			} else {
				wkday_later <- append(wkday_later, d[(j+1):(j + 840)])		
			}
			j <- j + 840
			nt_later <- append(nt_later, d[(j+1):(j + 120)])
			if(day == 7) {
				day <- 1
			} else {
				day <- day + 1
			}							
		}
	}

		#Change to summer time in March	
		j <- 30*1440
		nt_later <- append(nt_later, d[(j+1): (j + 420)])
		j <- j + 420
		if(day %% 6 == 0 || day %% 7 == 0) {
			wkend_later <- append(wkend_later, d[(j+1):(j + 840)])		
		} else {
			wkday_later <- append(wkday_later, d[(j+1):(j + 840)])		
		}
		j <- j + 840 
		nt_later <- append(nt_later, d[(j+1):(j+120)])

		return
		{
			list(nt, wkday, wkend , nt_later, wkday_later, wkend_later)
		}
}

#APRIL divide time
divide.time.April <- function(d)	{
	#2 vectors because only 1st & 2nd April = monday + tuesday
	nt <- c()
	wkday <- c()
		j <- 0
		nt <- append(nt, d[(j+1):(j + 480)])
		j <- j + 480
		wkday <- append(wkday, d[(j+1):(j + 840)])		
		j <- j + 840
		nt <- append(nt, d[(j+1):(j + 600)])
		j <- j + 600 
		wkday <- append(wkday, d[(j+1):(j+287)])
	return
	{
		list(nt,wkday)
	}
}

#Feb divide time : 26th Feb 17:11 - 28th
divide.time.Feb <- function(d)	{
	nt <- c()
	wkday <- c()
	#tuesday
	j <- 0
	wkday <- append(wkday, d[(j+1) : (j + 120+169)])
	j <- j + 169 + 120
	nt <- append(nt, d[(j+1) : (j+600)])
	j <- j + 600
	#wed
	wkday <- append(wkday, d[(j+1) : (j + 840)])
	j <- j + 840
	nt <- append(nt, d[(j+1):(j+600)])
	
	#thurs
	j <- j + 600
	wkday <- append(wkday, d[(j+1):(j+840)])
	j <- j + 840
	nt <- append(nt, d[(j+1):(j+120)])
	return
	{
		list(nt,wkday)
	}
}


#########################################################

#function to create 3 plots for different time vectors
#  v =  March vector
#  t = training data / testing data
create.plot <- function(v, tt, string)	{

	#plot(v[v != 0],type = "p",cex = 0.5)
	#hist(v[v != 0 ], freq = F,nclass = 20)
   	#computing training p values
    list[training.p] <- p.value(v[v != 0] , v[v != 0],string)
    train <- print(ks.test(training.p,"punif"))

	#computing testing p values	    
    list[testing.p] <- p.value(v[v != 0] , tt[tt != 0],string)
    test <- print(ks.test(testing.p,"punif"))
    
    ###############################    
    cat("\t\t\t\t\t\t\t\t\t\t\t\t",string, "\n")
    #cat( length(which(duplicated(p) == T)),"duplicates in length",length(p), "\n")
    #dev.off()
    list[trace_pos, positions] <- ewma(testing.p, 0.25,string)
   	#browser()
	#########################################      <-     ANSWER
	answer <- tt[tt!=0][trace_pos]
	dev.new()
	#pdf(file = "weekdaydaytime.pdf", width = 11.69 , height = 8.27)
	par(mfrow = c(2,3))
	#training
	#plot pmf			
    plot(table(v[v != 0 ])/ length(v[v != 0 ]), type = "h", xlab = "x (active counts)", ylab = "P(X=x)", main = paste("pmf ( ",string," )","\n", "( Training )",sep = "" ))
    #plot empirical cdf
   	plot(ecdf(v[v != 0]), ylab = "F(x) = P(X <= x)", xlab = "x", main = paste("Empirical CDF ( ",string," )","\n", "( Training )",sep = "" ),verticals = T, do.points = T)
	#draw training. pvalues
    hist(training.p, freq = F, main = paste("corrected p-values ( ",string," )","\n", "( Training )", sep = "" ), xlab = "p-values", nclass = 20, cex = 1.5 )
    legend("topright", legend = paste("K-S test p-value: ", round(train$p.value,digits = 2) ,sep ="" ) , cex = 1.5, bty = "n", text.col = "red")
#testing
	plot(table(tt[tt != 0 ])/ length(tt[tt != 0 ]), type = "h", xlab = "x (active counts)", ylab = "P(X=x)", main = paste("pmf ( ",string," )","\n", "( Testing )",sep = "" ))
	ttx <- table(tt[tt != 0 ])
	points(x = answer, y = ttx[as.character(answer)] / sum(ttx), pch = 1, cex = 3, col = "red")
	plot(ecdf(tt[tt != 0]), ylab = "F(x) = P(X <= x)", xlab = "x", main = paste("Empirical CDF ( ",string," )","\n", "( Testing )",sep = "" ),verticals = T, do.points = T)
    hist(testing.p, freq = F, main = paste("corrected p-values ( ",string," )","\n", "( Testing )", sep = "" ), xlab = "p-values", nclass = 20, cex = 1.5 )
    legend("topright", legend = paste("K-S test p-value: ", round(test$p.value,digits = 2) ,sep ="" ) , cex = 1.5, bty = "n", text.col = "red")
   	#dev.off()

    return
    {
    	#list(which(tt == tt[tt != 0][trace_pos]))
    	list(tt[tt!=0][trace_pos])
    }
}

#pass non-zero count vector + v2= observed vector
p.value <- function(v1,v2, string) {
	pvalues <- c()
    #vector of p values
    Fn <- ecdf(v1)
    #browser()
    for(i in 1:length(v2))  {
        observed <- v2[i]
     	#s_x
        s_x <- 1 - Fn(observed - 1)
        #s_x+1
        s_x1 <- 1 - Fn(observed)
        corrected.p <- runif(1, min = s_x1 , max = s_x)
        pvalues <- append(pvalues, corrected.p)
    }
    return
    {
    	list((pvalues))
    }
}

ewma <- function(pvec,w,string)	{
	n <- length(pvec)
	ch <- 1-pvec
	
	ch[ch == 0] <- ch[ch == 0] + 1e-323
	ch[ch == 1] <- ch[ch == 1] - 1e-16
	
	#z-score method
	z <- qnorm( ch , mean = 0 ,sd = 1) 
	#compute s_n
	s.vec = c()
	s_n <- 0 
	for(i in 1:length(z))	{
		s_n <- (1 - w) * s_n + w * z[i] 
		s.vec <- append(s.vec, s_n)
	}
	
	m <- 3.49* sqrt( (w/(2-w)) * (1-(1-w)^(2*seq(1,n,1))) )
	ucl <- m
	lcl <- -m
	dev.new()
	#plot(x = seq(1,length(z),1) , y = s.vec , type = "l",xlab = "time n", ylab = "EWMA values")
	#pdf(file = "EWMA.pdf", width = 11.69 , height = 8.27)
	matplot(x = seq(1,length(z),1), y = cbind(ucl,s.vec), lty = c(2,1), lwd = c(1.5,0.5), type = "l", xlab = "active minutes (n)", ylab = expression(S[n]), main = paste("EWMA Control Chart ( ", string," )",sep = ""), col = c("red", "black"))
	pos <- c()
	for(i in 1:length(s.vec))	{
		if(s.vec[i] > ucl[i])	{
			pos <- append(pos,i)
		}
	}
	points(x = which.max(s.vec), y = s.vec[which.max(s.vec)], col = "blue" ,pch = 1 ,cex = 5)
	cat("BackTrack (EWMA): position" , pos,"   value: ",s.vec[pos], "\n")
	#dev.off()
	#browser()
	return
	{
		list(which.max(s.vec), pos	)
	}
}

#Feb + March
seasonality <- function(string,string2, input)	{	
	naming <- to.dotted.quad(string2)
	d_mar <- get.node(string, string2)
	d_feb <- get.node(paste("Feb/",string,sep = ""),string2)
	d_april <- get.node(paste("April/",string,sep = ""),string2)
	d <- c(d_feb , d_mar[1:37440])
	d <- d[50:length(d)]
	#browser()
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
		shift <- i + 1080 + 24*60*4
		wrap <- rep(shift %% l, d[i])
		testing <- append(testing, wrap)
	}		

	tt <- table(factor(testing, levels = seq(1,l,1))) 
	if(input == 1)	{
		#day
		plot(tt / sum(tt), type = "l", xaxt = "n", xlab = "Hours in a day ", main = "Density Plot of IP count by Day (Training Period)", ylab = "density")
		axis(1, at = c(seq(1,l,120),l), labels = c("12:00am", seq(2,22,2),"12:00am"))
		legend("topright", legend = naming, cex = 1,bty = "n")
	} else	{
		#week
		plot(tt / sum(tt), type = "l", xaxt = "n", xlab = "Days in a week", main = "Density Plot of IP count by Week (Training Period)",ylab = "density")
		axis(1, at = c(seq(1,l,l / 7), l), labels = c("Fri 12am","Sat", "Sun","Mon" ,"Tues","Wed", "Thurs", "Fri 12am"))
		legend("topright", legend = naming, cex = 1,bty = "n")
	}
}

#March + April SPLIT
seasonality_split <- function(string,string2, input,trace_pos)	{	
	naming <- to.dotted.quad(string2)
	d_mar <- get.node(string, string2)
	d_april <- get.node(paste("April/",string,sep = ""),string2)
	d <- c(d_mar[37441:length(d_mar)] , d_april)
	
	
	trace_pos <- which(d == trace_pos)
	#day <- trace%/%840
	#cat("Happen on :", day , "day since 27th March", "\n")
	#trace <- ( (day)* 1440 )+ 480 + trace%% 840
	
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
		
		wrap <- rep(i %% l, d[i])
		testing <- append(testing, wrap)
	}		
	trace_pos <- trace_pos %% l
	#becos of factor, they become numbers, no need convert back to as.character
	tt <- table(factor(testing, levels = seq(1,l,1))) 
	if(input == 1)	{
		#day
		plot(tt / sum(tt), type = "l", xaxt = "n", xlab = "Hours in a day ", main = "Density Plot of IP count by Day (Testing Period)", ylab = "density")
		points(x = trace_pos, y = tt[as.character(trace_pos)]/sum(tt), pch = 1, cex = 3, col = "red")
		axis(1, at = c(seq(1,l,120),l), labels = c("12am", seq(2,22,2), "12am"))
		legend("topright", legend = naming, cex = 1,bty = "n")

	} else	{
		#week
		plot(tt / sum(tt), type = "l", xaxt = "n", xlab = "Days in a week", main = "Density Plot of IP count by Week (Testing Period)",ylab = "density")
		points(x = trace_pos, y = tt[trace_pos]/sum(tt), pch = 1, cex = 3, col = "red")
		axis(1, at = c(seq(1,l,l / 7), l), labels = c(paste("Wed 12am","\n", "(27th Mar)", sep = ""),"Thurs" ,"Fri","Sat", "Sun", "Mon", "Tues", paste("Wed 12am","\n", "(3rd April)", sep = "")))
		legend("topright", legend = naming, cex = 1,bty = "n")
	}
}

to.dotted.quad <- function(v)	{
	vec <- c()
	int <- as.integer( unlist(strsplit(v, "[:_.txt:]"))[1] )
	
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



###MAIN METHOD BELOW

#############################
#markov model ###############
#############################
	#nighttime   10:00pm - 08:00am	
	#daytime		 08:00am - 08:00pm 
processing <- function(string,string2,k){

	d <- get.node(string, string2)
################
###	OPTION 2  ##
################
#WEEK VS WEEK	
	#list[m,day31] <- divide.day(d)
	#list[nt1] <- get.nighttime(m)
	#list[wkday1] <- get.wkday(m)
	
	#nt <- nt1[1,]
	#nt_test <- nt1[2,]
	#wkday <- wkday1[1,]
	#wkday_test <- wkday1[2,]
		
################
###	OPTION 1  ##
################
#FEB + MARCH VS APRIL
	#testing data set
	d1 <- get.node(paste("April/",string,sep = ""),string2)
	d2 <- get.node(paste("Feb/",string,sep = ""),string2)
	#chopping up time
	list[nt_mar, wkday_mar, wkend_mar, nt_split, wkday_split, wkend_split] <- divide.time(k,d)
	list[nt_april, wkday_april] <- divide.time.April(d1)
	list[nt_feb, wkday_feb] <- divide.time.Feb(d2)

	nt <- c(nt_feb, nt_mar)
	wkday <- c(wkday_feb, wkday_mar)
	wkend <- c(wkend_mar)
	nt_test <- c(nt_split, nt_april)
	wkday_test <- c(wkday_split, wkday_april)
	#wkend_test <- wkend_split
	#browser()
	#########################
	###   NIGHT TIME   ######
	#########################
		
	#jpeg(filename = "nt3.jpeg",  height = 600, width = 900)	
	if( (length(nt[nt != 0 ]) != 0)	&& (length(nt_test[nt_test != 0 ]) != 0)){
		#testing data
		#list[trace,others] <- create.plot(nt, nt_test , "Night Time")
		#cat("CAUTION:", nt_test[trace], "\n")
	}
	#dev.off()
	
	##############################
	###   WEEKDAY DAYTIME   ######
	##############################
	# jpeg(filename = "wkday3.jpeg",  height = 600, width = 900)
	if( (length(wkday[wkday != 0]) != 0) && (length(wkday_test[wkday_test != 0]) != 0)) {
		#dev.new()
		#testing data
		list[trace, others] <- create.plot(wkday, wkday_test, "Weekday Daytime")
		#browser()
		cat("CAUTION:" ,wkday_test[trace], "\n")
		dev.new()
		#pdf(file = "testingbehave.pdf", width = 11.69 , height = 8.27)
		par(mfrow = c(2,1))
		seasonality_split(string,string2, 1, trace)
		seasonality_split(string,string2, 2, trace)
		#dev.off()
	}
	# dev.off()
	
	###############################
	###   WEEKEND DAYTIME  ########
	###############################	
	#jpeg(filename = "wkend3.jpeg",  height = 600, width = 900)
	#if( (length(wkend[wkend !=  0]) != 0)  && (length(wkend_test[wkend_test != 0]) != 0))	{
		#dev.new()
		# #training data
		#create.plot(wkend, wkend_test, "Weekend Daytime")
	#}	
		cat ("Night time", "\n")
		transit(nt)
		cat ("\n\n","Weekday", "\n")
		transit(wkday)
		cat("\n\n","Weekend", "\n")
		transit(wkend)
		
		verify(string, string2)

}




processing("top5", top5[3])		# 63015958
processing("top5", top5[4])  #weird		#47156382
processing("top5", top5[5])		#47073507

processing("top", top[1])		#18954456 


processing("median/m1", m1[1])		#38331
processing("median/m1", m1[4])		#5975
processing("median/m1", m1[7])		#85473
processing("median/m2", m2[6])		#251
processing("median/m2", m2[9])		#77
processing("median/m2", m2[11])		#1327
processing("median/m2", m2[13])		#66
processing("median/m2", m2[16])		#69

processing("median/m3", m3[12])		#142
processing("median/m3", m3[15])		#137


processing("median/m4", m4[4])		#504
processing("median/m4", m4[5])		#41207
processing("median/m4", m4[13])		#3920

processing("median/m5", m5[3])		#1802
processing("median/m5", m5[9])		#695
processing("median/m5", m5[7])		#1229
processing("median/m5", m5[14])		#2197
processing("median/m5", m5[18])		#694


########################################################
########################################################
													####
													#####	
processing("median/m6", m6[15],25)		#39204		#####	#show
processing("median/m6", m6[12],25)		#45743		#####	#not show
processing("median/m6", m6[10],25)		#54985 		######	#no anomaly			SHOW FALSE ALARM			#IP3
													#####
													####
processing("1717", t1717[5],25)		#dont show		####	nothing in march #but sth in april + need to adjust pvalues > 0.05
processing("5555", t5555[18],25)					####		#show definitely  IP2
processing("6000", t6000[20],25)					####	#dont show 
processing("7000", t7000[5],25)						#####		SHOW				$IP4
processing("7500", t7500[7],25)						####		SHOW definitely		#IP1
processing("7500", t7500[15],25)					####		#concentrate on March #nothing in april
processing("9000", t9000[13],25)					####		#very low low activity
													####
#########################################################
#1313 none work
#2323 none work
#3333 none work
pdf(file = "IP4trainingbehave.pdf", width = 11.69, height = 8.27)
par(mfrow = c(2,1))
seasonality("median/m6", m6[15],1)
seasonality("median/m6", m6[15],2)
dev.off()
par(mfrow = c(2,1))
seasonality("median/m6", m6[12],1)
seasonality("median/m6", m6[12],2)
pdf(file = "IP3trainingbehave.pdf", width = 11.69, height = 8.27)
par(mfrow = c(2,1))
seasonality("median/m6", m6[10],1)
seasonality("median/m6", m6[10],2)
dev.off()
pdf(file = "IP2trainingbehave.pdf", width = 11.69, height = 8.27)
par(mfrow = c(2,1))
seasonality("5555", t5555[18],1)
seasonality("5555", t5555[18],2)
dev.off()
par(mfrow = c(2,1))
seasonality("6000", t6000[20],1)
seasonality("6000", t6000[20],2)
par(mfrow = c(2,1))
seasonality("1717", t1717[5],1)
seasonality("1717", t1717[5],2)
pdf(file = "IP4trainingbehave.pdf", width = 11.69, height = 8.27)
par(mfrow = c(2,1))
seasonality("7000", t7000[5],1)
seasonality("7000", t7000[5],2)
dev.off()
par(mfrow = c(2,1))
seasonality("7000", t7000[15],1)
seasonality("7000", t7000[15],2)
pdf(file = "IP1trainingbehave.pdf", width = 11.69, height = 8.27)
par(mfrow = c(2,1))
seasonality("7500", t7500[7],1)
seasonality("7500", t7500[7],2)
dev.off()
par(mfrow = c(2,1))
seasonality("7500", t7500[15],1)
seasonality("7500", t7500[15],2)
par(mfrow = c(2,1))
seasonality("9000", t9000[13],1)
seasonality("9000", t9000[13],2)


#Feb + March 26 VS April
#7500: 28#IP 0001694320178    35#IP 0001694347777	36#IP 0001694351306  37#IP 0001694361088	
#7500: almost working	#IP 0001694302847
#9000: 31			#IP 0001694333539


#Feb VS March
#7500: 1 , 7,16
#9000: 10,13,16, 
#9591: 7, 17



#NT problem : t7500[7] 
#wkday problem: t5555[1], t7500[7]
pdf(file = "nighttimeipONE.pdf", width = 11.69 , height = 8.27)
par(mfrow = c(2,3))
processing("7500", t7500[3],25)


pdf(file = "nighttimeipTWO.pdf", width = 11.69 , height = 8.27)
par(mfrow = c(2,3))
processing("5555", t5555[1],25)


pdf(file = "weekdaydaytimeipONE.pdf", width = 11.69 , height = 8.27)
par(mfrow = c(2,3))
processing("7500", t7500[7],24)


pdf(file = "weekdaydaytimeipTWO.pdf", width = 11.69 , height = 8.27)
par(mfrow = c(2,3))
processing("5555", t5555[1],25)

pdf(file = "nighttimeipTHREE.pdf", width = 11.69 , height = 8.27)
par(mfrow = c(2,3))
processing("7000", t7000[4],25)

pdf(file = "weekdaydaytimeipTHREE.pdf", width = 11.69 , height = 8.27)
par(mfrow = c(2,3))
processing("7000", t7000[21],25)





