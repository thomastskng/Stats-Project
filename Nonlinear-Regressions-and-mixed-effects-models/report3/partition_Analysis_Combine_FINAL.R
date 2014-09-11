#############################################
#											#
#	set working directory + import data		#
#											#
#############################################
library(nlme)
library(plotrix)
setwd("/Users/thomastskng/Google Drive/RehabProject/BreathometerCsv")
testpid <- read.csv(file = "testPID.csv", header = T)
bdata <- read.csv(file = "CalibrationData.csv", header = T)
remove.ind1 <- union(which(bdata$After.WarmUp == 0),which(bdata$Condition == "tp1"))
remove.ind <- union(remove.ind1 , which(bdata$Condition == "tp4"))
bdata <- bdata[-remove.ind,]				# remove AfterWarmUp = 0  and tp1 + tp4

source("partition_by_AfterWarmUp_DD.R")

#########################################################
#														#
#		Preprocessing - Partition 2 Data Matrix			#
#														#
#########################################################

# # newdf has dimension 10406 x 10
# 		input 1: PID from the vector "filtered_pid"
#		input 2: ordered data frame "newdf"

# pid <- filtered_pid[9]
# dataa <- newdf

# use AfterWarmUp value given from CSV (LHS + RHS different lengths ) (no adjustments)


#par(mfrow = c(1,2))
#IndiviPlot(filtered_pid[15], newdf)
#Indivi_Partition_Matrix(filtered_pid[9], newdf)
# 9,15,18,19,23,31,38,41,53,54,57,60,62,63,65,67
abc <- Indivi_Partition_Matrix(filtered_pid[9], newdf)
df1 <- abc[[1]]
df2 <- abc[[2]]


###############################################################################
# Exploratory Data Analysis
IndiviPlot(filtered_pid[7], newdf)

# list of Good Devices
# 9,7,18,19,23,31,38,41,53,54,57,60,62,63,65,67,216
# 11,12,25,30,39,40,46,47,368,297,168,132,199,243, 73, 241,281
abc <- Indivi_Partition_Matrix(filtered_pid[7], newdf)

ran <- function(){
	
	random <- sample(1:379,1)
	print(random)
	abc <- Indivi_Partition_Matrix(filtered_pid[random], newdf)
}

ran()

abc <- Indivi_Partition_Matrix(filtered_pid[7], newdf)
df1 <- abc[[1]]
df2 <- abc[[2]]

################################################################################

#########################################
#										#
# 	Non linear mixed effect model LHS	#
#										#
#########################################

# 	LHS dependent on dosage
meanfuncLHS <- function(d,tt, b0,b1,b2){
	d <- exp(d)
	b0 <- exp(b0)
	b1 <- exp(b1)
	b2 <- exp(b2)
	f1 <- d*(b0 + b1*exp(- b2*tt ))
	
	meangrad <- array(0, c(length(tt), 3), list(NULL, c("b0", "b1","b2")))
	meangrad[,"b0"] <- d * b0
	meangrad[,"b1"] <- d*exp(-b2*tt) * b1
	meangrad[,"b2"] <- -d * b1*tt*exp(-b2*tt) * b2
	attr(f1,"gradient") <- meangrad
	f1
}

lhs.nlme2 <- nlme(df1_measures ~ meanfuncLHS(df1_cp, decay_time, b0,b1,b2) , 
			fixed = list(b0 ~ 1, b1 ~df1_cp, b2 ~ 1),
			random = b0 + b1 + b2 ~ 1| df1_condition /df1_subject,
			#groups = ~ df1_subject ,
			data = df1,
			start = list(fixed = c(b0 = 2.349248, b1 = 5.466975,b2 = 0.271762, -0.103237)),
			verbose = T
			)
summary(lhs.nlme2)

################################################################################


#########################################################
#														#
#	independent on DOSAGE, Group by Condition /subject	#
#														#
#########################################################

meanfuncLHS <- function(d,tt, b0,b1,b2){
	#d <- exp(d)
	b0 <- exp(b0)
	b1 <- exp(b1)
	b2 <- exp(b2)
	#f1 <- d*(b0 + b1*exp(- b2*tt ))
	f1 <- (b0 + b1*exp(- b2*tt ))
	meangrad <- array(0, c(length(tt), 3), list(NULL, c("b0", "b1","b2")))
	#meangrad[,"b0"] <- d * b0
	#meangrad[,"b1"] <- d*exp(-b2*tt) * b1
	#meangrad[,"b2"] <- -d * b1*tt*exp(-b2*tt) * b2
	meangrad[,"b0"] <- b0
	meangrad[,"b1"] <- exp(-b2*tt) * b1
	meangrad[,"b2"] <- -b1*tt*exp(-b2*tt) * b2
	attr(f1,"gradient") <- meangrad
	f1	
}

lhs.nlme6 <- nlme(df1_measures ~ meanfuncLHS(df1_cp, decay_time, b0,b1,b2) , 
			fixed = list(b0 ~ 1, b1 ~ 1, b2 ~ 1),
			random = b0 + b1 + b2 ~ 1| df1_condition /df1_subject,
			#groups = ~ df1_subject ,
			data = df1,
			#start = list(fixed = c(b0 = 1.98, 0,b1 = 5.5,b2 = -0.25,0.5)),
			start = list(fixed = c(b0 = 1.98,b1 = 5.5,b2 = 0.25)),  # device 7
			verbose = T
			)
summary(lhs.nlme6)




lhs.nlme7 <- nlme(df1_measures ~ meanfuncLHS(df1_cp, decay_time, b0,b1,b2) , 
			fixed = list(b0 ~ df1_cp, b1 ~ 1, b2 ~ df1_cp),
			random = b0 + b1 + b2 ~ 1| df1_condition /df1_subject,
			#groups = ~ df1_subject ,
			data = df1,
			#start = list(fixed = c(b0 = 1.98, 0,b1 = 5.5,b2 = -0.25,0.5)),
			start = list(fixed = c(b0 = 1.98, 0,b1 = 5.5,b2 = 0.25,0)),  # device 7
			verbose = T
			)
summary(lhs.nlme7)


################################################################################
#########################################
#										#
# 	Non linear mixed effect model RHS	#
#										#
#########################################
meanfuncRHStest <- function(d,tt,b0, b1,b2){
	
	d <- exp(d)
	b0 <- exp(b0)
	b1 <- exp(b1)
	b2 <- exp(b2)
	f1 <-  d*( b0 - b1*exp( -(b2)*( tt-20)) )
	
	meangrad <- array(0, c(length(tt), 3), list(NULL,c("b0", "b1", "b2")))
	meangrad[,"b0"] <- d * b0
	meangrad[,"b1"] <- ( - exp( -(b2)*( tt-20) )) * d * b1
	meangrad[,"b2"] <- (b1)*(tt-20) * exp( -(b2)*(tt-20) ) * d * b2
	attr(f1, "gradient") <- meangrad
	 f1
}

# Case 8: b0~ df2_cp, b1 ~df2_cp, b2~ df2_cp # GOOD
rhs.nlmetest8 <- nlme(df2_measures ~ meanfuncRHStest(df2_cp, blow_time,b0, b1,b2),
				fixed = list(b0~ df2_cp, b1 ~df2_cp, b2~ df2_cp),
				random = b0 + b1 + b2 ~ 1 | df2_condition /df2_subject,	
				#random = list(b0~1,b1 ~ 1, b2~ 1),
				#groups = ~df2_subject,
				#groups = ~df2_condition,
				data = df2,
				start = list(fixed = c(b0 = 3.979,0.0467, b1 = 3.618,0.77,b2 = 0.73,-0.1)),
				verbose = T)

summary(rhs.nlmetest8)


#################################################################################


drawCombineALL <- function(){
	
	model_L <- lhs.nlme7
	model_R <- rhs.nlmetest8
	# color vector
	col_vec <- c("black", "red", "green", "blue")
	names(col_vec) <- c("cp1", "cp2", "cp3", "cp4")

	for(i in unique(df1$df1_subject)){
		# LHS
		ind_L = df1$df1_subject == i
		x_L <- df1$decay_time[ind_L]
		y_L <- df1$df1_measures[ind_L]
		#		fitted values
		fv_L <- fitted(model_L, level = 2)[ind_L]
		fv_lv1_L <-fitted(model_L, level = 1)[ind_L]
		
		print(i)
		# RHS
		ind_R = df2$df2_subject == i
		x_R <- df2$blow_time[ind_R]
		y_R <- df2$df2_measures[ind_R]
		fv_R <- fitted(model_R, level = 2)[ind_R]
		fv_lv1_R <- fitted(model_R, level = 1)[ind_R]
		
		# COMBINE
		x <- c(x_L, x_R)
		y <- c(y_L, y_R)
		fv <- c(fv_L, fv_R)
		fv_lv1 <- c(fv_lv1_L, fv_lv1_R)
		
		#col_index <- strsplit( unique(names(fv)), "/" )[[1]][1]
		col_index <- unique(df1$df1_condition[ind_L])
		col_vec_plot <- col_vec[col_index]
		
		if(i == 1){
			plot(x,y, type = "l", col = col_vec_plot, lwd = 0.3, xlab = "time (secs)" , ylab = "M", main = "fitted line of Level I of the Population model ( group by CP levels)",ylim = c(0,260))	
		} else{
			lines(x,y, type = "l", col = col_vec_plot, lwd = 0.3)
		}	
		lines(x,fv_lv1, type = "l", col = col_vec_plot, lwd = 1.3)
		#lines(x,ff_lv1, type = "b", pch = 21, col = col_vec_plot)
		legend("topright", legend = names(col_vec), col = col_vec, lty = rep(1,length(col_vec)))
		mtext(paste("PID: ", unique(df2$df2_pid), sep = " "), side =3)
	}
}


par(mfrow = c(1,2))
drawCombineALL()






drawCombineIndivi <- function(i){
	
	model_L <- lhs.nlme7
	model_R <- rhs.nlmetest8
	# color vector
	col_vec <- c("black", "red", "green", "blue", "cyan")
	names(col_vec) <- c("cp1", "cp2", "cp3", "cp4", "Group")

		# LHS
		ind_L = df1$df1_subject == i
		x_L <- df1$decay_time[ind_L]
		y_L <- df1$df1_measures[ind_L]
		time <- as.character(unique(df1$df1_datetime[ind_L]))
		# fitted values
		fv_L <- fitted(model_L, level = 2)[ind_L]
		fv_lv1_L <-fitted(model_L, level = 1)[ind_L]
		
		print(i)
		# RHS
		ind_R = df2$df2_subject == i
		x_R <- df2$blow_time[ind_R]
		y_R <- df2$df2_measures[ind_R]
		fv_R <- fitted(model_R, level = 2)[ind_R]
		fv_lv1_R <- fitted(model_R, level = 1)[ind_R]
		
		# COMBINE
		x <- c(x_L, x_R)
		y <- c(y_L, y_R)
		fv <- c(fv_L, fv_R)
		fv_lv1 <- c(fv_lv1_L, fv_lv1_R)
		
		#col_index <- strsplit( unique(names(fv)), "/" )[[1]][1]
		col_index <- unique(df1$df1_condition[ind_L])
		col_vec_plot <- col_vec[col_index]
		
		plot(x,y, type = "b",pch = 6, col = col_vec_plot, lwd = 0.3, xlab = "time (secs)", ylab = "M", main = "fitted lines of Population model - Level I vs Level II on Individual level")		
		lines(x,fv_lv1, type = "l", col = "cyan", lwd = 1.5)
		lines(x,fv, type = "l", pch = 21, col = col_vec_plot, lwd = 1)
		legend("topright", legend = names(col_vec), col = col_vec, lty = rep(1,length(col_vec)))
		mtext(paste("PID: ", unique(df2$df2_pid), "     Time: ", time, sep = " "), side =3)
	
}

drawCombineIndivi(7)


################################################################################# 
# Making Sure that I understand the hierarchical structure properly
#########################################################################
#																		#
#		LHS hierarchical structure by computing fitted values 			#
#																		#
#########################################################################

#lhs.nlme6 <- nlme(df1_measures ~ meanfuncLHS(df1_cp, decay_time, b0,b1,b2) , 
#			fixed = list(b0 ~ df1_cp, b1 ~ 1, b2 ~ 1),
#			random = b0 + b1 + b2 ~ 1| df1_condition /df1_subject,
#			data = df1,
			#start = list(fixed = c(b0 = 1.98, 0,b1 = 5.5,b2 = -0.25,0.5)),
#			start = list(fixed = c(b0 = 1.98, 0,b1 = 5.5,b2 = 0.25)),  # device7
#			verbose = T
#			)
#summary(lhs.nlme6)

#####################
#	Check level 1	#
#####################
B <- as.vector(fixed.effects(lhs.nlme6))
b_i_lv1 <- as.matrix( random.effects(lhs.nlme6)[[1]])
b_i_lv2 <- as.matrix( random.effects(lhs.nlme6)[[2]])

vec <- NULL
indices <- NULL

for( i in 1:length(unique(df1$df1_condition))){
	id <- unique(df1$df1_condition)[i]
	
	index <- df1$df1_condition == id
	indd <- which(index == T)
	M <- model.matrix(~ df1_condition + df1_cp + decay_time + df1_measures + df1_subject, data = df1[index,])
	cp <- unique(df1$df1_cp[index])
	# A_i : design matrix depending on elements on a_i
	A_i <- matrix(0,3,length(B))
	A_i[1,] <- c(1,0,0)
	A_i[2,] <- c(0,1,0)
	A_i[3,] <- c(0,0,1)
	
	# B_i: design matrix (which elements have random effects)
	B_i <- matrix(0,dim(A_i)[1],dim(b_i_lv1)[2])
	B_i[1,] <- c(1,0,0)
	B_i[2,] <- c(0,1,0)
	B_i[3,] <- c(0,0,1)
	
	# individual-specific parameter
	beta_i <- A_i %*% B + B_i %*% b_i_lv1[id,]
	# Z_i : design matrix depending on elements on z_i
	Z_i <- M[,'decay_time']
	
	f_i <- as.matrix(meanfuncLHS(cp,Z_i, beta_i[1], beta_i[2], beta_i[3]))
	vec <- rbind(vec,f_i)
	indices <- c(indices , indd)	
}


ff <- fitted(lhs.nlme6,1)[indices]
cbind(vec,ff, vec /ff)

#####################
#	Check level 2	#
#####################
B <- as.vector(fixed.effects(lhs.nlme6))
b_i_lv1 <- as.matrix( random.effects(lhs.nlme6)[[1]])
b_i_lv2 <- as.matrix( random.effects(lhs.nlme6)[[2]])

vec <- NULL
indices <- NULL

for( i in 1:length(unique(df1$df1_condition))){
	idc <- unique(df1$df1_condition)[i]
	index <- df1$df1_condition == idc
	M <- model.matrix(~ df1_condition + df1_cp + decay_time + df1_measures + df1_subject, data = df1[index,])
	
	subjects  <- unique(M[,'df1_subject'])
	# among (between) individuals covariates
	cp <- unique(df1$df1_cp[index])
	condition <- unique(df1$df1_condition[index])
	
	for(j in 1:length(unique(subjects))){
		ids <- unique(subjects)[j]
		index_subject <- M[,'df1_subject'] == ids
		indd <- df1$df1_condition == idc & df1$df1_subject == ids
		indd <- which(indd == T)
		Ms <- M[index_subject,]
	
		# row name for b_i_lv2
		rn <- paste(as.character(idc), "/",ids, sep = "")
	
		# A_i : design matrix depending on elements on a_i
		A_i <- matrix(0,3,length(B))
		A_i[1,] <- c(1,0,0)
		A_i[2,] <- c(0,1,0)
		A_i[3,] <- c(0,0,1)
	
		# B_i: design matrix (which elements have random effects)
		B_i <- matrix(0,dim(A_i)[1],dim(b_i_lv2)[2])
		B_i[1,] <- c(1,0,0)
		B_i[2,] <- c(0,1,0)
		B_i[3,] <- c(0,0,1)
	
		# individual-specific parameter
		beta_i <- A_i %*% B + B_i %*% b_i_lv1[condition,] + B_i %*% b_i_lv2[rn,]
		# Z_i : design matrix depending on elements on z_i
		Z_i <- Ms[,'decay_time']
	
		f_i <- as.matrix(meanfuncLHS(cp,Z_i, beta_i[1], beta_i[2], beta_i[3]))
		vec <- rbind(vec,f_i)
		indices <- c(indices , indd)
		cat("condition = ", as.character(idc), "\n")
		cat("subject = ", ids, "\n")
		cat(length(f_i), "\n")
	}	
}


ff <- fitted(lhs.nlme6,2)[indices]
cbind(vec,ff, vec /ff)

##########
# nlme 7 
##########
lhs.nlme7 <- nlme(df1_measures ~ meanfuncLHS(df1_cp, decay_time, b0,b1,b2) , 
			fixed = list(b0 ~ df1_cp, b1 ~ 1, b2 ~ df1_cp),
			random = b0 + b1 + b2 ~ 1| df1_condition /df1_subject,
			#groups = ~ df1_subject ,
			data = df1,
			#start = list(fixed = c(b0 = 1.98, 0,b1 = 5.5,b2 = -0.25,0.5)),
			start = list(fixed = c(b0 = 1.98, 0,b1 = 5.5,b2 = 0.25,0)),  # device 7
			verbose = T
			)
summary(lhs.nlme7)

#####################
#	Check level 1	#
#####################
B <- as.vector(fixed.effects(lhs.nlme7))
b_i_lv1 <- as.matrix( random.effects(lhs.nlme7)[[1]])
b_i_lv2 <- as.matrix( random.effects(lhs.nlme7)[[2]])

vec <- NULL
indices <- NULL

for( i in 1:length(unique(df1$df1_condition))){
	id <- unique(df1$df1_condition)[i]
	
	index <- df1$df1_condition == id
	indd <- which(index == T)
	M <- model.matrix(~ df1_condition + df1_cp + decay_time + df1_measures + df1_subject, data = df1[index,])
	cp <- unique(df1$df1_cp[index])
	# A_i : design matrix depending on elements on a_i
	A_i <- matrix(0,3,length(B))
	A_i[1,] <- c(1,cp,0,0,0)
	A_i[2,] <- c(0,0,1,0,0)
	A_i[3,] <- c(0,0,0,1,cp)
	
	# B_i: design matrix (which elements have random effects)
	B_i <- matrix(0,dim(A_i)[1],dim(b_i_lv1)[2])
	B_i[1,] <- c(1,0,0)
	B_i[2,] <- c(0,1,0)
	B_i[3,] <- c(0,0,1)
	
	# individual-specific parameter
	beta_i <- A_i %*% B + B_i %*% b_i_lv1[id,]
	# Z_i : design matrix depending on elements on z_i
	Z_i <- M[,'decay_time']
	
	f_i <- as.matrix(meanfuncLHS(cp,Z_i, beta_i[1], beta_i[2], beta_i[3]))
	vec <- rbind(vec,f_i)
	indices <- c(indices , indd)	
}


ff <- fitted(lhs.nlme7,1)[indices]
cbind(vec,ff, vec /ff)

#####################
#	Check level 2	#
#####################
B <- as.vector(fixed.effects(lhs.nlme7))
b_i_lv1 <- as.matrix( random.effects(lhs.nlme7)[[1]])
b_i_lv2 <- as.matrix( random.effects(lhs.nlme7)[[2]])

vec <- NULL
indices <- NULL

for( i in 1:length(unique(df1$df1_condition))){
	idc <- unique(df1$df1_condition)[i]
	index <- df1$df1_condition == idc
	M <- model.matrix(~ df1_condition + df1_cp + decay_time + df1_measures + df1_subject, data = df1[index,])
	
	subjects  <- unique(M[,'df1_subject'])
	# among (between) individuals covariates
	cp <- unique(df1$df1_cp[index])
	condition <- unique(df1$df1_condition[index])
	
	for(j in 1:length(unique(subjects))){
		ids <- unique(subjects)[j]
		index_subject <- M[,'df1_subject'] == ids
		indd <- df1$df1_condition == idc & df1$df1_subject == ids
		indd <- which(indd == T)
		Ms <- M[index_subject,]
	
		# row name for b_i_lv2
		rn <- paste(as.character(idc), "/",ids, sep = "")
	
		# A_i : design matrix depending on elements on a_i
		A_i <- matrix(0,3,length(B))
		A_i[1,] <- c(1,cp,0,0,0)
		A_i[2,] <- c(0,0,1,0,0)
		A_i[3,] <- c(0,0,0,1,cp)
	
		# B_i: design matrix (which elements have random effects)
		B_i <- matrix(0,dim(A_i)[1],dim(b_i_lv2)[2])
		B_i[1,] <- c(1,0,0)
		B_i[2,] <- c(0,1,0)
		B_i[3,] <- c(0,0,1)
	
		# individual-specific parameter
		beta_i <- A_i %*% B + B_i %*% b_i_lv1[condition,] + B_i %*% b_i_lv2[rn,]
		# Z_i : design matrix depending on elements on z_i
		Z_i <- Ms[,'decay_time']
	
		f_i <- as.matrix(meanfuncLHS(cp,Z_i, beta_i[1], beta_i[2], beta_i[3]))
		vec <- rbind(vec,f_i)
		indices <- c(indices , indd)
		cat("condition = ", as.character(idc), "\n")
		cat("subject = ", ids, "\n")
		cat(length(f_i), "\n")
	}	
}


ff <- fitted(lhs.nlme7,2)[indices]
cbind(vec,ff, vec /ff)






#########################################################################
#																		#
#		RHS hierarchical structure by computing fitted values 			#
#																		#
#########################################################################
rhs.nlmetest8 <- nlme(df2_measures ~ meanfuncRHStest(df2_cp, blow_time,b0, b1,b2),
				fixed = list(b0~ df2_cp, b1 ~df2_cp, b2~ df2_cp),
				random = b0 + b1 + b2 ~ 1 | df2_condition /df2_subject,	
				#random = list(b0~1,b1 ~ 1, b2~ 1),
				#groups = ~df2_subject,
				#groups = ~df2_condition,
				data = df2,
				start = list(fixed = c(b0 = 3.979,0.0467, b1 = 3.618,0.77,b2 = 0.73,-0.1)),
				verbose = T)

summary(rhs.nlmetest8)

#####################
#	Check level 1	#
#####################
B <- as.vector(fixed.effects(rhs.nlmetest8))
b_i_lv1 <- as.matrix( random.effects(rhs.nlmetest8)[[1]])
b_i_lv2 <- as.matrix( random.effects(rhs.nlmetest8)[[2]])

vec <- NULL
indices <- NULL

for( i in 1:length(unique(df2$df2_condition))){
	id <- unique(df2$df2_condition)[i]
	
	index <- df2$df2_condition == id
	indd <- which(index == T)
	M <- model.matrix(~ df2_condition + df2_cp + blow_time + df2_measures + df2_subject, data = df2[index,])
	cp <- unique(df2$df2_cp[index])
	# A_i : design matrix depending on elements on a_i
	A_i <- matrix(0,3,length(B))
	A_i[1,] <- c(1,cp,0,0,0,0)
	A_i[2,] <- c(0,0,1,cp,0,0)
	A_i[3,] <- c(0,0,0,0,1,cp)
	
	# B_i: design matrix (which elements have random effects)
	B_i <- matrix(0,dim(A_i)[1],dim(b_i_lv1)[2])
	B_i[1,] <- c(1,0,0)
	B_i[2,] <- c(0,1,0)
	B_i[3,] <- c(0,0,1)
	
	# individual-specific parameter
	beta_i <- A_i %*% B + B_i %*% b_i_lv1[id,]
	# Z_i : design matrix depending on elements on z_i
	Z_i <- M[,'blow_time']
	
	f_i <- as.matrix(meanfuncRHStest(cp,Z_i, beta_i[1], beta_i[2], beta_i[3]))
	vec <- rbind(vec,f_i)
	indices <- c(indices , indd)	
}


ff <- fitted(rhs.nlmetest8,1)[indices]
cbind(vec,ff, vec /ff)

#####################
#	Check level 2	#
#####################
B <- as.vector(fixed.effects(rhs.nlmetest8))
b_i_lv1 <- as.matrix( random.effects(rhs.nlmetest8)[[1]])
b_i_lv2 <- as.matrix( random.effects(rhs.nlmetest8)[[2]])

vec <- NULL
indices <- NULL

for( i in 1:length(unique(df2$df2_condition))){
	idc <- unique(df2$df2_condition)[i]
	index <- df2$df2_condition == idc
	M <- model.matrix(~ df2_condition + df2_cp + blow_time + df2_measures + df2_subject, data = df2[index,])
	
	subjects  <- unique(M[,'df2_subject'])
	# among (between) individuals covariates
	cp <- unique(df2$df2_cp[index])
	condition <- unique(df2$df2_condition[index])
	
	for(j in 1:length(unique(subjects))){
		ids <- unique(subjects)[j]
		index_subject <- M[,'df2_subject'] == ids
		indd <- df2$df2_condition == idc & df2$df2_subject == ids
		indd <- which(indd == T)
		Ms <- M[index_subject,]
	
		# row name for b_i_lv2
		rn <- paste(as.character(idc), "/",ids, sep = "")
	
		# A_i : design matrix depending on elements on a_i
		A_i <- matrix(0,3,length(B))
		A_i[1,] <- c(1,cp,0,0,0,0)
		A_i[2,] <- c(0,0,1,cp,0,0)
		A_i[3,] <- c(0,0,0,0,1,cp)
	
		# B_i: design matrix (which elements have random effects)
		B_i <- matrix(0,dim(A_i)[1],dim(b_i_lv2)[2])
		B_i[1,] <- c(1,0,0)
		B_i[2,] <- c(0,1,0)
		B_i[3,] <- c(0,0,1)
	
		# individual-specific parameter
		beta_i <- A_i %*% B + B_i %*% b_i_lv1[condition,] + B_i %*% b_i_lv2[rn,]
		# Z_i : design matrix depending on elements on z_i
		Z_i <- Ms[,'blow_time']
	
		f_i <- as.matrix(meanfuncRHStest(cp,Z_i, beta_i[1], beta_i[2], beta_i[3]))
		vec <- rbind(vec,f_i)
		indices <- c(indices , indd)
		cat("condition = ", as.character(idc), "\n")
		cat("subject = ", ids, "\n")
		cat(length(f_i), "\n")
	}	
}


ff <- fitted(rhs.nlmetest8,2)[indices]
cbind(vec,ff, vec /ff)