library(xtable)
# defined my own "which" function: find last seen desired value
which.min2 <- function(x, last.index = T){
	if(last.index == T){
		return(rev(which(x==min(x)))[1] )
	} else{
		NA
	}
}

which.max2 <- function(x, last.index = T){
	if(last.index == T){
		return(rev(which(x==max(x)))[1] )
	} else{
		NA
	}
}


which.last <- function(x , y){
	return( rev(which(x == y))[1] )
}


# split PID into 2 groups first: qa2_4 and qa3_1 (note : all PIDS in testpid file are unique)
qa2_4 <- testpid$pid[testpid$testName == "qa2_4"]
qa3_1 <- testpid$pid[testpid$testName != "qa2_4"]

# look at those W/O " Error " : include Success + Problem pid
index <- NULL
for(i in 1: nrow(bdata)){
	if( grepl("Error",toString(bdata$Dummy[i])) == TRUE ){
		index <- c(index, i )
	}
}

bigset <- bdata
bigset <- bigset[-index, ]


# look at Success cells only
indexx <- NULL
for(i in 1: nrow(bdata)){
	if( grepl("Success",toString(bdata$Dummy[i])) == TRUE ){
		indexx <- c(indexx, i )
	}
}
sdata <- bdata
sdata <- sdata[indexx,]


#########################################
#										#
#		Exploratory Data Analysis		#
#										#
#########################################

#		Output : Graph 
IndiviPlot <- function(pid,data){
	
	index <- 1: nrow(data)
	# corresponding row numbers
	num <- index[pid == data$PID]
	
	newdata <- data[num,]
	string <- newdata$Data
	Data <- sapply(string, as.character)
	test <- strsplit(Data, "-")	#split string by regex "-"
	test2 <- lapply(test, as.double)	
	l <- lapply(test2, length)
	x <- lapply(test2, seq_along)
	y <- test2
	for(i in 1:nrow(newdata)){
		x[[i]] <- (x[[i]]-1) * 25/ (l[[i]] - 1)
	}
	# check number of occurences of PID in dataset
	print( paste("# of occurrence: ",  length(num), " \n") )
	# ONLY VISUALISE those devices that occurs > 6 times in the dataset
	col_vec <- c("black", "red", "cyan", "blue", "green", "purple")
	names(col_vec) <- c("cp1", "cp2", "cp3", "cp4", "tp1", "tp4")
	col_index <- as.character(newdata$Condition)
	col_vec_plot <- col_vec[col_index]
	
	cat( paste(newdata$Condition) , sep = "\n")
	
	print("\tIndividual Lengths:\n")
	cat(paste(l) , sep = "\n")
	print(newdata[,-which(names(data) %in% c("Data"))])
	export <- data.frame(newdata[,-which(names(data) %in% c("Data"))],stringsAsFactors = T)
	# keep system time
	export[,1] <- as.character(export[,1])
	# remove extra PID column
	rownames(export) <- NULL
	export <- export[,-3]
	tab <- xtable(export)
	plot(x[[1]],y[[1]], type = "l", xlab = "Time (secs)", ylab = "M", col = col_vec_plot[1], ylim = c(0,260), main = "Exploratory Analysis")
	for(i in 2:length(num)){
		lines(x[[i]],y[[i]], type = "l", col = col_vec_plot[i])
	}
	legend("topright", legend = names(col_vec), col = col_vec, lty = rep(1,length(col_vec)))
	mtext(paste("PID: ", pid, sep = " "), side =3)
	print(table(newdata$Condition))
	return(tab)
}

#IndiviPlot(filtered_pid[300], newdf)
#IndiviPlot(filtered_pid[9], sdata)
#IndiviPlot(qa3_1[3], sdata)

#############################
#							#
#		Preprocessing		#
#							#
#############################

# how many times each PID occurs in the "SUCCESS ONLY" dataset
occurs <- table(sdata$PID)

# frequency of occurrence
occur_freq <- table(table(sdata$PID))
# plot(occur_freq, type = "h" , xlab = "# of occurrence", ylab = "frequency of occurrence", main = "frequency of occurrence (ALL PIDs)")

# filtering those PIDs who occurred < 15 times in dataset
filtered <- occurs[names(which(occurs >= 15))]
sum(filtered)		# 10406 observations

# note that the filtered PIDS are mostly (99%) qa3_1 and contain "SUCCESS ONLY" cases 

# pick out column 1,2 , 12,13,14,15
filtered_pid <- names(filtered)	

# new data frame
newdf <- NULL
for(i in 1: length(filtered_pid)){
	# data from file "calibration.csv"
	indivi_data <- sdata[sdata$PID == filtered_pid[i],]
	indivi_data <- data.frame(indivi_data[,c(1,2,12:15)], stringsAsFactors = T)
	l <- dim(indivi_data)[1]
	Date.Time <- strptime(as.character(indivi_data[,1]),format="%m/%d/%Y %H:%M")
 	indivi_data <- data.frame(Date.Time,indivi_data[,-1], stringsAsFactors = T)
 	
	# data from another file "testPID.csv"
	indivi_data2 <- data.frame(testpid[testpid$pid == filtered_pid[i],], stringsAsFactors = T)
	indivi_data2 <- indivi_data2[rep(row.names(indivi_data2) , l), ]
	
	indivi_data <- data.frame(indivi_data, indivi_data2, stringsAsFactors = T)
	indivi_data <- indivi_data[,c(1,2,7:10, 3,4,5,6)]
	
	newdf <- rbind(newdf, indivi_data)
}




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

# (LHS + RHS different lengths ) (no adjustments)

# use AfterWarmUp value given from CSV (LHS + RHS different lengths ) (no adjustments)
Indivi_Partition_Matrix <- function(pid,dataa){
	
	index <- 1: nrow(dataa)
	# corresponding row numbers
	num <- index[pid == dataa$PID]
	
	newdata <- dataa[num,]
	newdata <- cbind(newdata, subject = (1:nrow(newdata)))

	string <- newdata$Data
	string <- sapply(string, as.character)
	data <- strsplit(string, "-")	#split string by regex "-"
	data <- lapply(data, as.double)	# in numeric form
	
	# minimum = after.warmup values
	minimum <- as.list(as.numeric(as.character(newdata$After.WarmUp)))
	min_index <- mapply(which.last,data, minimum )	# After warmup index 
	
	# split into 2 parts
	
	# 1st part: stop at after warm up values
	data1 <- mapply(function(d,i) d[1:i], data,min_index)
	len1 <- lapply(data1, length)
	num <- nrow(newdata)

	# condition and CP level
	cp <- c(0.2,0.4,0.6,0.8)
	names(cp) <- c("cp1", "cp2", "cp3", "cp4")
	
	# 1st dataframe
	datetime <- as.character(newdata$Date.Time)			# col1
	pid <- newdata$pid									# col2
	testgroup <- newdata$TestGroup						# col3
	factory <- as.character(newdata$Factory)				# col4
	testName <- as.character(newdata$testName)			# col5
	condition <- as.character(newdata$Condition)			# col6
	subject <- newdata$subject
	
	df1_datetime <- c()
	df1_pid <- c()
	df1_testgroup <- c()
	df1_factory <- c()
	df1_testName <- c()
	df1_condition <- c()
	df1_cp <- c()
	decay_time <- c()			# decay time (20 seconds)
	df1_measures <- c()			# measurements
	df1_subject <- c()
	for(i in 1:num){
		df1_datetime <- append(df1_datetime, rep(datetime[i], len1[[i]]))
		df1_pid <- append(df1_pid, rep(pid[i],len1[[i]]))
		df1_testgroup <- append( df1_testgroup, rep(testgroup[i], len1[[i]]))
		df1_factory <- append(df1_factory, rep(factory[i], len1[[i]]))
		df1_testName <- append(df1_testName , rep(testName[i], len1[[i]]))
		# CP levels (factor)
		df1_condition <- append(df1_condition, rep(condition[i], len1[[i]]))
		# Cp lvl
		df1_cp <- append(df1_cp, rep(cp[condition[i]] , len1[[i]]))
		# decay time (0 -> 20s or 1,2,3,4,5,6,7,8,9,...)
		decay_time <- c(decay_time, (20/(len1[[i]]-1))* (seq_along(data1[[i]]) - 1) )
		# measurements
		df1_measures	 <- c(df1_measures, data1[[i]])
		df1_subject <- append(df1_subject, rep(subject[i], len1[[i]]))

	}
	names(df1_cp) <- NULL
	df1 <- data.frame(df1_datetime, df1_pid, df1_testgroup, df1_factory, df1_testName, df1_condition, df1_cp, df1_subject,cbind(decay_time, df1_measures), stringsAsFactors = T)
	
	# 2nd part: append maximum values
	data2 <- mapply(function(d,i) d[-c(1:i)], data,min_index)
	len2 <- lapply(data2,length)
	
		
	df2_datetime <- c()
	df2_pid <- c()
	df2_testgroup <- c()
	df2_factory <- c()
	df2_testName <- c()
	df2_condition <- c()
	df2_cp <- c()
	blow_time <- c()			# blow-in time (5 seconds)
	df2_measures <- c()			# measurements
	df2_subject <- c()
	
	for(i in 1:num){
		df2_datetime <- append(df2_datetime, rep(datetime[i], len2[[i]]))
		df2_pid <- append(df2_pid, rep(pid[i],len2[[i]]))
		df2_testgroup <- append( df2_testgroup, rep(testgroup[i], len2[[i]]))
		df2_factory <- append(df2_factory, rep(factory[i], len2[[i]]))
		df2_testName <- append(df2_testName , rep(testName[i], len2[[i]]))
		# CP levels
		df2_condition <- append(df2_condition, rep(condition[i], len2[[i]]))
		# cp lvl
		df2_cp <- append(df2_cp, rep(cp[condition[i]] , len2[[i]]))
		# decay time (0 -> 5s or 1,2,3,4,5,6,7,8,9,...)
		#blow_time <- c(blow_time, 20+(5/(len2[[i]]-1))* (seq_along(data2[[i]]) -1 ) )
		# if exclude minimum
		blow_time <- c(blow_time, 20+(5/(len2[[i]]))* (seq_along(data2[[i]])  ) )

		# measurements
		df2_measures	 <- c(df2_measures, data2[[i]])
		df2_subject <- append(df2_subject, rep(subject[i], len2[[i]]))
	}
	
	names(df2_cp) <- NULL
	df2 <- data.frame(df2_datetime, df2_pid, df2_testgroup, df2_factory, df2_testName, df2_condition, df2_cp, df2_subject, cbind(blow_time, df2_measures), stringsAsFactors = T)
	
	
	# color
	col_vec <- c("black", "red", "cyan", "blue", "green", "purple")
	names(col_vec) <- c("cp1", "cp2", "cp3", "cp4", "tp1", "tp4")
	col_index <- as.character(newdata$Condition)
	col_vec_plot <- col_vec[col_index]
	
	# y index
	y.ind1 <- cumsum(unlist(len1))
	y.ind2 <- cumsum(unlist(len2))

	x <- c(df1$decay_time[1:len1[[1]]] ,df2$blow_time[1:len2[[1]]])
	y <- c(df1$df1_measures[1:len1[[1]]] ,df2$df2_measures[1:len2[[1]]])
	plot(x,y, type = "l", xlab = "Time (secs)", ylab = "M", col = col_vec_plot[1], ylim = c(0,260), xlim = c(0,27), main = "Cut-off point adjusted by After.WarmUp condition")
	for(i in 1:(num - 1) ){
		a <- y.ind1[i] + 1
		b <- y.ind1[i+1]
		c <- y.ind2[i] + 1
		d <- y.ind2[i+1]
		x <- c(df1$decay_time[a:b] ,df2$blow_time[c:d])
		y <- c(df1$df1_measures[a:b] ,df2$df2_measures[c:d])

		lines(x,y, type = "l", col = col_vec_plot[i+1])
	}
	legend("topright", legend =names(col_vec), col = col_vec, lty = rep(1,4))
	mtext(paste("PID: ", pid, sep = " "), side =3)

	return(list(df1 = df1 , df2 = df2))
}
