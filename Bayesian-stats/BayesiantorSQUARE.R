library(MASS)
library(xtable)
#Bayesian Project 2
#Data
school <- c("A", "B", "C", "D", "E","F", "G", "H")
# estimated coaching effects
y <- c(28.39,7.94,-2.75,6.82,-0.64,0.63,18.01,12.16)
sigma <- c(14.9,10.2,16.3,11,9.4,11.4,10.4,17.6)^2		
# variable sigma = sigma^2

#hierarchical model
#level 1 
#posterior theta : p( theta_j   |   mu, tor^2 , y ) 
theta.post <- function(j,mu, tor){
	mean.j <- ( tor^2 * y[j] + mu * sigma[j] ) / (tor^2 + sigma[j])
	var.j <- (sigma[j] * tor^2) / ( tor^2 + sigma[j])
	theta.j <- rnorm(length(tor), mean = mean.j, sd = sqrt(var.j))
		
	return(
		theta.j
	)
}

#marginal posterior distribution of mu : 	p( mu | tor^2 , y)
mu.post <- function(tor){
	mean.j <- sum( (1/ (sigma + tor^2)) * y ) / sum( 1 / (sigma + tor^2))
	var <- 1/ sum( 1 / (sigma + tor^2))
	
	return(
		rnorm(length(tor^2), mean = mean.j, sd = sqrt(var))
	)
}


#sample marginal posterior distribution of tor.square  :  p( tor | y)
tor.post <- function(size1){
	vect <- c()
	tor <- seq(0,30,0.1)
	for( i in 1: length(tor)){
		a <- (1 / sum( 1 / (sigma + tor[i]^2)))^(0.5)
		b <- prod( (sigma + tor[i]^2)^(-1/2)  )
		c <- sum((y^2) / (tor[i]^2 + sigma))
		d <- ((sum(y / (tor[i]^2 + sigma)) )^2) / sum( 1 / (sigma + tor[i]^2))
	
		density <- a * b * exp(-(1/2)* (c - d) )
		vect <- append(vect, density)
	}
	tor.sample <- sample(tor, size = size1 , replace = TRUE, prob = vect)
	return(
		#tor
		tor.sample
	)
}


#############################
#							#
#	Reproduce computations	#
#							#
#############################

#marginal distribution of tor plot against tor
tor.marginal <- function(tor){
	a <- (1 / sum( 1 / (sigma + tor^2)))^(0.5)
	b <- prod( (sigma + tor^2)^(-1/2)  )
	c <- sum((y^2) / (tor^2 + sigma))
	d <- ((sum(y / (tor^2 + sigma)) )^2) / sum( 1 / (sigma + tor^2))
	return(
		a * b * exp(-(1/2)* (c - d) )
	)
}
#plot p(tor | y) against tor
tor.plot <- function(){
	y <- lapply(seq(0,30,0.1), tor.marginal)
	plot(seq(0,30,0.1), y, xlab = "tau" ,type = "l", ylab = "p(tau | y)" , main = "Marginal Posterior Density")
}
pdf(file = "marginal_posterior_density_tor.pdf", width = 11.69, height = 8.27)
tor.plot()
dev.off()
#Plot conditional posterior means of treatment effects 
# E(theta_j | tor, y) by law of iterated expectation

conditional.post <- function(j , tor){
	expected.mu <- sum(y / (sigma + tor^2))  / (sum(1 / (sigma + tor^2)))
	a <- (tor^2) * y[j] + expected.mu*sigma[j]
	b <- (tor^2) + sigma[j]
	return(
		a / b
	)
}
cond.plot <- function(){
	plot(seq(0,30,0.1), sapply(seq(0,30,0.1), conditional.post, j = 1), xlab = "tau" , ylab = "Estimated Treatment Effects" , main = "Conditional posteiror means of treatment effects", xlim = c(0,32) , ylim = c(-5,30), type = "l")
	
	for( i in 2:8){
		par(new = T)
		plot(seq(0,30,0.1), sapply(seq(0,30,0.1), conditional.post, j = i), xlim = c(0,32) , ylim = c(-5,30), xlab = "", ylab = "", main = "", col = 3*i -1, type = "l")
	}
	labels = c("A", "B", "C", "D", "E","F", "G", "H")
	legend("topright", legend = labels, col = c(1,3*(2:8)-1), lty = rep(1,8))
	#text(locator(), labels = c("A", "B", "C", "D", "E","F", "G", "H"))
}
pdf(file = "conditional_plot.pdf", width = 11.69, height = 8.27)
cond.plot()
dev.off()
#Plot conditional posterior standard deviations of treatments sd(theta_j | tor, y)
st.d <- function(j , tor){
	#var(theta_j | tor, y)
	a <- (sigma[j] * tor^2) / (sigma[j] + tor^2)
	b <- (sigma[j] / (sigma[j] + tor^2))^2
		#var(mu | tor , y)
		var.mu <- 1/ sum(1 / (sigma + tor^2))
	return(
		sqrt(a + b * var.mu)
	)
}


st.d.plot <- function(){
	plot(seq(0,30,0.1), sapply(seq(0,30,0.1), st.d, j = 1), xlab = "tau" , ylab = "Posterior Standard Deviations" , main = "Condtional posterior standard deviations of treatment effects", xlim = c(0,33) , ylim = c(0,20), type = "l")
	
	for( i in 2:8){
		par(new = T)
		plot(seq(0,30,0.1), sapply(seq(0,30,0.1), st.d , j = i), xlim = c(0,33) , ylim = c(0,20), xlab = "", ylab = "", main = "", col = 3*i -1, type = "l")
	}
	labels = c("A", "B", "C", "D", "E","F", "G", "H")
	legend("topright", legend = labels, col = c(1,3*(2:8)-1), lty = rep(1,8))

	#text(locator(), labels = c("A", "B", "C", "D", "E","F", "G", "H"))
}
pdf(file = "sd_plot.pdf", width = 11.69, height = 8.27)
st.d.plot()
dev.off()


# reproduce posterior table
# summary of 200 simulations of the treatment effects in 8 schools
main.method <- function(num){
	step3.unsorted <- matrix(0 , nrow = 8, ncol = num)

	for( j in 1:8){
		# no need to square tor to become tor^2, this is being taken care in
		# mu.post and theta.post
		step1 <- tor.post(num)

		step2 <- mu.post(step1)
		
		step3.unsorted[j,] <- theta.post(j,step1,step2)
	}

	rownames(step3.unsorted) <- c("  A  ", "  B  ", "  C  ", "  D  ", "  E  ","  F  ", "  G  ", "  H  ")
	if(interactive()){
		answer <- readline("Detailed results ?")
		if(answer == "y"){	
			cat("School","\t\t", "Posterior quantiles" , "\n",sep = "")
			cat("________________________________________" , sep = "\n")
			print(t(round(apply(step3.unsorted, 1, quantile,prob = c(0.025,0.25,0.5,0.75,0.975)))))
			cat("\n")
	
			#reproduce histograms
			par(mfrow = c(1,2))
			hist(step3.unsorted[1,], freq = TRUE, breaks = 20, xlim = c(-20, 60) , main = "Effect in School A" , xlab = "theta_1")
			cat("\n")
			#second histogram

			hist(apply(step3.unsorted, 2, max), freq = TRUE, breaks = 20, xlim = c(-20,60), main = "largest effect" , xlab = "max{theta_j}")
		} else {}
	}	
	
	if(interactive()){
		answer <- readline("Return matrix?")
		if(answer == "y"){
			#return unsorted data
			return(step3.unsorted)
		} else{}
	}
}


# consider the case if tor^2 = infinity
question.e <- function(num){
	unsorted <- matrix( 0 , nrow = 8  ,ncol = num)
	#generate theta.j
	for(i in 1:8){
		unsorted[i,] <- rnorm(num, mean = y[i], sd = sqrt(sigma[i]))
	}
	return(unsorted)
	
}

# consider the case if tor^2 = 0 
question.f <- function(num){	
	unsorted <- matrix( 0 , nrow = 8  ,ncol = num)
	for(i in 1:8){
		mu <- rnorm(num, mean = sum(y / sigma) / sum(1/sigma) , sd = sqrt(1/ sum(1/sigma)))
		unsorted[i,] <- rnorm(num, mean = mu, sd = 0)
	}
	return(unsorted)
}


# use this function below:
#	1.	for each school j, reproduce compute the probability that its coaching program is the best.
#	2. 	For each pair of schools, j and k, compute the probability that the 
#		coaching program for school j is better than that for school k


best.coaching <- function(num){
	ma <- matrix(0 , nrow = 8 , ncol  = 8)
	if(interactive()){
		answer <- readline("Which question ? D or E or F? ")
		if(answer == "d"){
			step3.unsorted <- main.method(num)
		} else if(answer == "e"){
			step3.unsorted <- question.e(num)
		} else if(answer == "f"){
			step3.unsorted <- question.f(num)
		} else{
			print("Incorrect input. Please start again")
			best.coaching(num)
		}
	}

	maxi <- apply(step3.unsorted, 2, max)
	# compute for each pair of schools, j and k,
	# probability that the coaching program for school j is better than 
	# that for school k.
	for(i in 1:8){
		cat("School ", i, "against others\n")
		cat("__________________________________", "\n")
		for(j in 1:8){
			success <- 0
			for(k in 1:num){
				if(i != j ){
					if(step3.unsorted[i,k] > step3.unsorted[j,k]){
						success <- success + 1
					}	
				}
			}
			prob <- success / num
			if(i != j ){
				ma[i,j] <- prob
			} else{
				ma[i,j] <- c(" - ")
			}	
			cat("School", i ," >", j,": ",prob , " or ", success, "/", num, "\n")
		}
		cat("\n\n")
	}
	bmatrix <- xtable(ma, align = rep("", ncol(ma)+1))
	print(bmatrix , floating = FALSE, tabular.environment = "bmatrix", hline.after = NULL, include.rownames = F, include.colnames = F)
	print(ma)
	
	cat("\n\n")
	
	# probability that school j's coaching program is the best
	for( j in 1: 8){
		best <- 0
		for( k in 1:num){	
			if(step3.unsorted[j,k] == maxi[k]){
				best <- best + 1			
			}
		}
		best.prob <- best / num
		cat("School ", j, "prob of being best coaching program: \n " )
		cat("---------------------------------------", "\n")
		cat(best.prob , "or ", best," / " , num , "\n\n")
	}
	
	# latex matrix 
	
}

pdf(file = "hists.pdf", width = 11.69, height = 8.27)
best.coaching(200)
dev.off()