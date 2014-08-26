#theta <- ppoints(100)
#uniform prior
#posterior <- dbeta(theta, 27,23)
#par(mex = 0.6)
#plot(theta, posterior, type = "l")
#strong prior 
#prior <- dbeta(theta, 100,1)
#posterior.123 <- dbeta(theta, 127,24)

#sampling with grid method (non-conjugate prior)

#theta.sample <- sample(theta, size = 10000, replace = TRUE, prob = posterior)
#hist(theta.sample , nclass = 50)



#Bayesian Project 2
#Data
school <- c("A", "B", "C", "D", "E","F", "G", "H")
y <- c(28,8,-3,7,-1,1,18,12)
sigma <- c(15,10,16,11,9,11,10,18)^2

#hierarchical model
#level 1 
#posterior theta
theta.post <- function(j,mu, tor.sq)
{
		mean.j <- ( tor.sq * y[j] + mu * sigma[j] ) / (tor.sq + sigma[j])
		var.j <- sigma[j] * tor.sq / ( tor.sq + sigma[j])
		theta.j <- rnorm(length(tor.sq), mean = mean.j, sd = sqrt(var.j))
		
	
	return
	{
		theta.j
	}
}

#marginal posterior distribution of mu
mu.post <- function(tor.sq)
{
	mean.j <- sum( (1/ (sigma + tor.sq)) * y ) / sum( 1 / (sigma + tor.sq))
	var <- 1/ sum( 1 / (sigma + tor.sq))
	
	return
	{
		rnorm(length(tor.sq), mean = mean.j, sd = sqrt(var))
	}
}


#sample marginal posterior distribution of tor.square
tor.post <- function(size1)
{
	vect <- c()
	tor <- seq(0,30,0.1)
	for( i in 1: length(tor))
	{
		a <- (1 / sum( 1 / (sigma + tor[i]^2)))^(0.5)
		b <- prod( (sigma + tor[i]^2)^(-1/2)  )
		c <- sum((y^2) / (tor[i]^2 + sigma))
		d <- ((sum(y / (tor[i]^2 + sigma)) )^2) / sum( 1 / (sigma + tor[i]^2))
	
		density <- a * b * exp(-(1/2)* (c - d) )
		vect <- append(vect, density)
	}
	tor.sample <- sample(tor, size = size1 , replace = TRUE, prob = vect)
	return
	{
		#tor.square
		tor.sample
	}

}


##########################
#Reproduce computations ##
##########################
#marginal distribution of tor.square plot against tor
tor.marginal <- function(tor)
{
	a <- (1 / sum( 1 / (sigma + tor^2)))^(0.5)
	b <- prod( (sigma + tor^2)^(-1/2)  )
	c <- sum((y^2) / (tor^2 + sigma))
	d <- ((sum(y / (tor^2 + sigma)) )^2) / sum( 1 / (sigma + tor^2))
	return
	{
		a * b * exp(-(1/2)* (c - d) )
	}
}
#plot p(tor^2 | y) against tor
tor.plot <- function()
{
	y <- lapply(seq(0,30,0.1), tor.marginal)
	
	plot(seq(0,30,0.1), y)
}

#Plot conditional posterior means of treatment effects
conditional.post <- function(j , tor)
{
	expected.mu <- sum(y / (sigma + tor^2))  / (sum(1 / (sigma + tor^2)))
	a <- (tor^2) * y[j] + expected.mu*sigma[j]
	b <- (tor^2) + sigma[j]
	return
	{
		a / b
	}
}
cond.plot <- function()
{
	plot(seq(0,30,0.1), sapply(seq(0,30,0.1), conditional.post, j = 1), xlab = "tau" , ylab = "Estimated Treatment Effects" , main = "Conditional posteiror means of treatment effects", xlim = c(0,30) , ylim = c(-5,30), type = "l")
	
	for( i in 2:8)
	{
		par(new = T)
		plot(seq(0,30,0.1), sapply(seq(0,30,0.1), conditional.post, j = i), xlim = c(0,30) , ylim = c(-5,30), xlab = "", ylab = "", main = "", col = 2*i -1, type = "l")
	}
	
	legend(x = 25, y = 25, col = c(1,3,5,7,9,11,13,15) , legend = c("A", "B", "C", "D", "E","F", "G", "H"),lty = 1)
	text(locator(), labels = c("A", "B", "C", "D", "E","F", "G", "H"))
}

#Plot conditional posterior standard deviations of treatments sd(theta_j | tor, y)
st.d <- function(j , tor)
{
	#var(theta_j | tor, y)
	a <- (sigma[j] * tor^2) / (sigma[j] + tor^2)
	b <- (sigma[j] / (sigma[j] + tor^2))^2
		#var(mu | tor , y)
		var.mu <- 1/ sum(1 / (sigma + tor^2))
	return
	{
		sqrt(a + b * var.mu)
	}
}

st.d.plot <- function()
{
	plot(seq(0,30,0.1), sapply(seq(0,30,0.1), st.d, j = 1), xlab = "tau" , ylab = "Posterior Standard Deviations" , main = "Condtional posterior standard deviations of treatment effects", xlim = c(0,30) , ylim = c(0,20), type = "l")
	
	for( i in 2:8)
	{
		par(new = T)
		plot(seq(0,30,0.1), sapply(seq(0,30,0.1), st.d, j = i), xlim = c(0,30) , ylim = c(0,20), xlab = "", ylab = "", main = "", col = 2*i -1, type = "l")
	}
	
	legend(x = 3, y = 20, col = c(1,3,5,7,9,11,13,15) , legend = c("A", "B", "C", "D", "E","F", "G", "H"),lty = 1)
	text(locator(), labels = c("A", "B", "C", "D", "E","F", "G", "H"))
}


#reproduce posterior table
main.method <- function(num)
{
	#2.5% quantile
	lower.CI <- c()
	#25% quantile
	lower.Q <- c()
	#median
	med <- c()
	#75%
	upper.Q <- c()
	#97.5%
	upper.CI <- c()
	
	store <- matrix( 0 , nrow = 8, ncol = num)
	for( j in 1:8)
	{
		step1 <- tor.post(num)

		step2 <- mu.post(step1)

		store[j,] <- sort(theta.post(j,step1,step2) )
		
		lower.CI <- append(lower.CI, sprintf("%1.0f", store[j,num*0.025])	)
		lower.Q <- append(lower.Q, sprintf("%1.0f",store[j, num *0.25]))
		med <- append(med , sprintf("%1.0f",store[j, num*0.5]))
		upper.Q <- append(upper.Q, sprintf("%1.0f",store[j, num*0.75]) )
		upper.CI <- append(upper.CI, sprintf("%1.0f",store[j, num* 0.975])  )
	}
	
	present <- matrix( 0 , nrow = 8, ncol = 6)
	present[,1] <- school
	present[,2] <- lower.CI
	present[,3] <- lower.Q
	present[,4] <- med
	present[,5] <- upper.Q
	present[,6] <- upper.CI
	cat("\t","School","\t\t\t", "Posterior quantiles" , "\n",sep = "")
	cat("\t\t\t", "2.5%", "25%", "median" ,  "75%",  "97.5%", "\n")
	cat("________________________________________" , sep = "\n")
	print(present)

}
