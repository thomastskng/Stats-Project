library(MASS)

#Bayesian Project 2
#Data
school <- c("A", "B", "C", "D", "E","F", "G", "H")
y <- c(28.39,7.94,-2.75,6.82,-0.64,0.63,18.01,12.16)
sigma <- c(14.9,10.2,16.3,11,9.4,11.4,10.4,17.6)^2

#hierarchical model
#level 1 
#posterior theta
theta.post <- function(j,mu, tor)
{
		mean.j <- ( tor^2 * y[j] + mu * sigma[j] ) / (tor^2 + sigma[j])
		var.j <- (sigma[j] * tor^2) / ( tor^2 + sigma[j])
		theta.j <- rnorm(length(tor), mean = mean.j, sd = sqrt(var.j))
		
	
	return
	{
		theta.j
	}
}

#marginal posterior distribution of mu
mu.post <- function(tor)
{
	mean.j <- sum( (1/ (sigma + tor^2)) * y ) / sum( 1 / (sigma + tor^2))
	var <- 1/ sum( 1 / (sigma + tor^2))
	
	return
	{
		rnorm(length(tor^2), mean = mean.j, sd = sqrt(var))
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
		#tor
		tor.sample
	}

}


##########################
#Reproduce computations ##
##########################
#marginal distribution of tor plot against tor
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
	
	plot(seq(0,30,0.1), y, xlab = "tau" , ylab = "p(tau | y)" , main = "Marginal Posterior Density")
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
		plot(seq(0,30,0.1), sapply(seq(0,30,0.1), st.d , j = i), xlim = c(0,30) , ylim = c(0,20), xlab = "", ylab = "", main = "", col = 2*i -1, type = "l")
	}
	
	text(locator(), labels = c("A", "B", "C", "D", "E","F", "G", "H"))
}


#reproduce posterior table
main.method <- function(num)
{
	step3.unsorted <- matrix(0 , nrow = 8, ncol = num)

	for( j in 1:8)
	{
		step1 <- tor.post(num)

		step2 <- mu.post(step1)
		
		step3.unsorted[j,] <- theta.post(j,step1,step2)

	}

	rownames(step3.unsorted) <- c("  A  ", "  B  ", "  C  ", "  D  ", "  E  ","  F  ", "  G  ", "  H  ")
	if(interactive())
	{
		answer <- readline("Detailed results ?")
		if(answer == "y")
		{	
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
	
		if(interactive())
		{
			answer <- readline("Return matrix?")
			if(answer == "y")
			{
				#return unsorted data
				return
				{step3.unsorted}
			} else{}
		}
}

question.e <- function(num)
{
	unsorted <- matrix( 0 , nrow = 8  ,ncol = num)
	#generate theta.j
	for(i in 1:8)
	{
		unsorted[i,] <- rnorm(num, mean = y[i], sd = sqrt(sigma[i]))
	}
	return
	{
		unsorted
	}
}

question.f <- function(num)
{
	
	unsorted <- matrix( 0 , nrow = 8  ,ncol = num)
	for(i in 1:8)
	{
		mu <- rnorm(num, mean = sum(y / sigma) / sum(1/sigma) , sd = sqrt(1/ sum(1/sigma)))
		unsorted[i,] <- rnorm(num, mean = mu, sd = 0)
	}
	return
	{
		unsorted
	}
}


best.coaching <- function(num)
{
	ma <- matrix(0 , nrow = 8 , ncol  = 8)
	
	
	if(interactive())
	{
		answer <- readline("Which question ? D or E or F? ")
		if(answer == "d")
		{
			step3.unsorted <- main.method(num)
		} else if(answer == "e")
		{
			step3.unsorted <- question.e(num)
		} else if(answer == "f")
		{
			step3.unsorted <- question.f(num)
		} else
		{
			print("Incorrect input. Please start again")
			best.coaching(num)
		}
	}

	maxi <- apply(step3.unsorted, 2, max)
	for(i in 1:8)
	{
		
		cat("School ", i, "against others\n")
		cat("__________________________________", "\n")
		for(j in 1:8)
		{
			success <- 0
			
			for(k in 1:num)
			{
				if(i != j )
				{
					if(step3.unsorted[i,k] > step3.unsorted[j,k])
					{
						success <- success + 1
					}	
				}
			}
			prob <- success / num
			if(i != j )
			{
				ma[i,j] <- prob
			} else
			{
				ma[i,j] <- c("\\")
			}
			
			cat("School", i ," >", j,": ",prob , " or ", success, "/", num, "\n")
		}
		cat("\n\n")
	}
	
	print(ma)
	cat("\n\n")
	
	for( j in 1: 8)
	{
		best <- 0
		for( k in 1:num)
		{	
			if(step3.unsorted[j,k] == maxi[k])
			{
				best <- best + 1			
			}
		}
		best.prob <- best / num
		cat("School ", j, "prob of being best coaching program: \n " )
		cat("---------------------------------------", "\n")
		cat(best.prob , "or ", best," / " , num , "\n\n")
	}
}
