#!/usr/bin/env Rscript

#newton method
	
#Newton Method
#w_i
wi <- function(bac, t, r){
	vec <- c()
	for(i in 1:length(bac)){
		k <- 0
		for(j in 1:i){
			k <- k + bac[j] * exp(-r * (t[i]- t[j]))
		}
		vec <- append(vec,k)
	}
	return(vec)	
}

#dw_i/dr
dw.dr <- function(bac,t,r){
	vec <- c()
	for(i in 1:length(bac)){
		k <- 0
		for(j in 1:i){
			k <- k + ( bac[j] * exp(-r * (t[i]- t[j])) *(-1)*(t[i] - t[j]))
		}
		vec <- append(vec, k)
	}
	return(vec)		
}

#d^2 x_i(r) / dr^2 second derivative of dx/dr
d2x.dr2 <- function(bac,t,r){
	#call in vectors of dw/dr , w_i 
	w <- wi(bac,t,r)
	dwdr <- dw.dr(bac,t,r)
	vec <- c()
	for(i in 1:length(bac)){
		kk <- 0		
		for(j in 1:i){
			kk <- kk + (bac[j]* ((t[i] - t[j])^2 ) * exp(-r*(t[i]-t[j])))
		}
		element <- ((-1/log(10))*(w[i]^(-2))*(dwdr[i])^2)+((1/log(10))*(1/w[i])*kk)
		vec <- append(vec, element)	
	}
	return(vec)	
} 

#score function
score <- function(y,bac,t,r, alpha,beta){
	#vector of w_i
	w <- wi(bac,t,r)
	#vector of dw_i /dr
	dwdr <- dw.dr(bac,t,r)
	#vector of x_i(r)
	xr <- log(w,base = 10)
	#vector of dx_i/dr
	dxdr <- (1/log(10))*(1/w)*dwdr
	s <- matrix(,nrow = 3)
	s[1,1] <- sum(y - alpha - beta*xr) * (-2)
	s[2,1] <- sum( (y - alpha - beta*xr) * xr)*(-2) 
	s[3,1] <- sum( (y - alpha - beta*xr) * dxdr) *(-2) * (beta)
	return(s)
}
#second derivative - hessian for newton raphson
second.d <- function(y,bac,t,r,alpha,beta){
	#vector of w_i
	w <- wi(bac,t,r)
	#vector of dw_i /dr
	dwdr <- dw.dr(bac,t,r)
	#vector of x_i(r)
	xr <- log(w,base = 10)
	#vector of dx_i/dr
	dxdr <- (1/log(10))*(1/w)*dwdr
	#vector of 2nd derivative: d^2x/dr^2
	ddxddr <- d2x.dr2(bac,t,r)
	#the matrix
	sd <- matrix(,nrow = 3, ncol = 3)
	sd[1,1] <- 2 * length(bac)
	sd[2,2] <- 2 * sum(xr^2)
	sd[3,3] <- (-2)*beta* ( sum((-beta)*dxdr*dxdr) + sum((y-alpha-beta*xr)*ddxddr) )
	sd[1,2] <- 2* sum(xr)
	sd[2,1] <- sd[1,2]
	sd[2,3] <- (-2)*(sum( (y-alpha-beta*xr)*dxdr) + sum((-beta)*dxdr * xr))
	sd[3,2] <- sd[2,3]
	sd[1,3] <- 2*beta * sum(dxdr)
	sd[3,1] <- sd[1,3]
	#lambda <- 0
	#I <- diag(c(1,1,1))
	#while(is.positive.definite(-sd) == FALSE){
	#	lambda <- lambda + 1
	#	sd <- sd - lambda * I
	#}
	return(sd)
}

#scoring matrix - expectation of 2nd derivative (without minus sign so 
#					not expected fisher information) THIS IS A DIFFERENT PROBLEM NOW;
#					maximisation problem !!!! 
fisher.second.d <- function(y,bac,t,r,alpha,beta){
	#vector of w_i
	w <- wi(bac,t,r)
	#vector of dw_i /dr
	dwdr <- dw.dr(bac,t,r)
	#vector of x_i(r)
	xr <- log(w,base = 10)
	#vector of dx_i/dr
	dxdr <- (1/log(10))*(1/w)*dwdr
	#vector of 2nd derivative: d^2x/dr^2
	ddxddr <- d2x.dr2(bac,t,r)
	#the matrix
	sd <- matrix(,nrow = 3, ncol = 3)
	sd[1,1] <- 2 * length(bac)
	sd[2,2] <- 2 * sum(xr^2)
	sd[3,3] <- (-2)*beta* ( sum((-beta)*dxdr*dxdr))
	sd[1,2] <- 2* sum(xr)
	sd[2,1] <- sd[1,2]
	sd[2,3] <- (-2)*( sum((-beta)*dxdr * xr))
	sd[3,2] <- sd[2,3]
	sd[1,3] <- 2*beta * sum(dxdr)
	sd[3,1] <- sd[1,3]
	return
	{
		sd
	}
}



newton.raphson <-function(y,bac,t,r, alpha,beta){
	#theta is vector of parameters (alpha,beta ,r)
	theta <- c(alpha, beta, r)
	theta.iter <- c(alpha.iter <- 0, beta.iter <- 0,r.iter <- r + 0.1)
	theta.iter <- theta - 0.5*solve( second.d(y,bac,t,theta[3],theta[1], theta[2])) %*% score(y,bac,t,theta[3], theta[1],theta[2])
	counter <- 1
	while( (sum(abs(theta.iter - theta)) > 0.0001) && theta.iter[3] <= 30 && theta.iter[3] >= 0 )
	{
		theta <- theta.iter
		theta.iter <- theta - 0.5*solve( second.d(y,bac,t,theta[3],theta[1] ,theta[2])) %*% score(y,bac,t,theta[3], theta[1],theta[2])
		counter <- counter + 1
		#cat("iter: ",counter,"\t", "param: ",theta.iter, "\n")		
		
		if(any(is.na(theta.iter)))
		{
			browser()
		}		
	}
	theta <- theta.iter
	if(theta[3] < 0 || is.na(theta[3])== T)
	{
		theta[3] <- 0
	}	
	#vector of w_i
	w <- wi(bac,t,theta[3])

	#vector of x_i(r)
	xr <- log(w,base = 10)
	reg <- lm(y ~ xr)
	halpha <- as.double(coef(reg)[1])
	hbeta <- as.double(coef(reg)[2])
	aa <- summary(reg)
	cat("iter: ",counter <- counter + 1,"\n")
	cat("param: ","\n") 
	print(rbind(c(halpha,hbeta,theta[3]),c(theta)))
	cat(" sigma: ",aa$sigma, "\n")
	return(
		# hat{alpha}, hat{beta}, sigma, R^2, p-values, r
		c(halpha,hbeta, aa$sigma, aa$r.squared, aa$coefficients[2,4] ,theta[3])
	)
}
