print.logistic4p <- function(x, ...){
	converged <- x$converged
	if (converged){
		n.iter=x$n.iter
		loglike=x$loglike
		AIC=x$AIC
		BIC=x$BIC
		estimates=x$estimates
		cat("The algorithm converged in ", n.iter, " iterations.\n")
		cat("LogLikelihood = ", loglike, "\n")
		cat("AIC = ", AIC, "   BIC= ", BIC, "\n\n")
		cat("Parameter estimates: \n")
		print(estimates)
	}else{
		cat('Warning: The algorithm does not converge after', x$n.iter, 'iterations.\n')
		cat('The current parameter estimates are ', x$estimates, '\n')
		cat('Please increase the maximum of iterations, change the starting value or fit a different model.\n')
	}
}

logistic <- function(x, y, initial, max.iter=1000, epsilon=1e-6, detail=FALSE){
  x <- as.matrix(cbind(1,x))  ## Add intercept
  y <- as.matrix(y) 
  p <- ncol(x)
  
  ## check for initial values
  if (missing(initial)){
    gamma0 <- rep(0, p)
  }else{
    gamma0 <- as.matrix(initial)
  }
  
  n.iter <- 0
  d <- 10
  
  while (d > epsilon && n.iter < max.iter){
    F <- 1/(1+exp(-x%*%as.matrix(gamma0)))
    p.i <- F
    
    W <- diag(as.vector((1+exp(x%*%as.matrix(gamma0)))^2/(exp(x%*%as.matrix(gamma0)))))
    D <- cbind(as.vector(F*(1-F))*x)
    gamma1 <- try(ginv(t(D)%*%W%*%D)%*%t(D)%*%W%*%(as.matrix(y-p.i)+D%*%as.matrix(gamma0)),silent=TRUE)
    
    if(class(gamma1)!='try-error'){
      d <- max(abs(gamma1-gamma0))
      gamma0 <- gamma1 
      n.iter <- n.iter + 1
    }else{
      break
    }
    
    if (detail) cat(c(n.iter), c(gamma0), c(d), "\n")
  }
  
  if(n.iter < max.iter && d < epsilon){
    e.se <- sqrt(diag(solve(t(D)%*%W%*%D)))
    t.stat <- gamma0/e.se
    p.value <- 2*(1-pnorm(abs(t.stat)))
    F <- exp(x%*%as.matrix(gamma0))/(1+exp(x%*%as.matrix(gamma0)))
    p.i <- F
    ID <- c(which(p.i==0), which(p.i==1))
    
    if(length(ID!=0)){
      theta <- log(p.i[-ID])-log(1-p.i[-ID])
      loglike <- sum(y[-ID]*theta-log(1+exp(theta)))
      cat('Warning message:\n fitted probabilities numerically 0 or 1 occurred .\n')
    }else{
      theta <- log(p.i)-log(1-p.i)
      loglike <- sum(y*theta-log(1+exp(theta)))
    }
    
    
    estimates <- cbind(gamma0, e.se, t.stat, p.value)
    colnames(estimates) <- c('Estimates', 'Std.Error', 'z.value', 'Pr(>|z|)')
    rownames(estimates) <- c('Intercept', row.names(estimates)[-1])
    AIC=-2*loglike+2*ncol(x)
    BIC=-2*loglike+log(nrow(x))*(ncol(x)+0)
    
	converged <- TRUE
    structure(list(estimates=estimates, n.iter=n.iter, d=d, loglike=loglike, AIC=AIC, BIC=BIC, converged=converged), class="logistic4p")

    
  }else{
	converged <- FALSE
    cat('Warning: The algorithm does not converge after', n.iter, 'iterations.\n')
    cat('The current parameter estimates are ', gamma0, '\n')
    cat('Please increase the maximum of iterations, change the starting value or fit a different model.\n')
	names(gamma0) <- c("Intercept", names(x))
	structure(list(n.iter=n.iter, estimates=gamma0, converged=converged), class="logistic4p")
  }
}




logistic4p.fp.fn<-function(x, y, initial, max.iter=1000, epsilon=1e-6, detail=FALSE){
  x <- as.matrix(cbind(1,x))  ## Add intercept
  y <- as.matrix(y)  
  
  ## check for initial values
  if (missing(initial)){
    m=logistic(x[, -1], y)$estimates[, 1]
    gamma0 <- as.matrix(c(0, 0, m))
  }else{
    gamma0 <- as.matrix(initial)
  }
  
  n.iter <- 0
  d <- 10
  
  while (d > epsilon && n.iter < max.iter){
    F <- 1/(1+exp(-x%*%as.matrix(gamma0[-c(1:2)])))
    pi <- gamma0[1]+(1-gamma0[1]-gamma0[2])*F
    #W <- diag(1/as.vector(pi*(1-pi)))
    W=diag(as.vector((1+exp(x%*%as.matrix(gamma0[-c(1:2)])))^2/(gamma0[1]+(1-gamma0[2])*exp(x%*%as.matrix(gamma0[-c(1:2)])))/(1-gamma0[1]+gamma0[2]*exp(x%*%as.matrix(gamma0[-c(1:2)])))))
    D <- cbind(1-F, -F, as.vector((1-gamma0[1]-gamma0[2])*F*(1-F))*x)
    gamma1 <- try(ginv(t(D)%*%W%*%D)%*%t(D)%*%W%*%(as.matrix(y-pi)+D%*%as.matrix(gamma0)),silent=TRUE)
    
    if(class(gamma1)!='try-error'){
      d <- max(abs(gamma1-gamma0))
      gamma0=gamma1
      n.iter <- n.iter + 1
    }else{break}
    
    if (detail) cat(c(n.iter), c(gamma0), c(d), "\n")
  }
  if(n.iter< max.iter && d<epsilon){
    e.se <- sqrt(diag(solve(t(D)%*%W%*%D)))
    t.stat <- gamma0/e.se
    p.value <- 2*(1-pnorm(abs(t.stat))) 
    F=exp(x%*%gamma0[-c(1:2)])/(1+exp(x%*%gamma0[-c(1:2)]))
    pi=gamma0[1]+(1-gamma0[1]-gamma0[2])*F
    ID=c(which(pi==0), which(pi==1))
    if(length(ID!=0)){
      theta=log(pi[-ID])-log(1-pi[-ID])
      loglike=sum(y[-ID]*theta-log(1+exp(theta)))
      cat('Warning message:\n fitted probabilities numerically 0 or 1 occurred .\n')
    }else{
      theta=log(pi)-log(1-pi)
      loglike=sum(y*theta-log(1+exp(theta)))
    }
    
    estimates=cbind(gamma0,e.se, t.stat,p.value)
    colnames(estimates)=c('Estimates', 'Std.Error', 'z.value', 'Pr(>|z|)')
    rownames(estimates)=c('FP', 'FN', 'Intercept', row.names(estimates)[-(1:3)])
    
    AIC=-2*loglike+2*ncol(x)+4
    BIC=-2*loglike+log(nrow(x))*(ncol(x)+2)
    
    converged <- TRUE
    structure(list(estimates=estimates, n.iter=n.iter, d=d, loglike=loglike, AIC=AIC, BIC=BIC, converged=converged), class="logistic4p")

    
  }else{
	converged <- FALSE
    cat('Warning: The algorithm does not converge after', n.iter, 'iterations.\n')
    cat('The current parameter estimates are ', gamma0, '\n')
    cat('Please increase the maximum of iterations, change the starting value or fit a different model.\n')
	names(gamma0) <- c("Intercept", names(x))
	structure(list(n.iter=n.iter, estimates=gamma0, converged=converged), class="logistic4p")
  }
}

logistic4p.fp<-function(x, y, initial, max.iter=1000, epsilon=1e-6, detail=FALSE){
  x <- as.matrix(cbind(1,x))  ## Add intercept
  y <- as.matrix(y) 
  
  ## check for initial values
  if (missing(initial)){
    
    m=logistic(x[, -1], y)$estimates[, 1]
    gamma0 <- as.matrix(c(0, m))
  }else{
    gamma0 <- as.matrix(initial)
  }
  
  n.iter <- 0
  d <- 10
  
  while (d > epsilon && n.iter < max.iter){
    F<-1/(1+exp(-x%*%as.matrix(gamma0[-1])))
    pi<-gamma0[1]+(1-gamma0[1])*F
    
    W=diag(as.vector((1+exp(x%*%as.matrix(gamma0[-1])))^2/(gamma0[1]+exp(x%*%as.matrix(gamma0[-1]))/(1-gamma0[1]))))
    D=cbind(1-F, as.vector((1-gamma0[1])*F*(1-F))*x)
    gamma1=try(ginv(t(D)%*%W%*%D)%*%t(D)%*%W%*%(as.matrix(y-pi)+D%*%as.matrix(gamma0)), silent=TRUE)
    
    if(class(gamma1)!='try-error'){
      d <- max(abs(gamma1-gamma0))
      gamma0 <- gamma1
      
      n.iter <- n.iter + 1
    }else{break}
    
    if (detail) cat(c(n.iter), c(gamma0), c(d), "\n")
  }
  
  if(n.iter< max.iter && d<epsilon){
    e.se <- sqrt(diag(solve(t(D)%*%W%*%D)))
    t.stat <- gamma0/e.se
    p.value <- 2*(1-pnorm(abs(t.stat))) 
    F=exp(x%*%gamma0[-1])/(1+exp(x%*%gamma0[-1]))
    pi=(1-gamma0[1])*F
    ID=c(which(pi==0), which(pi==1))
    if(length(ID!=0)){
      theta=log(pi[-ID])-log(1-pi[-ID])
      loglike=sum(y[-ID]*theta-log(1+exp(theta)))
      cat('Warning message:\n fitted probabilities numerically 0 or 1 occurred .\n')
    }else{
      theta=log(pi)-log(1-pi)
      loglike=sum(y*theta-log(1+exp(theta)))
    }
    
    estimates=cbind(gamma0,e.se, t.stat,p.value)
    colnames(estimates)=c('Estimates', 'Std.Error', 'z.value', 'Pr(>|z|)')
    rownames(estimates)=c('FP', 'Intercept', row.names(estimates)[-(1:2)])
    
    AIC=-2*loglike+2*ncol(x)+2
    BIC=-2*loglike+log(nrow(x))*(ncol(x)+1)
    
    converged <- TRUE
    structure(list(estimates=estimates, n.iter=n.iter, d=d, loglike=loglike, AIC=AIC, BIC=BIC, converged=converged), class="logistic4p")

    
  }else{
	converged <- FALSE
    cat('Warning: The algorithm does not converge after', n.iter, 'iterations.\n')
    cat('The current parameter estimates are ', gamma0, '\n')
    cat('Please increase the maximum of iterations, change the starting value or fit a different model.\n')
	names(gamma0) <- c("Intercept", names(x))
	structure(list(n.iter=n.iter, estimates=gamma0, converged=converged), class="logistic4p")
  }
}


logistic4p.fn<-function(x, y, initial, max.iter=1000, epsilon=1e-6, detail=FALSE){
  x <- as.matrix(cbind(1,x))  ## Add intercept
  y <- as.matrix(y) 
  ## check for initial values
  if (missing(initial)){
    
    m=logistic(x[, -1], y)$estimates[, 1]
    gamma0 <- as.matrix(c(0, m))
  }else{
    gamma0 <- as.matrix(initial)
  }
  
  n.iter <- 0
  d <- 10
  
  while (d > epsilon && n.iter < max.iter){
    F<-1/(1+exp(-x%*%as.matrix(gamma0[-1])))
    pi<-(1-gamma0[1])*F
    
    W=diag(as.vector((1+exp(x%*%as.matrix(gamma0[-1])))^2/(1-gamma0[1])/exp(x%*%as.matrix(gamma0[-1]))/(1+gamma0[1]*exp(x%*%as.matrix(gamma0[-1])))))
    D=cbind(-F, as.vector((1-gamma0[1])*F*(1-F))*x)
    gamma1=try(ginv(t(D)%*%W%*%D)%*%t(D)%*%W%*%(as.matrix(y-pi)+D%*%as.matrix(gamma0)),silent=TRUE)
    
    if(class(gamma1)!='try-error'){
      d <- max(abs(gamma1-gamma0))
      gamma0 <- gamma1
      
      n.iter <- n.iter + 1
    }else{break}
    
    if (detail) cat(c(n.iter), c(gamma0), c(d), "\n")
  }
  if(n.iter< max.iter && d<epsilon){
    e.se <-sqrt(diag(solve(t(D)%*%W%*%D)))
    t.stat <- gamma0/e.se
    p.value <- 2*(1-pnorm(abs(t.stat))) 
    F=exp(x%*%gamma0[-1])/(1+exp(x%*%gamma0[-1]))
    pi=(1-gamma0[1])*F
    
    ID=c(which(pi==0), which(pi==1))
    if(length(ID!=0)){
      theta=log(pi[-ID])-log(1-pi[-ID])
      loglike=sum(y[-ID]*theta-log(1+exp(theta)))
      cat('Warning message:\n fitted probabilities numerically 0 or 1 occurred .\n')
    }else{
      theta=log(pi)-log(1-pi)
      loglike=sum(y*theta-log(1+exp(theta)))
    }
    
    estimates=cbind(gamma0,e.se, t.stat,p.value)
    colnames(estimates)=c('Estimates', 'Std.Error', 'z.value', 'Pr(>|z|)')
    rownames(estimates)=c('FN', 'Intercept', row.names(estimates)[-(1:2)])
    
    AIC=-2*loglike+2*ncol(x)+2
    BIC=-2*loglike+log(nrow(x))*(ncol(x)+1)
    
    converged <- TRUE
    structure(list(estimates=estimates, n.iter=n.iter, d=d, loglike=loglike, AIC=AIC, BIC=BIC, converged=converged), class="logistic4p")

    
  }else{
	converged <- FALSE
    cat('Warning: The algorithm does not converge after', n.iter, 'iterations.\n')
    cat('The current parameter estimates are ', gamma0, '\n')
    cat('Please increase the maximum of iterations, change the starting value or fit a different model.\n')
	names(gamma0) <- c("Intercept", names(x))
	structure(list(n.iter=n.iter, estimates=gamma0, converged=converged), class="logistic4p")
  }
}


logistic4p.e<-function(x, y, initial, max.iter=1000, epsilon=1e-6, detail=FALSE){
  x <- as.matrix(cbind(1,x))  ## Add intercept
  y <- as.matrix(y) 
  ## check for initial values
  if (missing(initial)){
    
    m=logistic(x[, -1], y)$estimates[, 1]
    gamma0 <- as.matrix(c(0, m))
  }else{
    gamma0 <- as.matrix(initial)
  }
  
  n.iter <- 0
  d <- 10
  
  while (d > epsilon && n.iter < max.iter){
    F<-1/(1+exp(-x%*%as.matrix(gamma0[-1])))
    pi<-gamma0[1]+(1-2*gamma0[1])*F
    
    W=diag(as.vector((1+exp(x%*%as.matrix(gamma0[-1])))^2/(gamma0[1]+(1-gamma0[1])*exp(x%*%as.matrix(gamma0[-1])))/(1-gamma0[1]+gamma0[1]*exp(x%*%as.matrix(gamma0[-1])))))
    D=cbind(1-2*F, as.vector((1-2*gamma0[1])*F*(1-F))*x)
    gamma1=try(ginv(t(D)%*%W%*%D)%*%t(D)%*%W%*%(as.matrix(y-pi)+D%*%as.matrix(gamma0)),silent=TRUE)
    
    if(class(gamma1)!='try-error'){
      d <- max(abs(gamma1-gamma0))
      gamma0 <- gamma1
      
      n.iter <- n.iter + 1
    }else{break}
    
    if (detail) cat(c(n.iter), c(gamma0), c(d), "\n")
  }
  if(n.iter< max.iter && d<epsilon){
    e.se <-sqrt(diag(solve(t(D)%*%W%*%D)))
    t.stat <- gamma0/e.se
    p.value <- 2*(1-pnorm(abs(t.stat))) 
    F=exp(x%*%gamma0[-1])/(1+exp(x%*%gamma0[-1]))
    pi=gamma0[1]+(1-2*gamma0[1])*F
    ID=c(which(pi==0), which(pi==1))
    if(length(ID!=0)){
      theta=log(pi[-ID])-log(1-pi[-ID])
      loglike=sum(y[-ID]*theta-log(1+exp(theta)))
      cat('Warning message:\n fitted probabilities numerically 0 or 1 occurred .\n')
    }else{
      theta=log(pi)-log(1-pi)
      loglike=sum(y*theta-log(1+exp(theta)))
    }
    
    estimates=cbind(gamma0,e.se, t.stat,p.value)
    colnames(estimates)=c('Estimates', 'Std.Error', 'z.value', 'Pr(>|z|)')
    rownames(estimates)=c('FP & FN', 'Intercept', row.names(estimates)[-(1:2)])
    AIC=-2*loglike+2*ncol(x)+2
    
    BIC=-2*loglike+log(nrow(x))*(ncol(x)+1)
    
    converged <- TRUE
    structure(list(estimates=estimates, n.iter=n.iter, d=d, loglike=loglike, AIC=AIC, BIC=BIC, converged=converged), class="logistic4p")

    
  }else{
	converged <- FALSE
    cat('Warning: The algorithm does not converge after', n.iter, 'iterations.\n')
    cat('The current parameter estimates are ', gamma0, '\n')
    cat('Please increase the maximum of iterations, change the starting value or fit a different model.\n')
	names(gamma0) <- c("Intercept", names(x))
	structure(list(n.iter=n.iter, estimates=gamma0, converged=converged), class="logistic4p")
  }
}


logistic4p<-function (x, y, initial, model=c('lg', 'fp.fn','fp', 'fn','equal'), max.iter=1000, epsilon=1e-6, detail=FALSE){
  ## remove missing data before analysis
  if (is.null(nrow(x))){
    check.missing <- is.na(x) + is.na(y) == 0
  }else{
    check.missing <- apply(is.na(x), 1, sum) + is.na(y) == 0
  }
  
  n <- length(y)
  n.miss <- n - sum(check.missing)
  if(n.miss > 0){    
    cat('Warning: ', n.miss, ' subjects contain missing data and are removed before data analysis.\n')
    y <- y[check.missing]
    if (is.null(nrow(x))){
      x <- x[check.missing]
    }else{
      x <- x[check.missing, ]
    }
  }
  
  model <- match.arg(model)
  m <- switch(model, lg=1, fp.fn=2, fp=3, fn=4, equal=5)
  
  x <- as.matrix(x)
  y <- as.matrix(y)
  
  
  if (m==1){
    if (!missing(initial)){
      if(length(initial)!=(ncol(x)+1)){stop('Error: The length of "initial" values must be of the length of the number of parameters in the model.')}
    }
    
    return(logistic(x, y, initial, max.iter, epsilon, detail))
    
  } 
  
  if (m==2){
    if(!missing(initial)){
      if(length(initial)!=(ncol(x)+3)){stop('Error: The length of "initial" values must be of the length of the number of parameters in the model.')}
    }
    return(logistic4p.fp.fn(x, y, initial, max.iter, epsilon, detail))
    
  } 
  
  if (m==3){
    if(!missing(initial)){ if(length(initial)!=(ncol(x)+2)){stop('Error: The length of "initial" values must be of the length of the number of parameters in the model.')}
    }
    return(logistic4p.fp(x, y, initial, max.iter, epsilon, detail))
    
  } 
  
  if (m==4){
    if(!missing(initial)){if(length(initial)!=(ncol(x)+2)){stop('Error: The length of "initial" values must be of the length of the number of parameters in the model.')}
    }
    return(logistic4p.fn(x, y, initial, max.iter, epsilon, detail))
    
  }
  if (m==5){
    if(!missing(initial)){if(length(initial)!=(ncol(x)+2)){stop('Error: The length of "initial" values must be of the length of the number of parameters in the model.')}
    }
    return(logistic4p.e(x, y, initial, max.iter, epsilon, detail))
  }
}


