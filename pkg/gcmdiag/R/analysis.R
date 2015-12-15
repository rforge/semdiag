uniout <- function(data){
	F.l<-quantile(data, .25)
	F.u<-quantile(data, .75)
	d.F<-F.u-F.l

	C.l<-F.l-d.F*1.5
	C.u<-F.u+d.F*1.5
	res <- c(which(data<C.l),which(data>C.u))
	res
}


mdout <- function(data,alpha){
	mu <- cov.rob(data,method="mcd")$center
	sig <- cov.rob(data,method="mcd")$cov
	md2 <- diag(t(t(data)-mu)%*%solve(sig)%*%(t(data)-mu))
	cut <- qchisq((1-alpha/2),4)
	mdo <- as.numeric(which(md2>cut))
	mdo
}

RFRAdiag.summary <- function (d, alpha = 0.025, digits = 2) 
{
    prob <- 1 - alpha
    p <- d$p
    q <- d$q

    critq <- sqrt(qchisq(prob, q))
    critp1 <- sqrt(qchisq(prob, p - q))
    dfi <- d$d_f
    dri <- d$d_r[, 2]
    index <- d$d_r[, 1]
    x <- d$x
    l.o.index <- index[(dri > critp1) & (dfi > critq)]
    o.index <- index[(dri > critp1) & (dfi <= critq)]
    l.index <- index[(dri <= critp1) & (dfi > critq)]
	out1 <- NULL
	out2 <- NULL
	out3 <- NULL
    if (length(l.o.index) >= 1) {
        cat("Leverage observations and outliers =", rownames(x)[l.o.index], 
            "\n")
	  out1 <- c(out1,rownames(x)[l.o.index])
    }
    else {
        cat("No case is both outlier and leverage observation. \n")
    }
    if (length(l.index) >= 1) {
        cat("Leverage observations not outliers =", rownames(x)[l.index], 
            "\n")
	    out2 <- c(out2,rownames(x)[l.index])

    }
    else {
        cat("No case is just leverage observation. \n")
    }
    if (length(o.index) >= 1) {
        cat("Outliers not leverage observations =", rownames(x)[o.index], 
            "\n")
	  out3 <- c(out3,rownames(x)[o.index])

    }
    else {
        cat("No case is just outliers. \n")
    }
    if (d$software == "EQS") {
        fit <- rbind(cbind(d$res$nml$chi, d$res$tsr$chi, d$res$dr$chi), 
            cbind(d$res$nml$df, d$res$tsr$df, d$res$dr$df), cbind(d$res$nml$pval, 
                d$res$tsr$pval, d$res$dr$pval))
        colnames(fit) <- c("NML", "TSR", "DR")
        rownames(fit) <- c("Statistics", "df", "p-value")
        estimates <- cbind(d$res$nml$par, d$res$tsr$par, d$res$dr$par)
        estimates <- round(estimates, digits = digits)
        estimates <- format(estimates, digits = digits)
        row0 <- c("", "NML", "", "", "TSR", "", "", "DR", "")
        row1 <- c("Est.", "S.E.", "z", "Est.", "S.E.", "z", "Est.", 
            "S.E.", "z")
        estimates <- rbind(row1, estimates)
        rownames(estimates)[1] <- c("Label")
        colnames(estimates) <- row0 <- c("", "NML", "", "", "TSR", 
            "", "", "DR", "")
        cat("\nModel fit comparison\n")
        print(fit)
        cat("\nParameter estimates\n")
        print(estimates, quote = FALSE, right = TRUE)
        cat("\nNote.\n")
        cat("  NML = Normal ML\n")
        cat("  TSR = Two-stage robust method\n")
        cat("  DR  = Direct robust method\n")
        cat("  Est.= Parameter estimates\n")
        cat("  S.E.= Standard error\n")
        cat("  z   = Z-score\n")
        invisible(list(fit = fit, estimates = estimates))
    }
    else {
        chisq1 <- d$res$nml$chisq
        chisq2 <- d$res$tsr$chisq
        chisq3 <- d$res$dr$chisq
        df1 <- d$res$nml$df
        df2 <- d$res$tsr$df
        df3 <- d$res$dr$df
        pvalue1 <- 1 - pchisq(chisq1, df1)
        pvalue2 <- 1 - pchisq(chisq2, df2)
        pvalue3 <- 1 - pchisq(chisq3, df3)
        fit <- rbind(cbind(chisq1, chisq2, chisq3), cbind(df1, 
            df2, df3), cbind(pvalue1, pvalue2, pvalue3))
        fit <- round(fit, 4)
        colnames(fit) <- c("NML", "TSR", "DR")
        rownames(fit) <- c("Statistics", "df", "p-value")
        estimates <- cbind(d$res$nml$coeff[, 1:3], d$res$tsr$coeff[, 
            1:3], d$res$dr$coeff[, 1:3])
        estimates <- round(estimates, digits = digits)
        estimates <- format(estimates, digits = digits)
        estimates <- as.matrix(estimates)
        row0 <- c("", "NML", "", "", "TSR", "", "", "DR", "")
        row1 <- c("Est.", "S.E.", "z", "Est.", "S.E.", "z", "Est.", 
            "S.E.", "z")
        estimates <- rbind(row1, estimates)
        rownames(estimates)[1] <- c("Label")
        colnames(estimates) <- row0 <- c("", "NML", "", "", "TSR", 
            "", "", "DR", "")
        cat("\nModel fit comparison\n")
        print(fit)
        cat("\nParameter estimates\n")
        print(estimates, quote = FALSE, right = TRUE)
        cat("\nNote.\n")
        cat("  NML = Normal ML\n")
        cat("  TSR = Two-stage robust method\n")
        cat("  DR  = Direct robust method\n")
        cat("  Est.= Parameter estimates\n")
        cat("  S.E.= Standard error\n")
        cat("  z   = Z-score\n")
        invisible(list(fit = fit, estimates = estimates))
    }
    list(out1,out2,out3)
}

gcmdiag <- function(data,method,lgcm, alpha=0.025){

	y <- as.matrix(data)
	T <- ncol(y)
	N <- nrow(y)

	x <- cbind(rep(1,T),0:(T-1))	
	var <- NULL
	for (t in 1:T){
		var <- c(var,paste('y',t,sep=''))
	}
	colnames(y) <- var

	if(method == 'UD'){
		UDvar <- NULL
		for (t in 1:T){
			UDvar <- c(UDvar,uniout(y[,t]))
		}		
		m1 <- sort(UDvar)
		m1.o <- as.numeric(unique(m1))
		cat("Univariate Detection\n")
		cat("Identified outlying observations:\n")
		cat(m1.o,'\n')
		ID <- list(m1.o)
		names(ID) <- c('OutlyingObs')
		ID



	}else if (method == 'MD-SMD'){
	
		m2.o <- mdout(y,alpha)
		cat("Multivariate detection based on robust Mahalanobis squared distances\n")
		cat("Identified outlying observations:\n")
		cat(m2.o,'\n')
		ID <- list(m2.o)
		names(ID) <- c('OutlyingObs')
		ID


	}else if (method == 'MD-IGA'){
	
		res <- matrix(NA,N,T)
		b <- matrix(NA,N,2)
		for(i in 1:N){
			b[i, ] <- solve(t(x)%*%x)%*%t(x)%*%y[i,]
			res[i,] <- y[i,]-x%*%b[i,]
		}
	
		ind <- which(eigen(cov(res))$values<1e-6)
		eigvec <- eigen(cov(res))$vectors[,ind]
		A <- semdiag.orthog(eigvec)
		nres <- t(A)%*%t(res)

		m3.o <- mdout(t(nres),alpha)
		m3.l <- mdout(b,alpha)
		cat("Multivariate detection based on individual-level growth analysis\n")
		cat("Identified outliers:\n")
		cat(m3.o,'\n')
		cat("Identified leverage observations:\n")
		cat(m3.l,'\n')		
		ID <- list(m3.o,m3.l)
		names(ID) <- c('Outliers','LeverageObs')
		ID


	}else if (method == 'NR-FRA'){
			
		S <- rawMoments(y)

		lgcm.est <- sem(lgcm,S,N,raw=T)
	
		psi <- diag(lgcm.est$coeff[(length(lgcm.est$coeff)-T+1):length(lgcm.est$coeff)])
		fs <- solve(t(x)%*%solve(psi)%*%x)%*%t(x)%*%solve(psi)%*%t(y)
		fs <- t(fs)

		ym <- x%*%t(fs)
		resid <- y-t(ym)

		ind <- which(eigen(cov(resid))$values<1e-6)
		eigvec <- eigen(cov(resid))$vectors[,ind]
		A <- semdiag.orthog(eigvec)
		nres <- t(A)%*%t(resid)

		m4.o <- mdout(t(nres),alpha)
		m4.l <- mdout(fs,alpha)
		cat("Non-robust model-based latent factor and residual analysis\n")
		cat("Identified outliers:\n")
		cat(m4.o,'\n')
		cat("Identified leverage observations:\n")
		cat(m4.l,'\n')		
		ID <- list(m4.o,m4.l)
		names(ID) <- c('Outliers','LeverageObs')
		ID

	}else if(method == 'R-FRA'){

		yout.1<-try(semdiag(y, ram.path=lgcm, max_it = 10000,software='sem'))
		if (class(yout.1)!='try-error'){
		out <- RFRAdiag.summary(yout.1)
		m5.o <- as.numeric(c(out[[3]],out[[1]]))
		m5.l <- as.numeric(c(out[[2]],out[[1]]))
		}

		cat("Robust model-based latent factor and residual analysis\n")
		cat("Identified outliers:\n")
		cat(m5.o,'\n')
		cat("Identified leverage observations:\n")
		cat(m5.l,'\n')		
		ID <- list(m5.o,m5.l)
		names(ID) <- c('Outliers','LeverageObs')
		ID

	}else if (method == 'MST'){

		m <- 2
		r <- 1

		z <- t(rep(1,N))
		pz <- t(z)%*%solve(z%*%t(z))%*%z
		S <- t(y)%*%(diag(N)-pz)%*%y
		E <- t(y)%*%(diag(N)-pz)
		M <- x%*%solve(t(x)%*%S%*%x)%*%t(x)

		T <- rep(NA,N)

		for(i in 1:N){
			pii <- pz[i,i]
			ei <- E[,i]
			T[i] <- (t(ei)%*%M%*%ei)/(1-pii)
		}
		TT <- sort(T,decreasing=TRUE)
		Tindex <- order(T,decreasing=TRUE)
	
		cf <- qf(.975,m,N-r-m)
		cv <- m*cf/(N-r-m+m*cf)

		m6.o <- Tindex[which(TT>=cv)]

		m6.o <- sort(m6.o)
		cat("Mean shift testing\n")
		cat("Identified outlying observations:\n")
		cat(m6.o,'\n')
		ID <- list(m6.o)
		names(ID) <- c('OutlyingObs')
		ID

	
	}else{cat('The method you entered is not available.\n')}
}


