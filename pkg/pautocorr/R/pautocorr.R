##----------------------------------------------##
## Partial Autocorrelation test by Zijun Ke & Johnny Zhang
##-------------------------------------------------##
# This is a version allowing missing values
# Complete-data analysis is performed.
# 1/sqrt(T) is modified
pautocorr.test <- function(x,lagmax=3,
		method='all',alpha = 0.05,L=30,B=2000, digits=3,
		print = FALSE){
		
	if (missing(x)){
		stop("Error: Data needed")
	}
	if(sum(is.na(x))>0){
		cat('Warning: Data Contain Missing Values. Results may not be valid.\n')
	}
	x = as.matrix(x)
	nP = ncol(x)
	nT = nrow(x)
	
	if (nP>1) cat("Warning: Only the first variable is tested. \n")
	x = x[, 1, drop=FALSE]
	nT.narm = nrow(na.omit(x))	
	nT.miss = get.T(x,lagmax)

	zcrit = qnorm(1-alpha/2)
	names = paste("Lag",1:lagmax,sep='')
	acf.est<-acf(x,lag.max = lagmax,plot=F,na.action = na.pass)$acf[2:(lagmax+1)]
	pacf.est<-acf(x,lag.max = lagmax,plot=F,na.action = na.pass,type='partial')$acf[1:lagmax]
	
	acf.res = vector('list','4')
	pacf.res = vector('list','3')
	names(acf.res) = c('Asymptotic','Bartlett','Surrogate','VMB')
	names(pacf.res) = c('Asymptotic','Surrogate','VMB')
	
	if (length(grep(method,c("asymptotic",'all')))>0){
		se = sqrt(1/nT.miss)
		z.stat = abs(acf.est/se)
		sig		<- (z.stat > zcrit)
		acf.res$Asymptotic = cbind(acf.est,rep(se,lagmax/length(se)),z.stat,sig)
		z.stat = abs(pacf.est/se)
		sig		<- (z.stat > zcrit)
		pacf.res$Asymptotic = cbind(pacf.est,rep(se,lagmax/length(se)),z.stat,sig)
		colnames(acf.res$Asymptotic)=c('Est.','S.E.','z.stat','Sig.')
		colnames(pacf.res$Asymptotic)=c('Est.','S.E.','z.stat','Sig.')
	}
	if (length(grep(method,c("surrogate",'all')))>0){
		ahat	<- JN.NB(x,lagmax)
		res	<- Surrogate(ahat,x,alpha/2,1-alpha/2,B,lagmax)
		CI	<- res$acf$CI
		sig = cbind(Test.CI(CI$per,acf.est),Test.CI(CI$BCa,acf.est))
		acf.res$Surrogate = cbind(acf.est,CI$per,sig[,1],CI$BCa,sig[,2])
		colnames(acf.res$Surrogate)=c('Est.',paste((1-alpha)*100,c('%CI LB','%CI UB'),sep=''),
		'Sig.',paste('BCa ',(1-alpha)*100,c('%CI LB','%CI UB'),sep=''),'BCa Sig.')
		CI	<- res$pacf$CI
		sig = cbind(Test.CI(CI$per,pacf.est),Test.CI(CI$BCa,pacf.est))
		pacf.res$Surrogate = cbind(pacf.est,CI$per,sig[,1],CI$BCa,sig[,2])
		colnames(pacf.res$Surrogate)=c('Est.',paste((1-alpha)*100,c('%CI LB','%CI UB'),sep=''),
		'Sig.',paste('BCa ',(1-alpha)*100,c('%CI LB','%CI UB'),sep=''),'BCa Sig.')
	}
	if (length(grep(method,c("vmb",'all')))>0){
		blocksize = round(nT^(1/3))
		ahat	<- JN.VMB(x,lagmax,blocksize)
		res	<- VMB(acf.est,pacf.est,ahat,x,blocksize,alpha/2,1-alpha/2,B,lagmax)
		CI	<- res$acf$CI
		sig = cbind(Test.CI(CI$per,0),Test.CI(CI$BCa,0))
		acf.res$VMB = cbind(acf.est,CI$per,sig[,1],CI$BCa,sig[,2])
		colnames(acf.res$VMB)=c('Est.',paste((1-alpha)*100,c('%CI LB','%CI UB'),sep=''),
		'Sig.',paste('BCa ',(1-alpha)*100,c('%CI LB','%CI UB'),sep=''),'BCa Sig.')
		CI	<- res$pacf$CI
		sig = cbind(Test.CI(CI$per,0),Test.CI(CI$BCa,0))
		pacf.res$VMB = cbind(pacf.est,CI$per,sig[,1],CI$BCa,sig[,2])
		colnames(pacf.res$VMB)=c('Est.',paste((1-alpha)*100,c('%CI LB','%CI UB'),sep=''),
		'Sig.',paste('BCa ',(1-alpha)*100,c('%CI LB','%CI UB'),sep=''),'BCa Sig.')
	}
	if (length(grep(method,c("bartlett",'all')))>0){
		se = BartlettSE(x,L,lagmax)
		CI = cbind(acf.est-zcrit*se, acf.est+zcrit*se)
		sig = Test.CI(CI,0)
		acf.res$Bartlett = cbind(acf.est,CI,sig)
		colnames(acf.res$Bartlett)=c('Est.',paste((1-alpha)*100,c('%CI LB','%CI UB'),sep=''),'Sig.')
		rownames(acf.res$Bartlett) = names
	}
	

	if(print == TRUE){
		acf.res = lapply(acf.res,function(x){
				if(is.null(x)==0){
					rownames(x) = names
					round(x, digits)
				}})
		pacf.res = lapply(pacf.res,function(x){
				if(is.null(x)==0){
					rownames(x) = names
					round(x, digits)
				}})
		if(method == 'all'){
			print('ACF Results')
			print(acf.res)
			print('PACF Results')
			print(pacf.res)
		}else{
			print('ACF Results')
			tmp = lapply(acf.res,function(x){ 
				if(is.null(x)==0){
					print(x)
				}})
			print('PACF Results')
			tmp = lapply(pacf.res,function(x){ 
				if(is.null(x)==0){
					print(x)
				}})
		}
	}
	output = list(acf.res=acf.res, pacf.res=pacf.res)
	class(output) = "pautocorr"
	invisible(output)
}

##-----------------------------------------------##
##   Plot Results     
##-----------------------------------------------##

plot.pautocorr <- function(x,alpha = 0.05,
	layout.v = c(2,2),
	legendpos = list(A='bottomright',B ='topleft'),...){
	acf.res = x$acf.res
	pacf.res = x$pacf.res
	lagmax = nrow(acf.res$Asymptotic)

	par(mfrow = layout.v)
	# hypothesis testing under the null
	yl = c(min(acf.res$Surrogate[,-c(4,7)])-0.2,max(acf.res$Surrogate[,-c(4,7)])+0.1)
	plot(acf.res$Asymptotic[,'Est.'],type = 'h',
		ylim = yl,ylab = 'ACF',xlab = 'Lag')
	abline(h=1.96*acf.res$Asymptotic[1,2],col = 'red',lty = 2)
	abline(h=-1.96*acf.res$Asymptotic[1,2],col = 'red',lty = 2)
	lines(1:lagmax,acf.res$Surrogate[,paste((1-alpha)*100,'%CI LB',sep='')],col = 'blue',lty = 2)
	lines(1:lagmax,acf.res$Surrogate[,paste((1-alpha)*100,'%CI UB',sep='')],col = 'blue',lty = 2)
	lines(1:lagmax,acf.res$Surrogate[,paste('BCa ',(1-alpha)*100,'%CI LB',sep='')],col = 'green',lty = 2)
	lines(1:lagmax,acf.res$Surrogate[,paste('BCa ',(1-alpha)*100,'%CI UB',sep='')],col = 'green',lty = 2)

	yl = c(min(pacf.res$Surrogate[,-c(4,7)])-0.2,max(pacf.res$Surrogate[,-c(4,7)])+0.1)		
	plot(pacf.res$Asymptotic[,'Est.'],type = 'h',
		ylim = yl,
		ylab = 'PACF',xlab = 'Lag')
	abline(h=1.96*pacf.res$Asymptotic[1,2],col = 'red',lty = 2)
	abline(h=-1.96*pacf.res$Asymptotic[1,2],col = 'red',lty = 2)
	lines(1:lagmax,pacf.res$Surrogate[,paste((1-alpha)*100,'%CI LB',sep='')],col = 'blue',lty = 2)
	lines(1:lagmax,pacf.res$Surrogate[,paste((1-alpha)*100,'%CI UB',sep='')],col = 'blue',lty = 2)
	lines(1:lagmax,pacf.res$Surrogate[,paste('BCa ',(1-alpha)*100,'%CI LB',sep='')],col = 'green',lty = 2)
	lines(1:lagmax,pacf.res$Surrogate[,paste('BCa ',(1-alpha)*100,'%CI UB',sep='')],col = 'green',lty = 2)
	if(is.numeric(legendpos$A)){
		legend(legendpos$A[1],legendpos$A[2],
		c('1/T','Surrogate','Surrogate-BCa'),
		col = c('red','blue','green'),lty = rep(2,3),cex = 0.7,bty = 'n')
	}else{
		legend(legendpos$A,c('1/T','Surrogate','Surrogate-BCa'),
		col = c('red','blue','green'),lty = rep(2,3),cex = 0.7,bty = 'n')
	}

	
	# the alternative
	yl = c(min(acf.res$Bartlett[,-c(4)])-0.2,max(acf.res$Bartlett[,-c(4)])+0.1)
	plot(acf.res$Asymptotic[,'Est.'],type = 'n',
			ylim = yl,ylab = 'ACF',xlab = 'Lag')
	abline(h = 0)
	for(li in 1:lagmax){
		lines(rep(li,2),acf.res$VMB[li,paste((1-alpha)*100,c('%CI LB','%CI UB'),sep='')],
			col = 'red',lty = 2,type = 'b')	
		lines(rep(li+0.1,2),acf.res$VMB[li, paste('BCa ',(1-alpha)*100,c('%CI LB','%CI UB'),sep='')],
			col = 'blue',lty = 2,type = 'b')
		try(lines(rep(li+0.2,2),acf.res$Bartlett[li,paste((1-alpha)*100,c('%CI LB','%CI UB'),sep='')],
			col = 'green',lty = 2,type = 'b'))
	}
	
	yl = c(min(pacf.res$VMB[,-c(4,7)])-0.2,max(pacf.res$VMB[,-c(4,7)])+0.1)
	plot(pacf.res$Asymptotic[,'Est.'],type = 'n',
			ylim = yl,ylab = 'PACF',xlab = 'Lag')
	abline(h = 0)
	for(li in 1:lagmax){
		lines(rep(li,2),pacf.res$VMB[li,paste((1-alpha)*100,c('%CI LB','%CI UB'),sep='')],
			col = 'red',lty = 2,type = 'b')	
		lines(rep(li+0.1,2),pacf.res$VMB[li, paste('BCa ',(1-alpha)*100,c('%CI LB','%CI UB'),sep='')],
			col = 'blue',lty = 2,type = 'b')
	}
	if(is.numeric(legendpos$B)){
		legend(legendpos$B[1],legendpos$B[2],
		c('VMB','VMB-BCa','Bartlett'),
		col = c('red','blue','green'),lty = rep(2,3),cex = 0.7,bty = 'n')
	}else{
		legend(legendpos$B,c('VMB','VMB-BCa','Bartlett'),
		col = c('red','blue','green'),lty = rep(2,3),cex = 0.7,bty = 'n')
	}


}

##-----------------------------------------------##
##   Acceleration Factor      
##-----------------------------------------------##
get.ahat <- function(dv){
	dv = na.omit(dv)
	dv = as.vector(dv)
	dv.c = mean(dv,na.rm = TRUE)- dv
	dv3 =t(dv.c)%*%(dv.c^2)
	sd.dv = sqrt(t(dv.c)%*%dv.c)
	a = dv3/6/(sd.dv^3)
	return(a)
}

##-----------------------------------------------##
##   get T for asymptotic method with missing values
##-----------------------------------------------##
get.T <- function(x,lagmax){
	Tall = length(x)
	T.miss = rep(NA,lagmax)
	for(li in 1:lagmax){
		data = cbind(x[1:(Tall-li)],x[(1+li):(Tall)])
		T.miss[li] = nrow(na.omit(data))+li
	}
	return(T.miss)
}


##-----------------------------------------------##
##   Obtain PACF from ACF
##	Shumway's book, P113
##	Tested yielding identical results as the built-in acf function   
##-----------------------------------------------##
Durbin_Levinson_rho <- function(rho){
	lagmax = length(rho)
	phi = rep(NA,lagmax)
	phi[1] = rho[1]
	phi.lk = vector("list",lagmax+1)
	phi.lk[[1]] = 0
	phi.lk[[2]] = as.vector(rho[1])
	for(l in 2:lagmax){
		phi.lk[[l+1]] = rep(NA,l)
		if(l>2){
		for(k in 1:(l-2)){
			phi.lk[[l]][k] = phi.lk[[l-1]][k] - phi.lk[[l]][l-1]*phi.lk[[l-1]][l-1-k]
		}
		}
		num = rho[l] - phi.lk[[l]][1:(l-1)]%*%rho[(l-1):1]
		den = 1-phi.lk[[l]][1:(l-1)]%*%rho[1:(l-1)]
		phi.lk[[l+1]][l] = num / den
		phi[l] = num/den
	}
	return(phi)
}

##------------------------------##
## Bartlett's method
##------------------------------##
Bartlett_Shumway <- function(x,rho,L,T){
	l = x
	tmp = 0
	for(li in 1:L){
		tmp2 = (rho[li+l+1] + rho[abs(li-l)+1]-2*rho[l+1]*rho[abs(li)+1])^2
		if(is.na(tmp2)==0){
			tmp = tmp + tmp2
		}
	}
	se = sqrt(tmp/T[l])
	return(se)
}	
BartlettSE <- function(data,L,lagmax){
	#T = length(na.omit(data))
	T = get.T(data,lagmax)
	rho_hat = acf(data,lag.max=L+lagmax,type="correlation",plot=F,na.action = na.pass)$acf[1:(L+lagmax+1)]
	l = matrix(1:lagmax,1,lagmax)
	se = apply(l,2,Bartlett_Shumway,rho=rho_hat,L=L,T = T)
	return(se)
}

##---------------------------------------------------##
##   Jackknife: For Vectorized Moving Block Bootstrap       
##--------------------------------------------------##
JN.VMB<-function(data,lagmax,l){
	n.t	<- length(data)   
	ahat	<- vector('list',2); names(ahat) = c('acf','pacf')                 
	acf.y	<- vector('list',lagmax)	
	pacf.y	<- vector('list',lagmax)	
	for(i in 1:lagmax){
		pair.id = rbind(1:(n.t-i),(1+i):(n.t))
		up = n.t-i-l+1
		tmp = rep(NA,up)
		for(j in 1:up){
			sel = j:(j+l-1)
			x.j<-data[pair.id[1,-sel]]
			y.j<-data[pair.id[2,-sel]]
			tmp1 = try(cor(x.j,y.j,use = 'complete.obs'),silent = TRUE)
			if(inherits(tmp1,'try-error')==0 && is.na(tmp1)==0){
			if((round(tmp1,3) > -1) && (round(tmp1,3) < 1)){
				tmp[j]<-tmp1
			}
			} 
		}
		acf.y[[i]] = tmp
		pacf.y[[i]] = rep(NA,up)
	}
	for(t in 1:(n.t-l)){
		v.acf = unlist(lapply(acf.y,function(x,i) x[i], i = t))
		tmp = Durbin_Levinson_rho(v.acf)
		if(length(na.omit(tmp))>0){
			for(i in 1:length(tmp)){
				pacf.y[[i]][t] = tmp[i]
			}
		}
	}
	ahat$acf = unlist(lapply(acf.y,get.ahat))
	ahat$pacf = unlist(lapply(pacf.y,get.ahat))
	return(ahat)
}

##-----------------------------------------------##
##   Vectorized Moving Block Bootstrap    
##-----------------------------------------------##
pair.VMB <- function(id.M,lagmax,n.t){
	l = nrow(id.M)
	pair.id.M = vector('list',lagmax)
	for(i in 1:lagmax){
		tmp = id.M+i
		tmp2 = rbind(c(id.M),c(tmp))
		sel = which(tmp2[2,]>n.t)
		if(length(sel)>0){tmp2 = tmp2[,-sel]}
		if(ncol(tmp2)>n.t){tmp2 = tmp2[,1:n.t]}
		pair.id.M[[i]] = tmp2
	}
	return(pair.id.M)
}
corr.VMB <- function(pair.id.M,data){
	lagmax = length(pair.id.M)
	lcorr= rep(NA,lagmax)
	n.t = length(data)
	for(i in 1:lagmax){
		x = data[pair.id.M[[i]][1,]]
		y = data[pair.id.M[[i]][2,]]
		tmp = try(cor(x,y,use = 'complete.obs'),silent=TRUE)
		if(inherits(tmp,'try-error')==0 && is.na(tmp)==0){
		if((round(tmp,3) > -1) && (round(tmp,3) < 1)){
			lcorr[i] <- tmp
		}
		} 
	}
	return(lcorr)
}
VMB<-function(acf.est,pacf.est,ahat,data,l,a1,a2,boot,lagmax){
	acf.y<-matrix(NA,nrow=boot,ncol=lagmax)
	pacf.y<-matrix(NA,nrow=boot,ncol=lagmax)
	n.t<-length(data)
	k <- floor(n.t/l)
	num.b<-n.t-l+1 #number of blocks
	seq.b<-seq(1,num.b,by=1)
	odr	<- seq.b %x% t(rep(1,l))
	add	<- rep(1,num.b) %x% t(0:(l-1))
	odr <- odr + add
	for(i in 1:boot){
		for(j in 1:50){
			temp	<- as.vector(sample(seq.b,k+1,replace=TRUE))
			odri	<- t(odr[temp,])
			pair.id.M <- pair.VMB(odri,lagmax,n.t)
			tmp.acf	<- corr.VMB(pair.id.M,data)
			tmp.pacf	<- Durbin_Levinson_rho(tmp.acf)
			if(sum(abs(tmp.pacf>1) + abs(tmp.pacf < (-1)),na.rm = TRUE)==0){break}
		}
		acf.y[i,]	<- tmp.acf
		pacf.y[i,]	<- tmp.pacf
	}
	acf.l <- list(se = apply(acf.y,2,sd,na.rm = TRUE),
		CI = list(per = Perc.CI(acf.y,a1,a2),
		BCa = BCa.CI(acf.y,acf.est,boot,ahat$acf,a1,a2)))
	pacf.l <- list(se = apply(pacf.y,2,sd,na.rm = TRUE),
		CI = list(per = Perc.CI(pacf.y,a1,a2),
		BCa = BCa.CI(pacf.y,pacf.est,boot,ahat$pacf,a1,a2)))
	return(list(acf = acf.l,pacf = pacf.l))
}

##-----------------------------------------------##
##   Jackknife: For surrogate data method       
##-----------------------------------------------##
JN.NB<-function(data,lagmax){
	n.t<-length(data)
	ahat<-vector('list',2)
	names(ahat) = c('acf','pacf')
	acf.y<-matrix(NA,nrow=n.t,ncol=lagmax)
	pacf.y<-matrix(NA,nrow=n.t,ncol=lagmax)
	for(t in 1:n.t){
		data.j<-data[-t]
		acf.y[t,]<-acf(data.j,lag.max=lagmax,plot=F,
			na.action = na.pass)$acf[2:(lagmax+1)]
		pacf.y[t,]<-acf(data.j,lag.max=lagmax,plot=F,
			na.action = na.pass,type = 'partial')$acf[1:lagmax]
	}
	ahat$acf = apply(acf.y,2,get.ahat)
	ahat$pacf = apply(pacf.y,2,get.ahat)
	return(ahat)
}
##-----------------------------------------------##
##  Surrogate
##----------------------------------------------##
Surrogate<-function(ahat,data,a1,a2,boot,lagmax){
	acf.y<-matrix(NA,nrow=boot,ncol=lagmax)
	pacf.y<-matrix(NA,nrow=boot,ncol=lagmax)
	n.t<-length(data)
	for(i in 1:boot){
		for(j in 1:50){
			data.b<-data[sample(1:n.t,n.t,replace=FALSE)]
			tmp.acf<-acf(data.b,lag.max=lagmax,plot=F,
				na.action = na.pass)$acf[2:(lagmax+1)]
			tmp.pacf<-acf(data.b,lag.max=lagmax,plot=F,
				na.action = na.pass,type = 'partial')$acf[1:lagmax]
			if(sum(abs(tmp.pacf>1) + abs(tmp.pacf < (-1)),na.rm = TRUE)==0){break}
		}
		acf.y[i,]	<- tmp.acf
		pacf.y[i,]	<- tmp.pacf
	}
	acf.l <- list(se = apply(acf.y,2,sd,na.rm = TRUE),
			CI = list(per = Perc.CI(acf.y,a1,a2),
			BCa = BCa.CI(acf.y,rep(0,lagmax),boot,ahat$acf,a1,a2)))
	pacf.l <- list(se = apply(pacf.y,2,sd,na.rm = TRUE),
			CI = list(per = Perc.CI(pacf.y,a1,a2),
			BCa = BCa.CI(pacf.y,rep(0,lagmax),boot,ahat$pacf,a1,a2)))
	res = list(acf= acf.l,pacf = pacf.l)
	return(res)
}
##-----------------------------------------------##
##   Percentile CI      
##-----------------------------------------------##
Perc.CI	<- function(est.b,a1,a2){
	lagmax = ncol(est.b)
	CI.l		<- as.vector(apply(est.b,2,quantile,a1,na.rm = TRUE))
	CI.u		<- as.vector(apply(est.b,2,quantile,a2,na.rm = TRUE))
	CI.per = cbind(CI.l,CI.u)
	rownames(CI.per) = paste('lag',1:lagmax,sep='')
	colnames(CI.per) = c('low','up')
	return(CI.per)
}
##-----------------------------------------------##
##   BCa CI      
##-----------------------------------------------##
BCa.CI	<- function(est.b,est,B,ahat,a1,a2){
	lagmax = ncol(est.b)
	num	<- apply(rbind(est,est.b),2,function(x) sum(x[2:(B+1)] < x[1],na.rm=TRUE))
	B.na = apply(est.b,2,function(x) length(na.omit(x)))
	z0	<- qnorm(num/B.na)
	if(sum(num == B.na)>0){z0[num==B.na]=1000}
	if(min(num) == 0){z0[num==0]=-1000}

	zlow	<- qnorm(a1)
	qlow	<- z0 + (z0+zlow)/(1-ahat*(z0+zlow))
	plow	<- matrix(pnorm(qlow),nrow = 1)
	zup	<- qnorm(a2)	
	qup	<- z0 + (z0+zup)/(1-ahat*(z0+zup))
	pup	<- matrix(pnorm(qup),nrow = 1)
	
	BCa.l	<- as.vector(apply(rbind(plow,est.b),2,
		function(x) quantile(x[2:(B+1)],prob=x[1],na.rm = TRUE)))
	BCa.u	<- as.vector(apply(rbind(pup,est.b),2,
		function(x) quantile(x[2:(B+1)],prob=x[1],na.rm = TRUE)))
	
	CI.BCa	<-cbind(BCa.l,BCa.u)
	rownames(CI.BCa) = paste('lag',1:lagmax,sep='')
	colnames(CI.BCa) = c('low','up')
	return(CI.BCa)
}
##-----------------------------------------------##
##   Hypothesis Testing using CI    
##-----------------------------------------------##
Test.CI	<- function(CI,test.value){	
	inT	<- (test.value > CI[,1]) * (test.value < CI[,2])
	return(abs(inT-1))}

