##----------------------------------------------##
## Autocorrelation test by Zijun Ke & Johnny Zhang
##-------------------------------------------------##
# This is a version allowing missing values
# Complete-data analysis is performed.
autocorr.test <- function(x,lagmax=3,
		method='surrogate',alpha = 0.05,L=30,B=1000, digits=3){
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

	zcrit = qnorm(1-alpha/2)
	names = paste("Lag",1:lagmax,sep='')
	acf.est<-acf(x,lag.max = lagmax,plot=F,na.action = na.pass)$acf[2:(lagmax+1)]
	
	if (length(grep(method,"asymptotic"))>0){
		se = sqrt(1/nT.narm)
		z.stat = abs(acf.est/se)
		sig		<- (z.stat > zcrit)
		res = cbind(acf.est,rep(se,lagmax),z.stat,sig)
		colnames(res)=c('Est.','S.E.','z.stat','Sig.')
	}else if (length(grep(method,"bartlett"))>0){
		se = BartlettSE(x,L,lagmax)
		CI = cbind(acf.est-zcrit*se, acf.est+zcrit*se)
		sig = Test.CI(CI,0)
		res = cbind(acf.est,CI,sig)
		colnames(res)=c('Est.',paste((1-alpha)*100,c('%CI LB','%CI UB'),sep=''),'Sig.')
	}else if (length(grep(method,"surrogate"))>0){
		ahat	<- JN.NB(x,lagmax)
		CI	<- Surrogate(acf.est,ahat,x,alpha/2,1-alpha/2,B,lagmax)$CI
		sig = cbind(Test.CI(CI$per[,,1],acf.est),Test.CI(CI$BCa[,,1],acf.est))
		res = cbind(acf.est,CI$per[,,1],sig[,1],CI$BCa[,,1],sig[,2])
		colnames(res)=c('Est.',paste((1-alpha)*100,c('%CI LB','%CI UB'),sep=''),
		'Sig.',paste('BCa ',(1-alpha)*100,c('%CI LB','%CI UB'),sep=''),'BCa Sig.')
	}else if (length(grep(method,"vmb"))>0){
		blocksize = round(nT^(1/3))
		ahat	<- JN.VMB(x,lagmax,blocksize)
		CI	<- VMB(acf.est,ahat,x,blocksize,alpha/2,1-alpha/2,B,lagmax)$CI
		sig = cbind(Test.CI(CI$per[,,1],0),Test.CI(CI$BCa[,,1],0))
		res = cbind(acf.est,CI$per[,,1],sig[,1],CI$BCa[,,1],sig[,2])
		colnames(res)=c('Est.',paste((1-alpha)*100,c('%CI LB','%CI UB'),sep=''),
		'Sig.',paste('BCa ',(1-alpha)*100,c('%CI LB','%CI UB'),sep=''),'BCa Sig.')
	}
	rownames(res) = names
	print(round(res, digits))
	invisible(res)
}
##------------------------------##
## Bartlett's method
##------------------------------##
Bartlett_Shumway <- function(x,rho,L,T){
	l = x
	tmp = 0
	for(li in 1:L){tmp = tmp + (rho[li+l+1] + rho[abs(li-l)+1]-2*rho[l+1]*rho[abs(li)+1])^2}
	se = sqrt(tmp/T)
	return(se)
}	
BartlettSE <- function(data,L,lagmax){
	T = length(na.omit(data))
	rho_hat = acf(data,lag.max=L+lagmax,type="correlation",plot=F,na.action = na.pass)$acf[1:(L+lagmax+1)]
	l = matrix(1:lagmax,1,lagmax)
	se = apply(l,2,Bartlett_Shumway,rho=rho_hat,L=L,T = T)
	return(se)
}
##---------------------------------------------------##
##   Jackknife: For Vectorized Moving Block Bootstrap       
##--------------------------------------------------##
JN.VMB<-function(data,lagmax,l){
	n.t	<-length(data)
	ahat		<-matrix(NA,ncol=lagmax,nrow=1)                      
	acf.y	<- vector('list',lagmax)		
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
	}
	for(i in 1:lagmax){
	if(length(na.omit(acf.y[[i]]))>2){
		temp=mean(acf.y[[i]],na.rm = TRUE)-acf.y[[i]]
		temp2=t(temp)%*%(temp^2)
		sd.acf=sqrt(t(temp)%*%temp)
		ahat[i]=temp2/6/(sd.acf^3)
	}
	}
	return(ahat)
}
##-----------------------------------------------##
##   Jackknife: For surrogate data method       
##-----------------------------------------------##
JN.NB<-function(data,lagmax){
	n.t<-length(data)
	ahat<-matrix(NA,ncol=lagmax,nrow=1)
	acf.y<-matrix(NA,nrow=n.t,ncol=lagmax)
	for(i in 1:n.t){
		data.j<-data[-i]
		acf.y[i,]<-acf(data.j,lag.max=lagmax,type="correlation",plot=F,na.action = na.pass)$acf[2:(lagmax+1)]
	}
	for(i in 1:lagmax){
		temp=mean(acf.y[,i],na.rm = TRUE)-acf.y[,i]
		temp2=t(temp)%*%(temp^2)
		sd.acf=sqrt(t(temp)%*%temp)
		ahat[i]=temp2/6/(sd.acf^3)
	}
	return(ahat)
}
##-----------------------------------------------##
##  Surrogate
##----------------------------------------------##
Surrogate<-function(acf.est,ahat,data,a1,a2,boot,lagmax){
	acf.y<-matrix(NA,nrow=boot,ncol=lagmax)
	n.t<-length(data)
	for(i in 1:boot){
		data.b<-data[sample(1:n.t,n.t,replace=FALSE)]
		acf.y[i,]<-acf(data.b,lag.max=lagmax,type="correlation",plot=F,na.action = na.pass)$acf[2:(lagmax+1)]
	}
	se.acf<-apply(acf.y,2,sd,na.rm = TRUE)
	CI.per<-Perc.CI(acf.y,a1,a2)
	CI.BCa<-BCa.CI(acf.y,rep(0,lagmax),boot,ahat,a1,a2)
	return(list(se = se.acf,CI = list(per = CI.per,BCa = CI.BCa)))
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
VMB<-function(acf.est,ahat,data,l,a1,a2,boot,lagmax){
	acf.y<-matrix(NA,nrow=boot,ncol=lagmax)
	n.t<-length(data)
	k <- floor(n.t/l)
	num.b<-n.t-l+1 #number of blocks
	seq.b<-seq(1,num.b,by=1)
	odr	<- seq.b %x% t(rep(1,l))
	add	<- rep(1,num.b) %x% t(0:(l-1))
	odr <- odr + add
	for(i in 1:boot){
		temp	<- as.vector(sample(seq.b,k+1,replace=TRUE))
		odri	<- t(odr[temp,])
		pair.id.M <- pair.VMB(odri,lagmax,n.t)
		acf.y[i,]	<-	corr.VMB(pair.id.M,data)
	}
	se.acf	<-apply(acf.y,2,sd,na.rm = TRUE)
	CI.per	<-Perc.CI(acf.y,a1,a2)
	CI.BCa	<-BCa.CI(acf.y,acf.est,boot,ahat,a1,a2)
	return(list(se = se.acf,CI = list(per = CI.per,BCa = CI.BCa)))
}
##-----------------------------------------------##
##   Percentile CI      
##-----------------------------------------------##
Perc.CI	<- function(est.b,a1,a2){
	lagmax = ncol(est.b)
	n.alpha = length(a1)
	CI.l		<- matrix(apply(est.b,2,quantile,a1,na.rm = TRUE),ncol=lagmax)
	CI.u		<- matrix(apply(est.b,2,quantile,a2,na.rm = TRUE),ncol=lagmax)
	CI.per = array(NA,dim = c(lagmax,2,n.alpha))
	for(i in 1:n.alpha){CI.per[,,i]	<-cbind(CI.l[i,],CI.u[i,])}
	dimnames(CI.per) = list(paste('lag',1:lagmax,sep=''),c('low','up'),paste('alpha=',round(a1,3),sep=''))
	return(CI.per)
}
##-----------------------------------------------##
##   BCa CI      
##-----------------------------------------------##
BCa.CI	<- function(est.b,est,B,ahat,a1,a2){
	lagmax = ncol(est.b)
	n.alpha = length(a1)
	CI.BCa = array(NA,dim = c(lagmax,2,n.alpha))
	num	<- apply(rbind(est,est.b),2,function(x) sum(x[2:(B+1)] < x[1],na.rm=TRUE))
	B.na = apply(est.b,2,function(x) length(na.omit(x)))
	z0	<- qnorm(num/B.na)
	if(sum(num == B.na)>0){z0[num==B.na]=1000}
	if(min(num) == 0){z0[which(num==0)]=-1000}
	for(i in 1:n.alpha){
		zlow	<- qnorm(a1[i])
		qlow	<- z0 + (z0+zlow)/(1-ahat*(z0+zlow))
		plow	<- matrix(pnorm(qlow),nrow = 1)
		zup	<- qnorm(a2[i])	
		qup	<- z0 + (z0+zup)/(1-ahat*(z0+zup))
		pup	<- matrix(pnorm(qup),nrow = 1)
		BCa.l	<- matrix(apply(rbind(plow,est.b),2,
		function(x) quantile(x[2:(B+1)],prob=x[1],na.rm = TRUE)),ncol=lagmax)
		BCa.u	<- matrix(apply(rbind(pup,est.b),2,
		function(x) quantile(x[2:(B+1)],prob=x[1],na.rm = TRUE)),ncol=lagmax)
		CI.BCa[,,i]	<-cbind(BCa.l,BCa.u)
	}
	dimnames(CI.BCa) = list(paste('lag',1:lagmax,sep=''),c('low','up'),paste('alpha=',round(a1,3),sep=''))
	return(CI.BCa)
}
##-----------------------------------------------##
##   Hypothesis Testing using CI    
##-----------------------------------------------##
Test.CI	<- function(CI,test.value){	
	inT	<- (test.value > CI[,1]) * (test.value < CI[,2])
	return(abs(inT-1))}