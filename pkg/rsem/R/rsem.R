#####################################
## Robust SEM with missing data    ##
## Zhiyong Zhang and Ke-Hai Yuan   ##
## Created: Dec 25, 2010           ##
#####################################

## Version 2011/09/28
## Version 2011/12/11
## Version 2012/06/07
## Version 2012/08/24 Add the support of Lavaan

##############################################
## Function to find missing data patterns   ##
## Function rsem.pattern                    ##
##############################################
rsem.pattern<-function(x,print=TRUE){
  ## This function generate missing data patterns
  ## INPUT
  ## x: data set
  ## OUTPUT
  ## misinfo
  ##   [1,] number of cases in the pattern
  ##   [2,] number of observed variables in the pattern
  ##   [3:(p+2), ] contains a permutation of {1,2,3,...,p} 
  ##                      the first part corresponding to observed variables
  ##                      the remaining corresponding to missing variables
  if (missing(x)) stop("A data set has to be provided!")
  if (!is.matrix(x)) x<-as.matrix(x)
  
  n<-dim(x)[1]
  p<-dim(x)[2]
  misorder<-rep(0,n)
  for (i in 1:n){
    misorderj<-0
    for (j in 1:p){
      if (is.na(x[i,j])) misorderj<-misorderj+2^(j-1)
    }
    misorder[i]<-misorderj
  }
  ## Combine data with missing pattern indicator
  ## order data according to misorder
  temp<-order(misorder)
  x<-x[temp,]
  misn<-misorder[temp]
  
  ##identifying the subscripts of missing variables and put them in misinfo;
  mi<-0; nmi<-0;oi<-0; noi<-0;
  for (j in 1:p){
    if (is.na(x[1,j])){
      mi<-c(mi,j)  ## recording the missing variable subscript in the first case
      nmi<-nmi+1   ## number of missing values in the first case
    }else{
      oi<-c(oi,j)
      noi<-noi+1
    }
  }
  oi<-oi[2:(noi+1)]
  if (nmi==0){
    misinfo_0 = c(noi, oi)
  }else{
    mi<-mi[2:(nmi+1)]
    misinfo_0<-c(noi,oi,mi) ##recording the # observed variables, locations;
  }	
  patnobs <- 0 ## number of observed cases in a pattern
  totpat<-1; ncount<-1;
  t1<-misn[1]
  for (i in 2:n){
    if (misn[i]==t1){
      ncount<-ncount+1
    }else{
      patnobs<-c(patnobs,ncount)
      t1<-misn[i]
      ncount<-1
      totpat<-totpat+1
      mi<-0; nmi<-0;oi<-0; noi<-0;
      for (j in 1:p){
        if (is.na(x[i,j])){
          mi<-c(mi,j)
          nmi<-nmi+1
        }else{
          oi<-c(oi,j)
          noi<-noi+1
        }
      }
      oi<-oi[2:(noi+1)]
      mi<-mi[2:(nmi+1)]
      misinfo_0 <- rbind(misinfo_0, c(noi,oi,mi))
    }
  }
  patnobs<-c(patnobs, ncount)
  patnobs<-patnobs[2:(totpat+1)]
  if (is.vector(misinfo_0)){
    misinfo<-c(patnobs, misinfo_0)
  }else{
    misinfo<-cbind(patnobs, misinfo_0)
  }
  if (!is.matrix(misinfo)){
    misinfo<-matrix(misinfo, nrow=1)
  }
  
  ## Different presentation of missing data patterns
  nr<-nrow(misinfo)
  mispat<-matrix(1, nrow=nr, ncol=p)
  
  for (i in 1:nr){
    if (misinfo[i,2]<p){
      ind<-misinfo[i, (misinfo[i,2]+3):(p+2)]
      mispat[i, ind]<-0
    }
  }
  mispat<-cbind(misinfo[,1:2, drop=FALSE], mispat)
  rownames(mispat)<-paste('Pattern ', 1:nr, sep="")
  colnames(mispat)<-c('n','nvar',colnames(x))
  
  if (print) print(mispat)
  
  invisible(list(misinfo=misinfo, mispat=mispat, x=x))
}

##############################################
## Function to calculate ssq of a matrix    ##
## Function rsem.ssq                        ##
##############################################
rsem.ssq<-function(x){
  ## x: a matrix
  sum(x^2)
}

##############################################
## EM algorithm for unstructured (mu, sigma)##
## Function rsem.emmusig                    ##
##############################################

rsem.emmusig<-function(xpattern, varphi=.1, max.it=1000, st='i'){
  ## x: data set
  ## misinfo: missing data pattern
  ## varphi: 
  if (is.null(xpattern$mispat)) stop("The output from the function rsem.pattern is required")
  x<-xpattern$x
  misinfo<-xpattern$misinfo
  
  ep <- 1e-6  ## precision
  n<-dim(x)[1]
  p<-dim(x)[2]
  
  mu0<-rep(0,p)
  sig0<-diag(p)
  if (st=='mcd'){
    y<-na.omit(x)
    ny<-nrow(y)
    par.st<-cov.rob(y, method='mcd')
    mu0<-par.st$center
    sig0<-par.st$cov
  }
  
  n_it<-0;        
  dt<-1;
  if (varphi==0){
    ck<-10e+10
    cbeta<-1
  }else{
    prob<-1-varphi ## chi-square p-value
    chip<-qchisq(prob, p)
    ck<-sqrt(chip)
    cbeta<-( p*pchisq(chip, p+2) + chip*(1-prob) )/p
  }
  while (dt>ep && n_it <= max.it){
    sumx<-rep(0,p); sumxx<-array(0,dim=c(p,p)); sumw1<-0; sumw2<-0;
    npat<-dim(misinfo)[1]  ## number of missing data patterns
    p1<-misinfo[1,2]       ## number of observed variables in pattern 1
    n1<-misinfo[1,1]       ## number of cases in pattern 1
    if (p1==p){            ## complete data
      sigin <- solve(sig0)  ## matrix inverse
      for (i in 1:n1){
        xi<-x[i,]
        xi0<-xi-mu0
        di2<-xi0%*%sigin%*%xi0
        di<-sqrt(di2)
        ## Huber weight functions
        if (di<=ck){
          wi1<-1
          wi2<-1/cbeta
        }else{
          wi1<-ck/di
          wi2<-wi1*wi1/cbeta
        } ## end Huber weight
        sumw1<-sumw1+wi1;
        xxi0<-xi0%*%t(xi0)
        sumx<-sumx+wi1*xi
        sumxx<-sumxx+c(wi2)*xxi0
        sumw2<-sumw2+wi2
      } ## end for
    }else{ ## end p1==p
      ## with missing data
      if (varphi==0){
        ck1<-1e+10
        cbeta1<-1
      }else{ 
        chip1<-qchisq(prob, p1)
        ck1<-sqrt(chip1)
        cbeta1<-( p1*pchisq(chip1,p1+2) + chip1*(1-prob) )/p1
      }
      o1<-misinfo[1,3:(2+p1)]
      m1<-misinfo[1,(2+p1+1):(p+2)]
      mu_o<-mu0[o1]; mu_m<-mu0[m1]
      sig_oo<-sig0[o1,o1]; sig_om<-sig0[o1,m1];
      if (p1==1) {sig_mo<-sig_om}else{sig_mo<-t(sig_om)} 
      sig_mm<-sig0[m1,m1];
      sigin_oo<-solve(sig_oo)
      beta_mo<-sig_mo%*%sigin_oo
      
      delt <- array(0, dim=c(p,p))
      delt[m1,m1]<-sig_mm - beta_mo%*%sig_om
      for (i in 1:n1){
        xi<-x[i,]
        xi_o<-xi[o1]
        xi0_o<-xi_o-mu_o
        stdxi_o<-sigin_oo%*%xi0_o
        di2<-xi0_o%*%stdxi_o
        di<-sqrt(di2)
        if (di<=ck1){ ##Huber weight
          wi1<-1
          wi2<-1/cbeta1
        }else{
          wi1<-ck1/di
          wi2<-wi1*wi1/cbeta1
        }
        sumw1<-sumw1+wi1
        xm1<-mu_m+sig_mo%*%stdxi_o
        xi[m1]<-xm1
        xi0<-xi-mu0
        xxi0<-xi0%*%t(xi0)
        sumx<-sumx+wi1*xi
        sumxx<-sumxx+c(wi2)*xxi0+delt
        sumw2<-sumw2+wi2
      } ##end for 1:n1  
    }## end of (p1=p)
    ## start from pattern 2	
    if (npat>1){
      snj<-n1	
      for (j in 2:npat){
        nj<-misinfo[j,1]; pj<-misinfo[j,2];
        oj<-misinfo[j, 3:(2+pj)]; mj<-misinfo[j, (2+pj+1):(p+2)];
        mu_o<-mu0[oj]; mu_m<-mu0[mj];
        sig_oo<-sig0[oj,oj]; sig_om<-sig0[oj,mj];
        if (pj==1) {sig_mo<-sig_om}else{sig_mo<-t(sig_om)} 
        sig_mm<-sig0[mj,mj];
        sigin_oo<-solve(sig_oo)
        beta_mo<-sig_mo%*%sigin_oo
        delt <- array(0, dim=c(p,p))
        delt[mj,mj]<-sig_mm - beta_mo%*%sig_om
        if (varphi==0){
          ckj<-10e+10
          cbetaj<-1
        }else{
          chipj<-qchisq(prob,pj)
          ckj<-sqrt(chipj)
          cbetaj<- ( pj*pchisq(chipj, pj+2) + chipj*(1-prob) )/pj
        }
        for (i in ((snj+1):(snj+nj))){
          xi<-x[i,]
          xi_o<-xi[oj]
          xi0_o<-xi_o - mu_o
          stdxi_o<-sigin_oo%*%xi0_o
          di2<-xi0_o%*%stdxi_o
          di<-sqrt(di2)
          if (di<=ckj){ ##Huber weight
            wi1<-1
            wi2<-1/cbetaj
          }else{
            wi1<-ckj/di
            wi2<-wi1*wi1/cbetaj
          }
          sumw1<-sumw1+wi1
          xmj<-mu_m+sig_mo%*%stdxi_o
          xi[mj]<-xmj
          xi0<-xi-mu0
          xxi0<-xi0%*%t(xi0)
          sumx<-sumx+wi1*xi
          sumxx<-sumxx+c(wi2)*xxi0+delt
          sumw2<-sumw2+wi2
        }
        snj<-snj+nj
      } ## for (j in 2:npat)
    }
    mu1<-sumx/sumw1
    sig1<-sumxx/n
    dt<-max(c(max(abs(mu1-mu0)), max(abs(sig1-sig0))));
    mu0<-mu1;
    sig0<-sig1;
    n_it<-n_it+1;
  } ## end while
  if (n_it>=max.it) warning("The maximum number of iteration was exceeded. Please increase max.it in the input.")
  rownames(sig1)<-colnames(sig1)
  names(mu1)<-colnames(sig1)
  list(mu=mu1, sigma=sig1, max.it=n_it)
}

##############################################
## Stacking a matrix to a vector            ##
## Function rsem.vec                        ##
##############################################
rsem.vec<-function(x){
  t(t(as.vector(x)))
}

##############################################
## Stacking lower triange of a matrix to    ##
##   a vector                               ##
## Function rsem.vech                       ##
##############################################
rsem.vech<-function(x){
  t(x[!upper.tri(x)])
}

##############################################
## Generate a duplication matrix            ##
## Function rsem.DP                         ##
##############################################
rsem.DP <- function(x){ 	
  mat <- diag(x)
  index <- seq(x*(x+1)/2)
  mat[ lower.tri( mat , TRUE ) ] <- index
  mat[ upper.tri( mat ) ] <- t( mat )[ upper.tri( mat ) ]
  outer(c(mat), index , function( x , y ) ifelse(x==y, 1, 0 ) )
}

##############################################
## index for vec(Sigma_j) corresponding to  ##
##   the observed cases                     ##
## Function rsem.index                      ##
##############################################
rsem.index<-function(p,oj){
  temp<-1:(p*p)
  index<-array(temp, dim=c(p,p))
  indexoj<-index[oj,oj]
  nj<-length(oj)
  rsem.vec(indexoj)
}

##############################################
## index for vec(Sigma) corresponding to    ##
##   selected variables                     ##
## Function rsem.indexv                     ##
##############################################
rsem.indexv<-function(p, select){
  pv<-length(select)
  pvs<-pv*(pv+1)/2
  index_s<-rep(0,pvs)
  count<-p
  countv<-0
  for (i in 1:p){
    for (j in i:p){
      count<-count+1
      for (iv in 1:pv){
        for (jv in iv:pv){
          if (i==select[iv] && j==select[jv]){
            countv<-countv+1
            index_s[countv]<-count
          }
        }
      }
    }
  }
  c(select, index_s)
}

##############################################
## index for vec(Sigma) corresponding to    ##
##   selected variables                     ##
## Function rsem.indexvc                    ##
##############################################
rsem.indexvc<-function(p, select){
  pv<-length(select)
  pvs<-pv*(pv+1)/2
  index_s<-rep(0,pvs)
  count<-0
  countv<-0
  for (i in 1:p){
    for (j in i:p){
      count<-count+1
      for (iv in 1:pv){
        for (jv in iv:pv){
          if (i==select[iv] && j==select[jv]){
            countv<-countv+1
            index_s[countv]<-count
          }
        }
      }
    }
  }
  index_s + rep(p,pvs)
}

##############################################
## generating a permutation matrix from the ## 
## order of vech(A) to the vecs(A) as used  ## 
## by EQS                                   ##
## Function rsem.switch                     ##
##############################################
rsem.switch<-function(p){
  ps<-p*(p+1)/2
  bmat<-array(0,c(p,p))
  nb<-0
  for (i in 1:p){
    for (j in 1:i){
      nb<-nb+1
      bmat[i,j]<-nb
    }
  }
  vb<-rsem.vech(bmat)
  Imatc<-diag(ps)
  permuc<-Imatc[,vb]
  iMat<-diag(p+ps)
  vp<-1:p
  vs<-c(vp, (vb+rep(p,ps)))
  permu<-iMat[,vs]
  permu<-rbind(permu[(p+1):(p+ps), ], permu[1:p, ])
  list(mu=permu, sigma=permuc)
}


##############################################
## computing the estimator of the asymptotic## 
## covariance of \hat\Omega_{\hat\beta};    ##
## Function rsem.Ascov                      ##
##############################################  
rsem.gname<-function(name){
    temp.name<-NULL
    k<-length(name)
    for (i in 1:k){
      for (j in i:k){
        temp.name<-c(temp.name, paste(name[i], ".", name[j], sep=""))
      }
    }
    temp.name
}

rsem.Ascov<-function(xpattern, musig, varphi=.1){
  if (is.null(xpattern$mispat)) stop("The output from the function rsem.pattern is required")
  x<-xpattern$x
  misinfo<-xpattern$misinfo
  
  mu0<-musig$mu
  sig0<-musig$sig
  
  n<-dim(x)[1]; p<-dim(x)[2];
  ps<-p*(p+1)/2; pps<-p+ps;
  dup<-rsem.DP(p) ##duplication matrix
  dupt<-t(dup) 
  i_p<-diag(p)
  B11<-array(0, c(p,p)); B12<-array(0,c(p,ps)); B22<-array(0,c(ps,ps));
  ddl11<-array(0,c(p,p)); ddl12<-array(0,c(p,ps)); 
  ddl21<-array(0,c(ps,p)); ddl22<-array(0,c(ps,ps));
  if (varphi==0){
    ck<-1e+10
    cbeta<-1
  }else{
    prob<-1-varphi
    chip<-qchisq(prob,p);
    ck<-sqrt(chip);
    cbeta<-( p*pchisq(chip,p+2)+ chip*(1-prob) )/p;
  }
  dl<-rep(0,pps)
  npat<-dim(misinfo)[1]
  n1<-misinfo[1,1]; p1<-misinfo[1,2];
  if (p1==p){
    ## complete data
    sigin<-solve(sig0)
    vecsig<-rsem.vec(sig0)
    Wmat<-kronecker(sigin,sigin)/2  ##Kronecker product
    for (i in 1:n1){
      xi<-x[i,]
      xi0<-xi-mu0;
      stdxi<-sigin%*%xi0
      stdxit<-t(stdxi)
      di2<-xi0%*%stdxi
      di<-sqrt(di2)
      ## Huber weight functions;
      if (di<=ck){
        wi1<-1
        wi2<-1/cbeta
        dwi1<-0
        dwi2<-0				
      }else{
        wi1<-ck/di
        wi2<-wi1*wi1/cbeta
        dwi1<-wi1/di2
        dwi2<-wi2/di2
      } ##end Huber weight
      ## computing B_\beta
      dlimu<-c(wi1)*stdxi
      xixi0<-xi0%*%t(xi0)
      vecyi<-rsem.vec(xixi0)
      wvecyi<-c(wi2)*vecyi
      dlisig<-dupt%*%Wmat%*%(wvecyi-vecsig)
      B11<-B11+dlimu%*%t(dlimu)
      B12<-B12+dlimu%*%t(dlisig)
      B22<-B22+dlisig%*%t(dlisig)
      dl_i<-c(dlimu, dlisig)
      dl<-dl+dl_i
      ## computing A_\beta
      Hi<-stdxi%*%stdxit
      tti<-c(wi1)*sigin
      uui<-c(wi2)*sigin
      ddl11<-ddl11 + (-tti+c(dwi1)*Hi)
      ddl22<-ddl22 + dupt%*%( Wmat - kronecker(Hi, (uui-.5*c(dwi2)*Hi) ) )%*%dup
      ddl12<-ddl12 + kronecker( (-tti+.5*c(dwi1)*Hi) ,stdxit)%*%dup
      ddl21<-ddl21 + dupt%*%kronecker( (-uui+c(dwi2)*Hi), stdxi )
    } ## for 1:n1		
  }else{			
    ## missing data
    if (varphi==0){
      ck1<-1e+10
      cbeta1<-1
    }else{
      chip1<-qchisq(prob,p1)
      ck1<-sqrt(chip1)
      cbeta1<-( p1*pchisq(chip1,p1+2) + chip1*(1-prob) )/p1
    }
    o1<-misinfo[1,3:(2+p1)]
    mu_o<-mu0[o1]
    sig_oo<-sig0[o1,o1]
    vecsig_oo<-rsem.vec(sig_oo)
    sigin_oo<-solve(sig_oo)
    E1<-i_p[o1,]; 
    if (o1==1){Et1=E1}else{Et1<-t(E1)}
    F1<-kronecker(E1, E1)%*%dup;
    Ft1<-t(F1)
    Wmat1<-.5*kronecker(sigin_oo, sigin_oo)
    for (i in 1:n1){
      xi<-x[i,]
      xi_o<-xi[o1]
      xi0_o<-xi_o-mu_o
      xi0_ot<-t(xi0_o)
      stdxi_o<-sigin_oo%*%xi0_o
      stdxit_o<-t(stdxi_o)
      di2<-xi0_o%*%stdxi_o
      di<-sqrt(di2)
      ## Huber weight functions;
      if (di<=ck){
        wi1<-1
        wi2<-1/cbeta
        dwi1<-0
        dwi2<-0				
      }else{
        wi1<-ck/di
        wi2<-wi1*wi1/cbeta
        dwi1<-wi1/di2
        dwi2<-wi2/di2
      } ##end Huber weight
      ## computing B_\beta
      dlimu<-c(wi1)*Et1%*%stdxi_o
      xixi0_o<-xi0_o%*%t(xi0_o)
      vecyi<-rsem.vec(xixi0_o)
      wvecyi<-c(wi2)*vecyi
      dlisig<-Ft1%*%Wmat1%*%(wvecyi-vecsig_oo)
      B11<-B11+dlimu%*%t(dlimu)
      B12<-B12+dlimu%*%t(dlisig)
      B22<-B22+dlisig%*%t(dlisig)
      dl_i<-c(dlimu, dlisig)
      dl<-dl+dl_i
      ## computing A_\beta
      Hi<-stdxi_o%*%stdxit_o
      tti<-c(wi1)*sigin_oo
      uui<-c(wi2)*sigin_oo
      ddl11<-ddl11 + Et1%*%(-tti+c(dwi1)*Hi)%*%E1
      ddl22<-ddl22 + Ft1%*%( Wmat1 - kronecker(Hi, (uui-.5*c(dwi2)*Hi) ) )%*%F1
      ddl12<-ddl12 + Et1%*%kronecker( (-tti+.5*c(dwi1)*Hi) ,stdxit_o)%*%F1
      ddl21<-ddl21 + Ft1%*%kronecker( (-uui+c(dwi2)*Hi), stdxi_o )%*%E1
      
    } ## end for 1:n1		
  }##end p1==p
  
  ## for patterns 2 and above
  if (npat>1){
    snj<-n1
    for (j in 2:npat){
      nj<-misinfo[j,1]
      pj<-misinfo[j,2]
      if (varphi==0){
        ckj<-1e+10
        cbetaj<-1
      }else{
        chipj<-qchisq(prob, pj)
        ckj<-sqrt(chipj)
        cbetaj<-( pj*pchisq(chipj,pj+2) + chipj*(1-prob) )/pj
      }
      oj<-misinfo[j, 3:(2+pj)]
      mu_o<-mu0[oj]
      sig_oo<-sig0[oj,oj]
      sigin_oo<-solve(sig_oo)
      vecsig_oo<-rsem.vec(sig_oo)
      Ej<-i_p[oj, ]
      if (pj==1){Etj<-Ej}else{Etj<-t(Ej)}
      Fj<-kronecker(Ej, Ej)%*%dup
      Ftj<-t(Fj)
      Wmati<-0.5*kronecker(sigin_oo, sigin_oo)
      for (i in (snj+1):(snj+nj)){
        xi<-x[i,]
        xi_o<-xi[oj]
        xi0_o<-xi_o-mu_o
        xi0_ot<-t(xi0_o)
        stdxi_o<-sigin_oo%*%xi0_o
        stdxit_o<-t(stdxi_o)
        di2<-xi0_o%*%stdxi_o
        di<-sqrt(di2)
        ## Huber weight functions;
        if (di<=ckj){
          wi1<-1
          wi2<-1/cbetaj
          dwi1<-0
          dwi2<-0				
        }else{
          wi1<-ckj/di
          wi2<-wi1*wi1/cbetaj
          dwi1<-wi1/di2
          dwi2<-wi2/di2
        } ##end Huber weight
        ## computing B_\beta
        dlimu<-c(wi1)*Etj%*%stdxi_o
        xixi0_o<-xi0_o%*%t(xi0_o)
        vecyi<-rsem.vec(xixi0_o)
        wvecyi<-c(wi2)*vecyi
        dlisig<-Ftj%*%Wmati%*%(wvecyi-vecsig_oo)
        B11<-B11+dlimu%*%t(dlimu)
        B12<-B12+dlimu%*%t(dlisig)
        B22<-B22+dlisig%*%t(dlisig)
        dl_i<-c(dlimu, dlisig)
        dl<-dl+dl_i
        ## computing A_\beta
        Hi<-stdxi_o%*%stdxit_o
        tti<-c(wi1)*sigin_oo
        uui<-c(wi2)*sigin_oo
        ddl11<-ddl11 + Etj%*%(-tti+c(dwi1)*Hi)%*%Ej
        ddl22<-ddl22 + Ftj%*%( Wmati - kronecker(Hi, (uui-.5*c(dwi2)*Hi) ) )%*%Fj
        ddl12<-ddl12 + Etj%*%kronecker( (-tti+.5*c(dwi1)*Hi) ,stdxit_o)%*%Fj
        ddl21<-ddl21 + Ftj%*%kronecker( (-uui+c(dwi2)*Hi), stdxi_o )%*%Ej
        
      } ## end for snj+1:snj+nj
      snj<-snj+nj
    } ##end for 2:npat
  }

  ## Constructing B_\bata and A_\beta matrices
  Bbeta<-rbind( cbind(B11, B12), cbind(t(B12), B22) )
  Abeta<-rbind( cbind(ddl11, ddl12), cbind(ddl21, ddl22) )
  Abin<-solve(Abeta)
  Omega<-n*Abin%*%Bbeta%*%t(Abin)
  Gamma<-Omega
  
  xnames<-colnames(x)
  if (is.null(xnames)) xnames<-paste('V', 1:p)
  
  mnames<-rsem.gname(xnames)
  colnames(Abeta)<-colnames(Bbeta)<-colnames(Gamma)<-rownames(Abeta)<-rownames(Bbeta)<-rownames(Gamma)<-c(xnames, mnames)
  
  list(Abeta=Abeta, Bbeta=Bbeta, Gamma=Gamma)
}

##############################################
## Main function for batch analysis         ##
## rsem.main                                ## 
##############################################
rsem<-function(dset, select, EQSmodel, moment=TRUE, varphi=.1, st='i', max.it=1000, eqsdata='data.txt', eqsweight='weight.txt', EQSpgm="C:/Progra~1/EQS61/WINEQS.EXE", serial="1234"){
  ## data: data matrix
  ## select: variabes to be selected for analysis in EQS
  ## varphi: proportion of data to be downweighted
  ## miss: missing data indicator
  if (missing(dset)) stop("A data set is needed!")
  if (!is.matrix(dset)) dset<-data.matrix(dset)
  n<-dim(dset)[1]
  p<-dim(dset)[2]
  cat("Sample size n =", n, "\n")
  cat("Total number of variables q =", p, "\n\n")
  
  if (missing(select)) select<-c(1:p)	
  cat("The following",length(select),"variables are selected for SEM models \n")
  cat(colnames(dset)[select], "\n\n")
  p_v<-length(select)
  
  pvs<-p_v+p_v*(p_v+1)/2
  
  ## missing data patterns
  miss_pattern<-rsem.pattern(dset)
  x<-miss_pattern$x                             ## data after ordering
  misinfo<-miss_pattern$misinfo                 ## missing data pattern
  
  totpat<-dim(misinfo)[1]                       ## total number of patterns
  cat("There are", totpat, "missing data patterns. They are \n")
  print(misinfo)
  cat("\n")
  ## run EM
  em_results<-rsem.emmusig(miss_pattern, varphi=varphi)
  if (em_results$max.it >= max.it){ warning("\nMaximum iteration for EM is exceeded and the results may not be trusted. Change max.it to a greater number.\n") }
  hmu1<-em_results$mu                           ## means for all variables including auxiliary variables
  hsigma1<-em_results$sigma                     ## covariance matrix for all variables
  ## Calculate the sandwitch covariance matrix
  ascov_results<-rsem.Ascov(miss_pattern, em_results, varphi=varphi)
  Abeta<-ascov_results$Abeta
  Bbeta<-ascov_results$Bbeta
  hupsilon<-ascov_results$Gamma
  index_beta<-rsem.indexv(p, select)
  index_sig<-rsem.indexvc(p,select)
  
  index_other<-c(select, index_sig)
  gamma_other<-hupsilon[index_other,index_other]
  
  hmu<-hmu1[select]                           ## means for selected variables
  hsigma<-hsigma1[select, select]           ## covariance matrix for selected variables
  cat("Estimated means: \n")
  print(hmu)
  if (missing(EQSmodel)){
    se.hup<-sqrt(diag(hupsilon/n))
    se.hmu1<-se.hup[1:p]
    se.hsig1<-se.hup[(p+1):(p*(p+3)/2)]
    se.matrix.hsig1<-array(0, c(p,p))
    se.matrix.hsig1[lower.tri(se.matrix.hsig1,TRUE)]<-se.hsig1
    se.matrix.hsig1[upper.tri(se.matrix.hsig1)]<-se.matrix.hsig1[lower.tri(se.matrix.hsig1)]
    se.hmu<-se.hmu1[select]
    se.matrix.hsig<-se.matrix.hsig1[select,select]
    cat("Standard errors for estimated means:\n");
    print(se.hmu)
  }
  cat("\nEstimated covariance matrix: \n")
  print(hsigma)
  if (missing(EQSmodel)){
    cat("Standard errors for estimated covariance matrix: \n")
    print(se.matrix.hsig)		
  }
  cat("\n")
  
  
  if (!missing(EQSmodel)){
    ## Construct data for EQS SEM analysis 
    permu_sig<-rsem.switch(p_v)$sigma
    permu_beta<-rsem.switch(p_v)$mu
    
    hgamma_sig<-hupsilon[index_sig,index_sig]
    hgamma_sig<-permu_sig%*%hgamma_sig%*%t(permu_sig)
    
    hgamma_beta<-hupsilon[index_beta,index_beta]
    hgamma_beta_eqs<-rbind( cbind(permu_beta%*%hgamma_beta%*%t(permu_beta), rep(0,pvs)), c(rep(0,pvs), 1) ) ##weight matrix
    hgamma_beta<-permu_beta%*%hgamma_beta%*%t(permu_beta)
    ## Run EQS for data analysis
    ## write data and weight matrix for EQS
    if (moment){
      write.table(rbind(hsigma,hmu), 'data.txt', row.names=FALSE,col.names=FALSE)
      ## write the weight matrix
      write.table(hgamma_beta_eqs, 'weight.txt', row.names=FALSE,col.names=FALSE)
    }else{
      write.table(rbind(hsigma), 'data.txt', row.names=FALSE,col.names=FALSE)
      ## write the weight matrix
      write.table(hgamma_sig, 'weight.txt', row.names=FALSE,col.names=FALSE)
    }
    #EQSmodel<-paste(getwd(), '/' , EQSmodel, sep='')
    res <- semdiag.run.eqs(EQSpgm = EQSpgm, EQSmodel = EQSmodel, serial = serial)
    
    ## Fit statistics
    ## All fit statistics
    ## res$fit.indices
    ## res$pval
    
    ## The 4 statistics in Table 1
    
    fit<-res$fit.indices
    pval<-res$pval
    
    fit.stat<-rbind(
      c(fit[sub(' +','',row.names(fit))=='SBCHI',1], pval[sub(' +','',row.names(pval))=='SBPVAL',1]),
      c(fit[sub(' +','',row.names(fit))=='MVADJCHI',1], pval[sub(' +','',row.names(pval))=='TPADJCHI',1]),
      c(fit[sub(' +','',row.names(fit))=='YBRESTST',1], pval[sub(' +','',row.names(pval))=='TPYBRTST',1]),
      c(fit[sub(' +','',row.names(fit))=='YBRESF',1], pval[sub(' +','',row.names(pval))=='TPYBRESF',1])
    )
    
    colnames(fit.stat)<-c('T','p')
    rownames(fit.stat)<-c('RML','AML','CRADF','RF')
    
    ## print fit statistics
    cat('Test statistics:\n')
    print(fit.stat)
    cat('\nParameter estimates:\n')
    z<-res$par.table[,1]/res$par.table[,3]
    par.est<-cbind(res$par.table[,c(1,3)], z)
    colnames(par.est)<-c('Parameter', 'SE', 'z-score')
    print(par.est)
    invisible(list(fit.stat=fit.stat, para=par.est, sem=list(mu=hmu, sigma=hsigma, gamma_eqs_cov=hgamma_sig, gammam_eqs_mcov=hgamma_beta_eqs), misinfo=miss_pattern, em=em_results, ascov=ascov_results, eqs=res))
  }else{
    invisible(list(sem=list(mu=hmu, sigma=hsigma, gamma=gamma_other), misinfo=miss_pattern, em=em_results, ascov=ascov_results))		
  }
}

rsem.print<-function(object, robust.se, robust.fit, estimates=TRUE, fit.measures=FALSE, standardized=FALSE, 
                     rsquare=FALSE, std.nox=FALSE, modindices=FALSE) {
  ## print TML
  t0.txt <- sprintf("  %-40s", "Statistic")
  t1.txt <- sprintf("  %10s", "ML")
  cat(t0.txt, t1.txt, "\n", sep="")
  t0.txt <- sprintf("  %-40s", "Value")  
  t1.txt <- sprintf("  %10.3f", object@Fit@test[[1]]$stat)
  cat(t0.txt, t1.txt, "\n", sep="")
  
  # 2. degrees of freedom
  t0.txt <- sprintf("  %-40s", "Degrees of freedom")
  t1.txt <- sprintf("  %10i", object@Fit@test[[1]]$df); 
  cat(t0.txt, t1.txt, "\n", sep="")
  
  # 3. P-value
  t0.txt <- sprintf("  %-40s", "P-value")
  t1.txt <- sprintf("  %10.3f", object@Fit@test[[1]]$pvalue)
  cat(t0.txt, t1.txt, "\n\n", sep="")
  
  ## Print T_RML
  t0.txt <- sprintf("  %-40s", "Statistic")
  t1.txt <- sprintf("  %10s", "RML")
  cat(t0.txt, t1.txt, "\n", sep="")
  t0.txt <- sprintf("  %-40s", "Value")  
  t1.txt <- sprintf("  %10.3f", robust.fit$TRML[[1]][1])
  cat(t0.txt, t1.txt, "\n", sep="")
  
  # 2. degrees of freedom
  t0.txt <- sprintf("  %-40s", "Degrees of freedom")
  t1.txt <- sprintf("  %10i", robust.fit$TRML[[1]][2]); 
  cat(t0.txt, t1.txt, "\n", sep="")
  
  # 3. P-value
  t0.txt <- sprintf("  %-40s", "P-value")
  t1.txt <- sprintf("  %10.3f", robust.fit$TRML[[1]][3])
  cat(t0.txt, t1.txt, "\n\n", sep="")
  
  ## Print T_AML
  t0.txt <- sprintf("  %-40s", "Statistic")
  t1.txt <- sprintf("  %10s", "AML")
  cat(t0.txt, t1.txt, "\n", sep="")
  t0.txt <- sprintf("  %-40s", "Value")  
  t1.txt <- sprintf("  %10.3f", robust.fit$TAML[[1]][1])
  cat(t0.txt, t1.txt, "\n", sep="")
  
  # 2. degrees of freedom
  t0.txt <- sprintf("  %-40s", "Degrees of freedom")
  t1.txt <- sprintf("  %10.3f", robust.fit$TAML[[1]][2]); 
  cat(t0.txt, t1.txt, "\n", sep="")
  
  # 3. P-value
  t0.txt <- sprintf("  %-40s", "P-value")
  t1.txt <- sprintf("  %10.3f", robust.fit$TAML[[1]][3])
  cat(t0.txt, t1.txt, "\n\n", sep="")
  
  ## Print T_CRADF
  t0.txt <- sprintf("  %-40s", "Statistic")
  t1.txt <- sprintf("  %10s", "CRADF")
  cat(t0.txt, t1.txt, "\n", sep="")
  t0.txt <- sprintf("  %-40s", "Value")  
  t1.txt <- sprintf("  %10.3f", robust.fit$TCRADF[[1]][1])
  cat(t0.txt, t1.txt, "\n", sep="")
  
  # 2. degrees of freedom
  t0.txt <- sprintf("  %-40s", "Degrees of freedom")
  t1.txt <- sprintf("  %10i", robust.fit$TCRADF[[1]][2]); 
  cat(t0.txt, t1.txt, "\n", sep="")
  
  # 3. P-value
  t0.txt <- sprintf("  %-40s", "P-value")
  t1.txt <- sprintf("  %10.3f", robust.fit$TCRADF[[1]][3])
  cat(t0.txt, t1.txt, "\n\n", sep="")
  
  ## Print T_RF
  t0.txt <- sprintf("  %-40s", "Statistic")
  t1.txt <- sprintf("  %10s", "RF")
  cat(t0.txt, t1.txt, "\n", sep="")
  t0.txt <- sprintf("  %-40s", "Value")  
  t1.txt <- sprintf("  %10.3f", robust.fit$TRF[[1]][1])
  cat(t0.txt, t1.txt, "\n", sep="")
  
  # 2. degrees of freedom
  t0.txt <- sprintf("  %-40s", "Degrees of freedom 1")
  t1.txt <- sprintf("  %10.3f", robust.fit$TRF[[1]][2]); 
  cat(t0.txt, t1.txt, "\n", sep="")
  t0.txt <- sprintf("  %-40s", "Degrees of freedom 2")
  t1.txt <- sprintf("  %10.3f", robust.fit$TRF[[1]][3]); 
  cat(t0.txt, t1.txt, "\n", sep="")
  
  # 3. P-value
  t0.txt <- sprintf("  %-40s", "P-value")
  t1.txt <- sprintf("  %10.3f", robust.fit$TRF[[1]][4])
  cat(t0.txt, t1.txt, "\n\n", sep="")
  if(std.nox) standardized <- TRUE   
  
  if(estimates) {       
    # local print function
    print.estimate <- function(name="ERROR", i=1, z.stat=TRUE) {
      
      # cut name if (still) too long
      name <- substr(name, 1, 13)
      
      if(!standardized) {
        if(is.na(se[i])) {
          txt <- sprintf("    %-13s %9.3f %8.3f\n", name, est[i], se[i])
        } else if(se[i] == 0) {
          txt <- sprintf("    %-13s %9.3f\n", name, est[i])
        } else if(est[i]/se[i] > 9999.999) {
          txt <- sprintf("    %-13s %9.3f %8.3f\n", name, est[i], se[i])
        } else if(!z.stat) {
          txt <- sprintf("    %-13s %9.3f %8.3f\n", name, est[i], se[i])
        } else {
          z <- est[i]/se[i]
          pval <- 2 * (1 - pnorm( abs(z) ))
          txt <- sprintf("    %-13s %9.3f %8.3f %8.3f %8.3f\n",
                         name, est[i], se[i], z, pval)
        }
      } else {
        if(is.na(se[i])) {
          txt <- sprintf("    %-13s %9.3f %8.3f                   %8.3f %8.3f\n", name, est[i], se[i], est.std[i], est.std.all[i])
        } else if(se[i] == 0) {
          txt <- sprintf("    %-13s %9.3f                            %8.3f %8.3f\n", name, est[i], est.std[i], est.std.all[i])
        } else if(est[i]/se[i] > 9999.999) {
          txt <- sprintf("    %-13s %9.3f %8.3f                   %8.3f %8.3f\n", name, est[i], se[i], est.std[i], est.std.all[i])
        } else if(!z.stat) {
          txt <- sprintf("    %-13s %9.3f %8.3f                   %8.3f %8.3f\n", name, est[i], se[i], est.std[i], est.std.all[i])
        } else {
          z <- est[i]/se[i]
          pval <- 2 * (1 - pnorm( abs(z) ))
          txt <- sprintf("    %-13s %9.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n",
                         name, est[i], se[i], z, pval, est.std[i], est.std.all[i])
        }
      }
      cat(txt)
    }
    
    est <- object@Fit@est
    se  <- object@Fit@se
    se[object@ParTable$free!=0]<-robust.se$se[[1]]
    
    for(g in 1:object@Data@ngroups) {
      ov.names <- lavaan:::vnames(object@ParTable, "ov", group=g)
      lv.names <- lavaan:::vnames(object@ParTable, "lv", group=g)
      
      # group header
      if(object@Data@ngroups > 1) {
        if(g > 1) cat("\n\n")
        cat("Group ", g, 
            " [", object@Data@group.label[[g]], "]:\n\n", sep="")
      }
      
      # estimates header
      if(!standardized) {
        cat("                   Estimate       SE  Z-value  P-value\n")
      } else {
        if(std.nox) {
          cat("                   Estimate  Std.err  Z-value  P(>|z|)   Std.lv  Std.nox\n")
        }
        else {
          cat("                   Estimate  Std.err  Z-value  P(>|z|)   Std.lv  Std.all\n")
        }
      }
      
      makeNames <- function(NAMES, LABELS) {
        # labels?
        l.idx <- which(nchar(LABELS) > 0L)
        if(length(l.idx) > 0L) {
          LABELS <- abbreviate(LABELS, 4)
          LABELS[l.idx] <- paste(" (", LABELS[l.idx], ")", sep="")
          MAX.L <- max(nchar(LABELS))
          NAMES <- abbreviate(NAMES, minlength = (13 - MAX.L), 
                              strict = TRUE)
          NAMES <- sprintf(paste("%-", (13 - MAX.L), "s%", MAX.L, "s",
                                 sep=""), NAMES, LABELS)
        } else {
          NAMES <- abbreviate(NAMES, minlength = 13, strict = TRUE)
        }
      }
      
      NAMES <- object@ParTable$rhs
      
      # 1a. indicators ("=~") (we do show dummy indicators)
      mm.idx <- which( object@ParTable$op == "=~" & 
        !object@ParTable$lhs %in% ov.names &
        object@ParTable$group == g)
      if(length(mm.idx)) {
        cat("Latent variables:\n")
        lhs.old <- ""
        NAMES[mm.idx] <- makeNames(  object@ParTable$rhs[mm.idx],
                                     object@ParTable$label[mm.idx])
        for(i in mm.idx) {
          lhs <- object@ParTable$lhs[i]
          if(lhs != lhs.old) cat("  ", lhs, " =~\n", sep="")
          print.estimate(name=NAMES[i], i)
          lhs.old <- lhs
        }
        cat("\n")
      }
      
      # 1b. formative/composites ("<~")
      fm.idx <- which( object@ParTable$op == "<~" &
        object@ParTable$group == g)
      if(length(fm.idx)) {
        cat("Composites:\n")
        lhs.old <- ""
        NAMES[fm.idx] <- makeNames(  object@ParTable$rhs[fm.idx],
                                     object@ParTable$label[fm.idx])
        for(i in fm.idx) {
          lhs <- object@ParTable$lhs[i]
          if(lhs != lhs.old) cat("  ", lhs, " <~\n", sep="")
          print.estimate(name=NAMES[i], i)
          lhs.old <- lhs
        }
        cat("\n")
      }
      
      # 2. regressions
      eqs.idx <- which(object@ParTable$op == "~" & object@ParTable$group == g)
      if(length(eqs.idx) > 0) {
        cat("Regressions:\n")
        lhs.old <- ""
        NAMES[eqs.idx] <- makeNames(  object@ParTable$rhs[eqs.idx],
                                      object@ParTable$label[eqs.idx])
        for(i in eqs.idx) {
          lhs <- object@ParTable$lhs[i]
          if(lhs != lhs.old) cat("  ", lhs, " ~\n", sep="")
          print.estimate(name=NAMES[i], i)
          lhs.old <- lhs
        }
        cat("\n")
      }
      
      # 3. covariances
      cov.idx <- which(object@ParTable$op == "~~" & 
        !object@ParTable$exo &
        object@ParTable$lhs != object@ParTable$rhs &
        object@ParTable$group == g)
      if(length(cov.idx) > 0) {
        cat("Covariances:\n")
        lhs.old <- ""
        NAMES[cov.idx] <- makeNames(  object@ParTable$rhs[cov.idx],
                                      object@ParTable$label[cov.idx])
        for(i in cov.idx) {
          lhs <- object@ParTable$lhs[i]
          if(lhs != lhs.old) cat("  ", lhs, " ~~\n", sep="")
          print.estimate(name=NAMES[i], i)
          lhs.old <- lhs
        }
        cat("\n")
      }
      
      # 4. intercepts/means
      int.idx <- which(object@ParTable$op == "~1" & 
        !object@ParTable$exo &
        object@ParTable$group == g)
      if(length(int.idx) > 0) {
        cat("Intercepts:\n")
        NAMES[int.idx] <- makeNames(  object@ParTable$lhs[int.idx],
                                      object@ParTable$label[int.idx])
        for(i in int.idx) {
          print.estimate(name=NAMES[i], i)
        }
        cat("\n")
      }
      
      # 5. (residual) variances
      var.idx <- which(object@ParTable$op == "~~" &
        !object@ParTable$exo &
        object@ParTable$lhs == object@ParTable$rhs &
        object@ParTable$group == g)
      if(length(var.idx) > 0) {
        cat("Variances:\n")
        NAMES[var.idx] <- makeNames(  object@ParTable$rhs[var.idx],
                                      object@ParTable$label[var.idx])
        for(i in var.idx) {
          if(object@Options$mimic == "lavaan") {
            print.estimate(name=NAMES[i], i, z.stat=TRUE)
          } else {
            print.estimate(name=NAMES[i], i, z.stat=TRUE)
          }
        }
        cat("\n")
      }
      
    } # ngroups
    
    # 6. variable definitions
    def.idx <- which(object@ParTable$op == ":=")
    if(length(def.idx) > 0) {
      if(object@Data@ngroups > 1) cat("\n")
      cat("Defined parameters:\n")
      NAMES[def.idx] <- makeNames(  object@ParTable$lhs[def.idx], "")
      for(i in def.idx) {
        print.estimate(name=NAMES[i], i)
      }
      cat("\n")
    }
    
    # 7. constraints
    cin.idx <- which((object@ParTable$op == "<" | 
      object@ParTable$op == ">"))
    ceq.idx <- which(object@ParTable$op == "==")
    if(length(cin.idx) > 0L || length(ceq.idx) > 0L) {
      # set small negative values to zero, to avoid printing " -0.000"
      slack <- ifelse(abs(est) < 1e-5, 0, est)
      #slack[cin.idx] <- object@Model@cin.function(object@Fit@x)
      #slack[ceq.idx] <- object@Model@ceq.function(object@Fit@x)
      
      if(object@Data@ngroups > 1 && length(def.idx) == 0L) cat("\n")
      cat("Constraints:                               Slack (>=0)\n")
      for(i in c(cin.idx,ceq.idx)) {
        lhs <- object@ParTable$lhs[i]
        op <- object@ParTable$op[i]
        rhs <- object@ParTable$rhs[i]
        if(rhs == "0" && op == ">") {
          con.string <- paste(lhs, " - 0", sep="")
        } else if(rhs == "0" && op == "<") {
          con.string <- paste(rhs, " - (", lhs, ")", sep="")
        } else if(rhs != "0" && op == ">") {
          con.string <- paste(lhs, " - (", rhs, ")", sep="")
        } else if(rhs != "0" && op == "<") {
          con.string <- paste(rhs, " - (", lhs, ")", sep="")
        } else if(rhs == "0" && op == "==") {
          con.string <- paste(lhs, " - 0", sep="")
        } else if(rhs != "0" && op == "==") {
          con.string <- paste(lhs, " - (", rhs, ")", sep="")
        }
        con.string <- abbreviate(con.string, 41, strict = TRUE)
        txt <- sprintf("    %-41s %8.3f\n", 
                       con.string, slack[i])
        cat(txt)
      }   
      cat("\n")
    }
    
  } # parameter estimates
  
  
  # R-square?
  if(rsquare) {
    r2 <- rsquare(object, est.std.all=est.std.all)
    if(object@Data@ngroups == 1L) r2 <- list(r2)
    for(g in 1:object@Data@ngroups) {
      if(object@Data@ngroups > 1) {
        cat("R-Square Group ", object@Data@group.label[[g]], 
            ":\n\n", sep="")       
      } else {
        cat("R-Square:\n\n")
      } 
      for(i in 1:length(r2[[g]])) {
        t1.txt <- sprintf("    %-13s %9.3f\n", names(r2[[g]])[i], 
                          r2[[g]][i])
        cat(t1.txt)
      }
      if(g < object@Data@ngroups) cat("\n")
    }
  }
  
  # modification indices?
  if(modindices) {
    cat("Modification Indices:\n\n")
    print( modificationIndices(object, standardized=TRUE) )
  }
  
}


## Function to calculate the robust standard errors
## object: an lavaan output
## gamma: robust gamma matrix for saturated mean and covariance
rsem.switch.gamma<-function(gamma, ov.names){
	gamma.old.names<-rownames(gamma)
	gamma.new.name<-ov.names
    k<-length(ov.names)
    for (i in 1:k){
      for (j in i:k){
        temp.name<-paste(ov.names[i], ".", ov.names[j], sep="")
        if (temp.name %in% gamma.old.names){
        	gamma.new.name <- c(gamma.new.name, temp.name)
        }else{
        	gamma.new.name <- c(gamma.new.name, paste(ov.names[j], ".", ov.names[i], sep=""))
        }
      }
    }     
	gamma.new<-gamma[gamma.new.name, gamma.new.name]
	gamma.new
}


rsem.se<-function(object, gamma){
  if (!is.list(gamma)){
  	temp<-gamma
  	gamma<-NULL
  	gamma[[1]]<-temp 	
  }
  samplestats<-object@SampleStats
  
  ## calculate the W and Delta matrices
  WD<-lavaan:::computeExpectedInformation(object@Model, object@SampleStats, extra=TRUE)
  Delta <- attr(WD, "Delta")
  WLS.V <- attr(WD, "WLS.V")
  
  ## a different way to calculate W and Delta
  ## lavaan:::compute.Abeta.complete(object@Fit@Sigma.hat)
  ## lavaan:::computeDelta(object@Model)
  ## the covariance matrix for the parameters
  vcov <- vector("list", length=samplestats@ngroups)
  se <- vector("list", length=samplestats@ngroups)
  for(g in 1:samplestats@ngroups) {
  	gamma[[g]]<-rsem.switch.gamma(gamma[[g]], object@Data@ov.names[[g]])
    A<-t(Delta[[g]])%*%WLS.V[[g]]%*%Delta[[g]]
    B<-t(Delta[[g]])%*%WLS.V[[g]]%*%gamma[[g]]%*%WLS.V[[g]]%*%Delta[[g]]
    D<-solve(A)
    vcov[[g]] <- D%*%B%*%D
    ## calculate the sandwich type standard errors
    se[[g]]<-sqrt(diag(vcov[[g]])/(object@SampleStats@nobs[[g]]-1))
  }
  
  ## return to the standard errors and variance and covariance matrix
  list(se=se, vcov=vcov, delta=Delta, W=WLS.V)
}


## Test statistics
## Trml, Taml, Tcradf, Trf

rsem.fit<-function(object, gamma, musig){
  if (!is.list(gamma)){
    temp<-gamma
    gamma<-NULL
    gamma[[1]]<-temp 	
  }
  
  samplestats<-object@SampleStats
  for(g in 1:samplestats@ngroups) {
  	gamma[[g]]<-rsem.switch.gamma(gamma[[g]], object@Data@ov.names[[g]])
  }
  ## the TRML statistic
  
  ## calculate the W and Delta matrices
  WD<-lavaan:::computeExpectedInformation(object@Model, object@SampleStats, extra=TRUE)
  Delta <- attr(WD, "Delta")
  WLS.V <- attr(WD, "WLS.V")
  
  ngroups<-samplestats@ngroups
  TRML<-TAML<-TCRADF<-TRF <- vector("list", length=samplestats@ngroups)
  ## need to decide whether n or n-1 used here.
  ## object@Fit@test[[1]]$stat<-object@Fit@test[[1]]$stat*87/88
  ## for TRML & TAML  
  for (g in 1:ngroups){
    A<-t(Delta[[g]])%*%WLS.V[[g]]%*%Delta[[g]]    
    D<-solve(A)
    U<-WLS.V[[g]] - WLS.V[[g]]%*%Delta[[g]]%*%D%*%t(Delta[[g]])%*%WLS.V[[g]]
    df<-object@Fit@test[[g]]$df
    n<-object@SampleStats@nobs[[g]]
    n1<-n-1
    ## for TRML
    trUT<-sum(diag(U%*%gamma[[g]]))
    m<-df/trUT
    trml<-m*object@Fit@test[[g]]$stat
    df.rml<-df
    p.rml<-1-pchisq(trml, df)
    temp<-c(trml, df.rml, p.rml)
    names(temp)<-c('Statistic','df','p-value')
    TRML[[g]]<-temp
    ## for TAML
    trUT2<-sum(diag(U%*%gamma[[g]]%*%U%*%gamma[[g]]))
    m1<-trUT/trUT2
    m2<-(trUT)^2/trUT2
    taml<-m1*object@Fit@test[[g]]$stat
    p.aml<-1-pchisq(taml, m2)
    temp<-c(taml, m2, p.aml)
    names(temp)<-c('Statistic','df','p-value')
    TAML[[g]]<-temp
  }
  
  ## CRADF and RF
  for (g in 1:ngroups){
    D<-solve(gamma[[g]])
    B<-solve(t(Delta[[g]])%*%D%*%Delta[[g]])
    Q<-D - D%*%Delta[[g]]%*%B%*%t(Delta[[g]])%*%D
    ovnames<-object@Data@ov.names[[g]]
    sigmahat<-musig$sigma[ovnames,ovnames]
    muhat<-musig$mu[ovnames]
    if (object@Model@meanstructure){
      r<-c(muhat-object@Fit@Mu.hat[[g]], vech(sigmahat-object@Fit@Sigma.hat[[g]]))
    }else{
      r<-vech(sigmahat-object@Fit@Sigma.hat[[g]])
    }
    r<-matrix(r, length(r), 1)
    radf<-t(r)%*%Q%*%r
    n<-object@Data@nobs[[g]]
    tcradf<-radf*n1/(1+radf)
    df<-object@Fit@test[[g]]$df
    p.cradf<-1-pchisq(tcradf, df)
    temp<-c(tcradf, df, p.cradf)
    names(temp)<-c('Statistic','df','p-value')
    TCRADF[[g]]<-temp
    
    ## for RF
    trf<-(n-df)*n1*radf/(n1*df)
    p.rf<-1-pf(trf, df, n-df)
    temp<-c(trf, df, n-df, p.rf)
    names(temp)<-c('Statistic','df1','df2','p-value')
    TRF[[g]]<-temp
  }
  return(list(TRML=TRML, TAML=TAML, TCRADF=TCRADF, TRF=TRF))
}
