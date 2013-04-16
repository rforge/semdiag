## R function to rate the package
download<-function(package,...){
	pkgpath <- find.package(package, ...)
    if (length(pkgpath) == 0L) {
		gettextf("there is no package called %s", sQuote(package))
	}else{
		pkgpath <- find.package('RCurl', ...)
		if (length(pkgpath) == 0L){
			URL<-paste('http://rstats.psychstat.org/rate.php?type=1&name=',  package, sep='')
		browseURL(URL)
		#URL
		}else{
			library('RCurl')
			getForm("http://rstats.psychstat.org/rate.php", type='1', name=package, rcurl='1')
		}
	}		
}

like<-function(package,...){
	pkgpath <- find.package(package, ...)
    if (length(pkgpath) == 0L) {
		gettextf("there is no package called %s", sQuote(package))
	}else{
		pkgpath <- find.package('RCurl', ...)
		if (length(pkgpath) == 0L){
		URL<-paste('http://rstats.psychstat.org/rate.php?type=2&name=', package, sep='')
		browseURL(URL)
		#URL
		}else{
			library('RCurl')
			getForm("http://rstats.psychstat.org/rate.php", type='2', name=package, rcurl='1')
		}
	}		
}

dislike<-function(package,...){
	pkgpath <- find.package(package, ...)
    if (length(pkgpath) == 0L) {
		gettextf("there is no package called %s", sQuote(package))
	}else{
		pkgpath <- find.package('RCurl', ...)
		if (length(pkgpath) == 0L){
			URL<-paste('http://rstats.psychstat.org/rate.php?type=3&name=', package, sep='')
		browseURL(URL)
		#URL
		}else{
			library('RCurl')
			getForm("http://rstats.psychstat.org/rate.php", type='3', name=package, rcurl='1')
		}
	}		
}

rate<-function(package,rating,...){
	
	pkgpath <- find.package(package, ...)
    if (length(pkgpath) == 0L) {
		gettextf("there is no package called %s", sQuote(package))
	}else{
		if (rating %in% 1:5){	
			pkgpath <- find.package('RCurl', ...)
		if (length(pkgpath) == 0L){
			URL<-paste( 'http://rstats.psychstat.org/rate.php?type=4&name=', package, '&rating=',rating,sep='')
		browseURL(URL)
		#URL
		}else{
			library('RCurl')
			getForm("http://rstats.psychstat.org/rate.php", type='4', name=package, rating=rating, rcurl='1')
		}
		}else{
		  cat('The rating has to be 1 from 5.')
		}
	}		
}

Comment<-function(package,comment,...){
	
	pkgpath <- find.package(package, ...)
    if (length(pkgpath) == 0L) {
		gettextf("there is no package called %s", sQuote(package))
	}else{
		if (comment!=''){
		pkgpath <- find.package('RCurl', ...)
		if (length(pkgpath) == 0L){	URL<-paste('http://rstats.psychstat.org/rate.php?type=5&name=', package, '&comment=',URLencode(comment, TRUE),sep='')
		browseURL(URL)
		#URL
		}else{
			library('RCurl')
			getForm("http://rstats.psychstat.org/rate.php", type='5', name=package, comment=URLencode(comment, TRUE), rcurl='1')
		}
		}else{
		  cat('The rating has to be 1 from 5.')
		}
	}		
}

view<-function(package,...){
	pkgpath <- find.package('RCurl', ...)
	if (length(pkgpath) == 0L){			URL<-paste('http://rstats.psychstat.org/comments.php?name=',  package, sep='')
		browseURL(URL)
		#URL
	}else{
		library('RCurl')
					rating<-getURL(paste('http://rstats.psychstat.org/comments.php?name=',  package, sep=''))
		temp<-strsplit(rating, "\n")
		temp<-temp[[1]][2]
		temp<-unlist(strsplit(temp, ";", fixed=TRUE))
		#rating<-NULL
		for (i in 1:4){
			#rating<-c(rating, unlist(strsplit(temp[i], ":", fixed=TRUE))[2])
			cat(temp[i], "\n")
		}
	}		
}