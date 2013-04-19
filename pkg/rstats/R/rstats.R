## R function to rate the package
download<-function(package="base",lib.loc = NULL){
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only rate a package you installed already. Thanks. ", sQuote(package))
	}else{
		meta <- packageDescription(pkg = package)
		meta <- URLencode(paste(meta$Version, ";", meta$Built, ";", meta$Maintainer, ";", meta$Repository), TRUE)
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		if (dir == ""){
			URL<-paste('http://rstats.psychstat.org/rate.php?type=1&name=',  package, '&meta=', meta, sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			getForm("http://rstats.psychstat.org/rate.php", type='1', name=package, meta=meta, rcurl='1')
		}
	}
	cat("Thanks for your feedback!\n")
}

like<-function(package="base",lib.loc = NULL){
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only rate a package you installed already. Thanks. ", sQuote(package))
	}else{
		meta <- packageDescription(pkg = package)
		meta <- URLencode(paste(meta$Version, ";", meta$Built, ";", meta$Maintainer, ";", meta$Repository), TRUE)
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		if (dir == ""){
			URL<-paste('http://rstats.psychstat.org/rate.php?type=2&name=', package, '&meta=', meta, sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			getForm("http://rstats.psychstat.org/rate.php", type='2', name=package, meta=meta, rcurl='1')
		}
	}		
	cat("Thanks for your feedback!\n")
}

dislike<-function(package="base",lib.loc = NULL){
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only rate a package you installed already. Thanks. ", sQuote(package))
	}else{
		meta <- packageDescription(pkg = package)
		meta <- URLencode(paste(meta$Version, ";", meta$Built, ";", meta$Maintainer, ";", meta$Repository), TRUE)
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		if (dir == ""){
			URL<-paste('http://rstats.psychstat.org/rate.php?type=3&name=', package, '&meta=', meta, sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			getForm("http://rstats.psychstat.org/rate.php", type='3', name=package, meta=meta, rcurl='1')
		}
	}
	cat("Thanks for your feedback!\n")	
}

rate<-function(package="base",rating=5,lib.loc = NULL){
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only rate a package you installed already. Thanks. ", sQuote(package))
	}else{
		if (rating %in% 1:5){	
			meta <- packageDescription(pkg = package)
			meta <- URLencode(paste(meta$Version, ";", meta$Built, ";", meta$Maintainer, ";", meta$Repository), TRUE)
			dir <- system.file(package = "RCurl", lib.loc = lib.loc)
			if (dir == ""){
				URL<-paste( 'http://rstats.psychstat.org/rate.php?type=4&name=', package, '&rating=', rating, '&meta=', meta, sep='')
				browseURL(URL)
			}else{
				library('RCurl')
				getForm("http://rstats.psychstat.org/rate.php", type='4', name=package, rating=rating, meta=meta, rcurl='1')
			}
				cat("Thanks for your feedback!\n")
		}else{
			cat('The rating has to be 1 from 5.')
		}
	}
}

Comment<-function(package="base",comment=NULL,lib.loc = NULL){ 
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only rate a package you installed already. Thanks. ", sQuote(package))
	}else{
		if (!is.null(comment)){
			meta <- packageDescription(pkg = package)
			meta <- URLencode(paste(meta$Version, ";", meta$Built, ";", meta$Maintainer, ";", meta$Repository), TRUE)
			dir <- system.file(package = "RCurl", lib.loc = lib.loc)
			if (dir == ""){	
				URL<-paste('http://rstats.psychstat.org/rate.php?type=5&name=', package, '&comment=',URLencode(comment, TRUE), '&meta=', meta, sep='')
				browseURL(URL)
			}else{
				library('RCurl')
				getForm("http://rstats.psychstat.org/rate.php", type='5', name=package, comment=URLencode(comment, TRUE), meta=meta, rcurl='1')
				cat('Thanks for your feedback!\n')
			}
		}else{
		  cat('Please provide your comment first.')
		}
	}		
}

view<-function(package="base",comment=FALSE, ncomment=1:5, lib.loc = NULL){
	dir <- system.file(package = "RCurl", lib.loc = lib.loc)
	if (dir == ""){
		URL<-paste('http://rstats.psychstat.org/comments.php?name=',  package, sep='')
		browseURL(URL)
	}else{
		library('RCurl')
		rating<-getURL(paste('http://rstats.psychstat.org/comments.php?name=',  package, sep=''))
		temp<-strsplit(rating, "\n")
		rate<-temp[[1]][2]
		rate<-unlist(strsplit(rate, ";", fixed=TRUE))
		for (i in 1:4){
			cat(rate[i], "\n")
		}
		cat("\n")
		if (comment){
			if (length(temp[[1]])==3) stop("No comment available yet")
			a<-temp[[1]][4]
			b<-strsplit(a, "</tr><tr>", fixed=TRUE)
			d<-strsplit(b[[1]], "</td> <td>", fixed=TRUE)
			nd<-length(d)-1
			cat("There are ", nd, " comments in total.\n")
			if (max(ncomment)>nd){
				ncomment<-1:nd
			}
			for (i in (ncomment+1)){
				txt<-d[[i]]
				txt<-sub("</td>","",txt[2],fixed=TRUE)
				txt<-sub("</tr>","",txt,fixed=TRUE)
				cat(i-1, txt, "\n")
			}
		}
	}		
}