rate<-function(package="base", download=NULL, like=NULL, rating=NULL, comment=NULL, lib.loc = NULL){
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only rate a package you installed already. Thanks. ", sQuote(package))
	}else{
		meta <- packageDescription(pkg = package)
		meta <- paste(meta$Version, ";", meta$Built, ";", meta$Maintainer, ";", meta$Repository)
		if (!is.null(download)) download <- '1'
		if (!is.null(like)) like <- '1'
		if (!is.null(rating)){
			if (!(rating %in% 1:5)) stop('The rating has to be 1 from 5.')
			rating <- as.character(rating)
		}			
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		if (dir == ""){	
			URL<-paste('http://rstats.psychstat.org/rateGet.php?name=', package, '&download=', download, '&like=', like, '&rating=', rating, '&comment=',URLencode(comment,TRUE), '&meta=', URLencode(meta,TRUE), sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			postForm("http://rstats.psychstat.org/rate.php", name=package, download=download, like=like, rating=rating, comment=comment, meta=meta)
			cat('Thanks for your feedback!\n')
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
		rating<-getURL(paste('http://rstats.psychstat.org/view.php?name=',  package, sep=''))
		rate<-strsplit(rating, "\n")[[1]]
		nrate<-length(rate)
		if (nrate>2){
			for (i in 1:3) cat(rate[i], "\n")
			cat("\n")		
			if (comment){
				if (nrate==3) stop("No comment available yet")
				nd<-nrate-3
				cat("There are ", nd, " comments in total.\n")
				if (max(ncomment)>nd) ncomment<-1:nd
				for (i in (ncomment+3)) cat(rate[i], "\n")
			}
		}else{
			cat(rating)	
		}
	}		
}

