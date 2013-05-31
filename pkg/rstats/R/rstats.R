download<-function(package, email, name, meta=TRUE,  lib.loc = NULL){
	## check the package name
	if (missing(package)){
		if (is.null(options()$rstats)){
			stop('Package name is needed. You can provide it in the function or set it use setRstats().')
		}else{
			rstats.op<-options()$rstats
			package <- rstats.op[1]
		}
	}
	if (missing(email)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			email <- rstats.op[2]
		}else{
			email<-''	
		}
	}
	if (missing(name)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			user <- rstats.op[3]
		}else{
			user<-''	
		}
	}
	
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only rate a package you have installed. Thanks. ", sQuote(package))
	}else{
		uid<-URLencode(uniqueID(),TRUE)
		if (meta){
			meta <- packageDescription(pkg = package)
			meta <- paste(meta$Maintainer, ";", meta$Version, ";", meta$Built, ";", meta$Repository)
		}else{
			meta <- "Not provided"
		}
		meta<-URLencode(meta,TRUE)
		download <- '1'
		like<-NULL
		rating<-NULL	
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		comment <- URLencode("A user just downloaded your package. Congratulations!",TRUE)
		if (dir == ""){	
			URL<-paste('http://rstats.psychstat.org/rate.php?package=', package, '&download=', download, '&like=', like, '&rating=', rating, '&email=', email, '&user=', user, '&comment=',comment, '&uid=',uid, '&meta=', URLencode(meta,TRUE), sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			out<-postForm("http://rstats.psychstat.org/rate.php", package=package, download=download, like=like, rating=rating, comment=comment, email=email, user=user, uid=uid, meta=meta)
			cat(out)
			
			view(TRUE, package=package)
		}
	}
}

like<-function(package, email, name, meta=TRUE, lib.loc = NULL){
	## check the package name
	if (missing(package)){
		if (is.null(options()$rstats)){
			stop('Package name is needed. You can provide it in the function or set it use setRstats().')
		}else{
			rstats.op<-options()$rstats
			package <- rstats.op[1]
		}
	}
	if (missing(email)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			email <- rstats.op[2]
		}else{
			email<-''	
		}
	}
	if (missing(name)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			user <- rstats.op[3]
		}else{
			user<-''	
		}
	}
	
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only rate a package you have installed. Thanks. ", sQuote(package))
	}else{
		uid<-URLencode(uniqueID(),TRUE)
		if (meta){
			meta <- packageDescription(pkg = package)
			meta <- paste(meta$Maintainer, ";", meta$Version, ";", meta$Built, ";", meta$Repository)
		}else{
			meta <- "Not provided"
		}
		meta<-URLencode(meta,TRUE)
		download <- NULL
		like<-'1'
		rating<-NULL	
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		comment <- URLencode("A user liked your package. Congratulations!",TRUE)
		if (dir == ""){	
			URL<-paste('http://rstats.psychstat.org/rate.php?package=', package, '&download=', download, '&like=', like, '&rating=', rating, '&email=', email, '&user=', user, '&comment=',comment, '&uid=',uid, '&meta=', URLencode(meta,TRUE), sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			out<-postForm("http://rstats.psychstat.org/rate.php", package=package, download=download, like=like, rating=rating, comment=comment, email=email, user=user, uid=uid, meta=meta)
			cat(out)
			
			view(TRUE, package=package)
		}
	}
}

comment<-function(comment, package, email, name, meta=TRUE, lib.loc = NULL){
	## check the package name
	if (missing(package)){
		if (is.null(options()$rstats)){
			stop('Package name is needed. You can provide it in the function or set it use setRstats().')
		}else{
			rstats.op<-options()$rstats
			package <- rstats.op[1]
		}
	}
	if (missing(email)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			email <- rstats.op[2]
		}else{
			email<-''	
		}
	}
	if (missing(name)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			user <- rstats.op[3]
		}else{
			user<-''	
		}
	}
	
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only rate a package you have installed. Thanks. ", sQuote(package))
	}else{
		uid<-URLencode(uniqueID(),TRUE)
		if (meta){
			meta <- packageDescription(pkg = package)
			meta <- paste(meta$Maintainer, ";", meta$Version, ";", meta$Built, ";", meta$Repository)
		}else{
			meta <- "Not provided"
		}
		meta<-URLencode(meta,TRUE)
		download <- NULL
		like<-NULL
		rating<-NULL	
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		comment <- URLencode(comment,TRUE)
		if (dir == ""){	
			URL<-paste('http://rstats.psychstat.org/rate.php?package=', package, '&download=', download, '&like=', like, '&rating=', rating, '&email=', email, '&user=', user, '&comment=',comment, '&uid=',uid, '&meta=', URLencode(meta,TRUE), sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			out<-postForm("http://rstats.psychstat.org/rate.php", package=package, download=download, like=like, rating=rating, comment=comment, email=email, user=user, uid=uid, meta=meta)
			cat(out)
			
			view(TRUE, package=package)
		}
	}
}


rate<-function(rating=5, package, email, name, meta=TRUE,  lib.loc = NULL){
	## check the package name
	if (missing(package)){
		if (is.null(options()$rstats)){
			stop('Package name is needed. You can provide it in the function or set it use setRstats().')
		}else{
			rstats.op<-options()$rstats
			package <- rstats.op[1]
		}
	}
	if (missing(email)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			email <- rstats.op[2]
		}else{
			email<-''	
		}
	}
	if (missing(name)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			user <- rstats.op[3]
		}else{
			user<-''	
		}
	}
	
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only rate a package you have installed. Thanks. ", sQuote(package))
	}else{
		uid<-URLencode(uniqueID(),TRUE)
		if (meta){
			meta <- packageDescription(pkg = package)
			meta <- paste(meta$Maintainer, ";", meta$Version, ";", meta$Built, ";", meta$Repository)
		}else{
			meta <- "Not provided"
		}
		meta<-URLencode(meta,TRUE)
		download <- NULL
		like <- NULL
		if (!is.null(rating)){
			if (!(rating %in% 1:5)) stop('The rating has to be 1 from 5.')
			rating <- as.character(rating)
		}else{
			stop('You have to provide a rating between 1 and 5.')
		}			
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		comment <- URLencode("A user just rated your package.",TRUE)
		if (dir == ""){	
			URL<-paste('http://rstats.psychstat.org/rate.php?package=', package, '&download=', download, '&like=', like, '&rating=', rating, '&email=', email, '&user=', user, '&comment=',comment, '&uid=',uid, '&meta=', URLencode(meta,TRUE), sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			out<-postForm("http://rstats.psychstat.org/rate.php", package=package, download=download, like=like, rating=rating, comment=comment, email=email, user=user, uid=uid, meta=meta)
			cat(out)
			
			view(TRUE, package=package)
		}
	}
}

view<-function(comment=FALSE, ncomment=1:5, package, lib.loc = NULL){
	## check the package name
	if (missing(package)){
		if (is.null(options()$rstats)){
			stop('Package name is needed. You can provide it in the function or set it use setRstats().')
		}else{
			rstats.op<-options()$rstats
			package <- rstats.op[1]
		}
	}
	comment<-ifelse(comment, 1, 0)
	dir <- system.file(package = "RCurl", lib.loc = lib.loc)
	if (dir == ""){
		URL<-paste('http://rstats.psychstat.org/comments.php?package=',  package, sep='')
		browseURL(URL)
	}else{
		library('RCurl')
		rating<-getURL(paste('http://rstats.psychstat.org/view.php?package=',  package,  '&comment=', comment, sep=''))
		rate<-strsplit(rating, "\n")[[1]]
		nrate<-length(rate)
		if (nrate>2){
			for (i in 1:3) cat(rate[i], "\n")
			cat("\n")		
			if (comment){
				if (nrate==3) stop("No comment available yet")
				totalcomment<-nrate-3
				cat("There are ", totalcomment, " comments in total. The number in parenthesis is # of replies.\n")
				if (max(ncomment)>totalcomment) ncomment<-1:totalcomment
				for (i in (ncomment+3)) cat(rate[i], "\n")
			}
		}else{
			cat(rating)	
		}
	}		
}

ask<-function(comment=NULL, package,  email, name, cc, meta=TRUE, lib.loc = NULL){
	## check the package name
	if (missing(package)){
		if (is.null(options()$rstats)){
			stop('Package name is needed. You can provide it in the function or set it use setRstats().')
		}else{
			rstats.op<-options()$rstats
			package <- rstats.op[1]
		}
	}
	if (missing(email)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			email <- rstats.op[2]
		}else{
			email<-''	
		}
	}
	if (missing(name)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			user <- rstats.op[3]
		}else{
			user<-''	
		}
	}
	if (missing(cc)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			cc <- rstats.op[4]
		}else{
			cc<-''	
		}
	}
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only ask a question about a package you have installed. Thanks. ", sQuote(package))
	}else{
		uid<-URLencode(uniqueID(),TRUE)
		if (meta){
			meta <- packageDescription(pkg = package)
			meta <- paste(meta$Maintainer, ";", meta$Version, ";", meta$Built, ";", meta$Repository)
		}else{
			meta <- "Not provided"
		}
		meta<-	URLencode(meta,TRUE)
		comment<-URLencode(comment,TRUE)
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		if (dir == ""){	
			URL<-paste('http://rstats.psychstat.org/ask.php?name=', package, '&comment=',comment, '&email=', email, '&user=', user, '&cc=', cc, '&uid=',uid, '&meta=', meta, sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			out<-postForm("http://rstats.psychstat.org/ask.php", package=package, comment=comment, email=email, user=user, cc=cc, uid=uid, meta=meta)
			cat(out)
		}
	}
}


reply<-function(id, comment=NULL, package, email, name, cc, meta=TRUE, lib.loc = NULL){
	
	if (missing(id)) stop('The id of the question or comment is needed! Please use view() function to find out the id.')
		
		## check the package name
	if (missing(package)){
		if (is.null(options()$rstats)){
			stop('Package name is needed. You can provide it in the function or set it use setRstats().')
		}else{
			rstats.op<-options()$rstats
			package <- rstats.op[1]
		}
	}
	if (missing(email)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			email <- rstats.op[2]
		}else{
			email<-''	
		}
	}
	if (missing(name)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			user <- rstats.op[3]
		}else{
			user<-''	
		}
	}
	
	if (missing(cc)){
		if (!is.null(options()$rstats)){
			rstats.op<-options()$rstats
			cc <- rstats.op[4]
		}else{
			cc<-''	
		}
	}
	uid<-URLencode(uniqueID(),TRUE)
		if (meta){
			meta <- packageDescription(pkg = package)
			meta <- paste(meta$Maintainer, ";", meta$Version, ";", meta$Built, ";", meta$Repository)
		}else{
			meta <- "Not provided"
		}
		meta<-URLencode(meta,TRUE)
		comment<-URLencode(comment,TRUE)
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		if (dir == ""){	
			URL<-paste('http://rstats.psychstat.org/reply.php?package=', package, '&comment=',comment, '&email=', email, '&user=', user,  '&cc=', cc,  '&uid=',uid, '&meta=', meta, sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			out<-postForm("http://rstats.psychstat.org/reply.php", package=package, id=as.character(id), comment=comment, email=email, user=user, cc=cc, uid=uid, meta=meta)
			cat(out)			
			viewreply(id=id, package=package)
		}
}

viewreply<-function(id, package, lib.loc = NULL){
	if (missing(id)) stop('The id of the question or comment is needed! Please use view() function to find out the id.')
	if (missing(package)){
		if (is.null(options()$rstats)){
			stop('Package name is needed. You can provide it in the function or set it use setRstats().')
		}else{
			rstats.op<-options()$rstats
			package <- rstats.op[1]
		}
	}
	dir <- system.file(package = "RCurl", lib.loc = lib.loc)
	if (dir == ""){
		URL<-paste('http://rstats.psychstat.org/comments.php?name=',  package, sep='')
		browseURL(URL)
	}else{
		library('RCurl')
		rating<-getURL(paste('http://rstats.psychstat.org/viewreply.php?package=',  package,  '&id=', id, sep=''))		
		cat(rating)	
	}		
}

setRstats<-function(package='base', email='', name='', cc=''){
	options(rstats=c(package, email, name, cc))
}

uniqueID<-function(){
	## generate a unique id to avoid repeated rating
	p1<-Sys.getenv()
	p1<-paste(p1, collapse = "")
	p2<-Sys.info()
	p3<-paste(p2[c(1,2,3,5)],collapse=";")
	p2<-paste(p2, collapse='')	
		
	ind<-Sys.which("ipconfig")
	if (ind!=""){
		p5<-system("ipconfig", intern=TRUE)
	}else{
		p5<-system("ifconfig", intern=TRUE)
	}
	p5<-paste(p5, collapse="")
	p4<-paste(p1,p2,p5)
	
	n.p4<-nchar(p4)
	m.p4<-seq(1,n.p4, 3)
	
	p4<-unlist(strsplit(p4,''))
	p4.sub<-p4[m.p4]
	
	p4<-paste(p4.sub, collapse='')
	
	paste(p3,p4,sep=';')
}

