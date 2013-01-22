library(RJSONIO)

checkIfFunctionExist <- function(fname) {
	list <- getFunctionList()
	for(i in 1:length(list)) {
		if(list[i] == fname) return(TRUE)
	}
	return(FALSE)
}

execute <- function(method, data) {
	do.call(method,list(data))
}

getSource <- function(fname) {
	source(file.path(paste('./methods/',fname,".R",sep="")))
}

getImageCommand <- function(fname) {
	source(file.path(paste('./images/',fname,".R",sep="")))
}

sourceFunctions <- function(list) {
	for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
	       if(trace) cat(nm,":")           
	       source(file.path(path, nm), ...)
	       if(trace) cat("\n")
	    }
}
 
sendResponse <- function(type,obj) {
	if(type=="image") {
		setContentType("image/jpg")
		# set header for the image
		setHeader('Content-Length',file.info(obj)$size)
		# send binary
		sendBin(readBin(obj,'raw',n=file.info(obj)$size))
		# send the binary data to garbage
		unlink(obj)
	}
	else if(type == "json") {
		setContentType("application/json")
		# 
		cat(toJSON(obj))
	}
	DONE
}

# temp
verifyRequest <- function(vars) {
	TRUE
}