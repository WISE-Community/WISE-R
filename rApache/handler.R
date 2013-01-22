source(file.path("./functions.R"))

type <- NULL
vars <- NULL

# POST only
if(is.null(GET)) {
	if(is.null(POST$rdata)) {
		sendResponse("json",list(output=NULL,result="no rdata"))
	} else {
		type <- as.character(POST$rtype)
		vars <- try( fromJSON(as.character(POST$rdata)) )
		
		if(type == "rstat") {
			# verify input
			result <- try(verifyRequest(vars)) 
			if(class(result) == "try-error") {
				#
				sendResponse("json",list(output=NULL,result="invalid data"))
			} else {
				#
				result <- try( getSource(vars[["method"]]) ) 
				if(class(result) == "try-error") {
					sendResponse("json",list(output=NULL,result="unavailable method"))
				} else {
					output <- try( execute(vars[["method"]], vars[["data"]]) )
					if(class(output) == "try-error") {
						sendResponse("json",list(output=NULL,result="excecute error"))
					} else {
						sendResponse("json",list(output=type,result="success"))
					}
				}
			}
		} else { # rimage
			result <- try( getImageCommand(vars[["format"]]) )
			if(class(result) == "try-error") {
				sendResponse("json",list(output=NULL,result="unavailable method"))
			} else {
				t <- tempfile()
				width <- as.numeric(vars[["width"]])
				height <- as.numeric(vars[["height"]])
				if(class(width) == "try-error" || class(height) == "try-error") {
					sendResponse("json",list(output=NULL,result="invalid width and height"))
				} else {
					jpeg(t, width = width, height = height)
						result <- try( execute(vars[["format"]], vars[["data"]]) ) 
					dev.off()

					if(class(result) == "try-error") {
						sendResponse("json",list(output=NULL,result="invalid data"))
					} else {
						setContentType("image/jpg")
						# set header for the image
						setHeader('Content-Length',file.info(t)$size)
						# send binary
						sendBin(readBin(t,'raw',n=file.info(t)$size))
					}
				}
				# send the binary data to garbage
				unlink(t)
			}
		}
	}
}



DONE





