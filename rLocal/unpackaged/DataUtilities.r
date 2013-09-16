#################################### PARSING TOOLS #######################################
## These functions are generic utilities not requiring wise data type
##
##

###
#   In some cases we have used an open response to accomodate multiple choice
#   by asking students to put an x to all that apply.  This will find those
#   strings to the left or right of the check.
#   strarr - is the array of strings to be processed
#   mark - what the student is supposed to put as a mark (e.g. x)
#   labels - a list of the possible labels being checked
#   header - any string we want to remove from the beginning of the string, "Eg check all that apply"
#   markRightOfLabel - boolean - should we be looking for the mark on the right side of a label?
#      #TODO implement left of label
#   Will return an array of strings where marked labels are separated by commas:
#   e.g.
#   school x
#   library
#   homeX
#   Returns "school, home"
###
parseOpenResponseForChecks = function(strarr, mark, labels, header = NULL, markRightOfLabel = TRUE){
	strarr.out = character();
	strarr = sub(header, "", strarr)
	for (str in strarr){
		if (markRightOfLabel){
			arr = strsplit(paste(str,"TAIL"),mark)[[1]]
			if (length(arr) > 1){
				# remove last item
				arr = arr[1:(length(arr)-1)]
				# iterate through each label looking for its place in
				# array, if two in multiple place in arr take second
				labelArr = rep("", length(arr))
				indexArr = rep(-1, length(arr))
				for (l in 1:length(labels)){
					label = labels[l]
					sindexes = regexpr(label, arr)
					if (length(which(sindexes > 0)) > 0){
						index = which(sindexes > 0)[1]
						if (indexArr[index] < sindexes[index]){
							indexArr[index] = sindexes[index]
							labelArr[index] = label
						}
					}
				}
				labelArr = labelArr[labelArr != ""]
			} else {
				labelArr = ""
			}
		}
		strarr.out = c(strarr.out, paste(labelArr, collapse = ","))
	}
	return(strarr.out)
}