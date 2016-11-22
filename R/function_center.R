
#### center one matrix only, and return the centered matrix #############
demean <- function(x,columncenters_vect=NULL){
	if(is.null(columncenters_vect)){
		means <- x - matrix(rep(colMeans(x),each = nrow(x)),nrow(x),ncol(x))
	}else{
		means <- x - matrix(rep(columncenters_vect,each = nrow(x)),nrow(x),ncol(x))
	}
	return(means)
}