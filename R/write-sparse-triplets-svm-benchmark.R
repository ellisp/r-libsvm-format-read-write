# Convert a simple triplet matrix to svm format
# Not exported
# author Peter Ellis
# returns a character vector of length n=nrow(stm)
#' @import slam
calc_stm_svm_bm <- function(stm, y){
  # returns a character vector of length y ready for writing in svm format
  if(!"simple_triplet_matrix" %in% class(stm)){
    stop("stm must be a simple triple matrix")
  }
  if(!is.vector(y) | nrow(stm) != length(y)){
    stop("y should be a vector of length equal to number of rows of stm")
  }
  n <- length(y)
  out <- character(n)
    for(k in 1:n) {
      whichi <- stm$i==k
      out[k] <- paste(y[k], paste(paste(stm$j[whichi], stm$v[whichi], sep=":"), collapse = " "))
  }
  
  return(out)
}


#' @export
#' @param stm a simple triplet matrix (class exported slam) of features (ie explanatory variables)
#' @param y a vector of labels.  If not provided, a dummy of 1s is provided
#' @param file file to write to.
#' @import slam
#' @author Peter Ellis
write_stm_svm_bm <- function(stm, y = rep(1, nrow(stm)), file){
  out <- calc_stm_svm_bm(stm, y)  
  writeLines(out, con = file)
}
