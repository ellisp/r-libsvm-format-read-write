# Convert a simple triplet matrix to svm format
# Not exported
# author Peter Ellis
# returns a character vector of length n=nrow(stm)
#' @import slam
calc_stm_svm <- function(stm, y){
  # returns a character vector of length y ready for writing in svm format
  if(!"simple_triplet_matrix" %in% class(stm)){
    stop("stm must be a simple triple matrix")
  }
  if(!is.vector(y) | nrow(stm) != length(y)){
    stop("y should be a vector of length equal to number of rows of stm")
  }
  n <- length(y)
  i <- stm$i
  j <- stm$j
  v <- stm$v
  # Notes, this is the expensive bit.  Here's what we've found so far:
  # Parallelization with foreach and doParallel made no improvement.
  # Changing from for() to sapply made a marginal (3.5%) speed gain.
  # Defining i, j and v as their own vectors rather than calling them via stm$i, stm$j and stm$v made another 3.5% gain
  out <- sapply(1:n, function(k){
    whichi <- which(i == k)
    paste(paste(j[whichi], v[whichi], sep = ":"), collapse = " ")
  })
  
  out <- paste(y, out)
  
  return(out)
}


#' @export
#' @param stm a simple triplet matrix (class exported slam) of features (ie explanatory variables)
#' @param y a vector of labels.  If not provided, a dummy of 1s is provided
#' @param file file to write to.
#' @import slam
#' @author Peter Ellis
write_stm_svm <- function(stm, y = rep(1, nrow(stm)), file){
  out <- calc_stm_svm(stm, y)  
  writeLines(out, con = file)
}
