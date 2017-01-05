# Convert a simple triplet matrix to svm format
# Not exported
# author Peter Ellis
# returns a character vector of length n=nrow(stm)
#' @import slam
#' @import foreach
#' @import doParallel
calc_stm_svm <- function(stm, y = rep(1, nrow(stm)), parallel = FALSE, num.cores = detectCores()){
  # returns a character vector of length y ready for writing in svm format
  if(!"simple_triplet_matrix" %in% class(stm)){
    stop("stm must be a simple triple matrix")
  }
  if(!is.vector(y) | nrow(stm) != length(y)){
    stop("y should be a vector of length equal to number of rows of stm")
  }
  n <- length(y)
  if(parallel){
    cluster <- makeCluster(num.cores) # leave one CPU spare...
    registerDoParallel(cluster)

    
        
    clusterExport(cluster, "stm")
    out <- foreach(k = 1:n, .combine = c) %dopar% {
      whichi <- stm$i==k
      paste(y[k], paste(paste(stm$j[whichi], stm$v[whichi], sep=":"), collapse = " "))
    }
    stopCluster(cluster)
  } else {
    out <- foreach(k = 1:n, .combine = c) %do% {
      whichi <- stm$i==k
      paste(y[k], paste(paste(stm$j[whichi], stm$v[whichi], sep=":"), collapse = " "))
    }
  }
  return(out)
}


#' @export
#' @param stm a simple triplet matrix (class exported slam) of features (ie explanatory variables)
#' @param y a vector of labels.  If not provided, a dummy of 1s is provided
#' @param file file to write to.
#' @import slam
#' @author Peter Ellis
write_stm_svm <- function(stm, y, file){
  out <- calc_stm_svm(stm, y)  
  writeLines(out, con = file)
}
