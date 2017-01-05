library(data.table)

# Convert a simple triplet matrix to svm format
#' @author Peter Ellis
#' @return a character vector of length n = nrow(stm)
#' @import slam
#' @import data.table
calc_stm_svm <- function(stm, y){
  # returns a character vector of length y ready for writing in svm format
  if(!"simple_triplet_matrix" %in% class(stm)){
    stop("stm must be a simple triple matrix")
  }
  if(!is.vector(y) | nrow(stm) != length(y)){
    stop("y should be a vector of length equal to number of rows of stm")
  }
  n <- length(y)
  
  # data.table solution thanks to @roland at http://stackoverflow.com/questions/41477700/optimising-sapply-or-for-paste-to-efficiently-transform-sparse-triplet-m/41478999#41478999
  stm2 <- data.table(i = stm$i, j = stm$j, v = stm$v)
  res <- stm2[, .(i, jv = paste(j, v, sep = ":"))
             ][order(i), .(res = paste(jv, collapse = " ")), by = i][["res"]]
  
  out <- paste(y, res)
  
  return(out)
}


#' @param stm a simple triplet matrix (class exported slam) of features (ie explanatory variables)
#' @param y a vector of labels.  If not provided, a dummy of 1s is provided
#' @param file file to write to.
#' @author Peter Ellis
write_stm_svm <- function(stm, y = rep(1, nrow(stm)), file){
  out <- calc_stm_svm(stm, y)  
  writeLines(out, con = file)
}
