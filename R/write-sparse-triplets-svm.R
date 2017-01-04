# write a simple triple matrix to svm format, with

# y should be given separately, not part of the sparse matrix



calc_stm_svm <- function(stm, y){
  # returns a character vector of length y ready for writing in svm format
  if(class(stm) != "simple_triplet_matrix"){
    stop("stm must be a simple triple matrix")
  }
  if(!is.vector(y) | nrow(stm) != length(y)){
    stop("y should be a vector of length equal to number of rows of stm")
  }
  n <- length(y)
  
  out <- character(n)
  for(k in 1:n){
    whichi <- stm$i==k
    out[k] <- paste(y[k], paste(paste(stm$j[whichi], stm$v[whichi], sep=":"), collapse = " "))
  }
  return(out)
}

write_stm_svm <- function(stm, y, file){
  out <- calc_stm_svm(stm, y)  
  writeLines(out, con = file)
}
