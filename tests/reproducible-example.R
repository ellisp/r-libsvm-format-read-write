library(microbenchmark)

# create an example object like a simple_triple_matrix
# number of rows and columns in sparse matrix:
n <- 10 # real number is about 300,000
ncols <- 1000 # real number is about 80,000

# number of non-zero values, about 10 per row:
nonzerovalues <- n * 10

stm <- data.frame(
  i = sample(1:n, nonzerovalues, replace = TRUE),
  j = sample(1:ncols, nonzerovalues, replace = TRUE),
  v = sample(rpois(nonzerovalues, 5), replace = TRUE)
)

# It seems to save about 3% of time to have i, j and v objects in their own right
i <- stm$i
j <- stm$j
v <- stm$v

expensive <- function(){
  sapply(1:n, function(k){
  # microbenchmarking suggests quicker to have which() rather than a vector of TRUE and FALSE:
  whichi <- which(i == k)
  paste(paste(j[whichi], v[whichi], sep = ":"), collapse = " ")
})
}

expensive()

microbenchmark(expensive())


