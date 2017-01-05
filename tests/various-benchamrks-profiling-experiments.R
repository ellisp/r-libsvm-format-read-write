


x <- rnorm(100000)
i <- x > 0
w <- which(x > 0)

microbenchmark(
  sum(x[i]),
  sum(x[w])
)



x <- c("cat", "dog", "tiger")
paste(x, x)
y <- cat(x)
cat
sprintf(x)
paste(x)

microbenchmark(
  sprintf(x),
  paste(x, collapse = " ")
)
