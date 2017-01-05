library(testthat)
library(slam)
library(data.table)

source("R/f_write.libsvm.r")
source("R/f_read.libsvm.r")
source("R/write-sparse-triplets-svm.R")

#================read and write, using dense matrix in R================
input_file = 'data/example.csv'
output_file = 'tests/test_dense.txt'
label_index = 1

dense <- read.csv( input_file, header = FALSE)

stm <- as.simple_triplet_matrix(dense)

write.libsvm( output_file, dense[ , label_index], dense[ , -label_index] )

read_test <- read.libsvm(output_file, dimensionality = 7)
expect_equal(sum(dense - read_test), 0)
expect_equal(dim(read_test), dim(dense))             


#===================write from sparse matrix, read back to dense matrix==================
# using dense as defined in tests/test_dense_sparse.R
stm <- as.simple_triplet_matrix(dense[ , -1])
y <- dense[ ,1]
calc_stm_svm(stm, y = y)

write_stm_svm(stm, y, "tests/test_stm.txt")

expect_equal(
  readLines("tests/test_stm.txt"),
  readLines("tests/test_dense.txt")
)

expect_equal(
  read.libsvm("tests/test_stm.txt", dimensionality = 7),
  read.libsvm("tests/test_dense.txt", dimensionality = 7)
)

