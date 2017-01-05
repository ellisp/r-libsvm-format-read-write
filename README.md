R code for reading and writing files in libsvm format
=====================================================

The functions for reading and writing _libsvm_ sparse format from R, rather slow in the original dense matrix form. See _read_example.r_ and _write_example.r_ for usage.

Now includes `write_stm_svm()` which is blisteringly fast, for objects of class `simple_triplet_matrix` (eg 300,000 x 90,000 sparse matrix in 2 minutes).

At the moment this is just a bunch of functions but I might turn it into an R package.
