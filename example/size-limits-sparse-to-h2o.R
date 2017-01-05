library(microbenchmark)
library(data.table)
library(tidyverse)
library(tidytext)
library(SnowballC)

#==============larger test=====================
if(!"docword.nytimes.txt" %in% list.files(path = "data")){
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/bag-of-words/docword.nytimes.txt.gz",
              destfile = "data/docword.nytimes.txt.gz", mode = "wb")
  gunzip("data/docword.nytimes.txt.gz")
}



nyt <- fread("data/docword.nytimes.txt", skip = 3, sep = " ", nrows = -1)  
names(nyt) <- c("doc", "wordid", "count")

dim(nyt)
length(unique(nyt$doc)) #299752, should be about 300000
sum(nyt$count) # 99.542 million should be about 100 million

# 102660 words in the official vocab, but only 101636 distinct words in the data file.  So we need to be careful...
nyt_vocab <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/bag-of-words/vocab.nytimes.txt")$V1
vocab_df <- data.frame(word = nyt_vocab, wordid = 1:length(nyt_vocab))

# the wordStem part of this next operation pushes my 12GB RAM machine to its limit and takes 5-10 minutes
nyt2 <- nyt %>%
   # attach the names of words back to the words
   left_join(vocab_df, by = "wordid") %>%
   select(-wordid) %>%
   # stopwords are meant to be already removed but no harm in checking:
   anti_join(stop_words, by = "word") %>%
   # reduce words to their stems (otherwise abandon, abandoning, abandons all treated as different, etc):
   mutate(word = wordStem(word)) %>%
   group_by(doc, word) %>%
   summarise(count = sum(count)) 

# another expensive operation, about 2 minutes, just scrapes in with existing RAM:
nyt_M <- nyt2 %>%
   cast_dtm(doc, word, count)

# clean up
rm(nyt, nyt2); gc()

#================testing length of converting and writing to libsvm format=================
source("R/write-sparse-triplets-svm.R")
source("R/write-sparse-triplets-svm-benchmark.R")
nyt_samp <- nyt_M[1:nrow(nyt_M), ]

# How long does the whole process, including writing to disk, take?
# Note that the thing to optimise
# is the calc_stm_svm function.
system.time({
  write_stm_svm(nyt_samp, file = tempfile())
})
# 50000 rows in 17 
# 100000 rows in 36 seconds
# 200000 rows in 77 seconds
# 300000 rows in 115 seconds (full dataset!)

dim(nyt_M)






# Import to H2O
h2o.init(nthreads = -1, max_mem_size = "10G")

