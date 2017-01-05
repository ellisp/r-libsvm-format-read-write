library(data.table)
library(tidyverse)
library(tidytext)
library(SnowballC)
# this relates to script 0076

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
nyt_samp <- nyt_M[1:30000, ]

# How long does it take just to create the character vector that needs to be written to disk?
system.time({
  nyt_svm <- calc_stm_svm(nyt_samp)
})
# Doesn't scale up well:
# 1000 rows in 1 second
# 5000 rows in 14  seconds
# 10000 rows in 59 seconds
# 20000 rows in 217 seconds 
# 30000 rows in 475 seconds

# How long does the whole process, including writing to disk, take?
# Turns out to be basically the same.  So the thing to optimise
# is the calc_stm_svm function.
system.time({
  write_stm_svm(nyt_samp, file = tempfile())
})
# 1000 rows in 1 second
# 5000 rows in 15 seconds
# 10000 rows in 56 seconds
# 20000 rows in 209 seconds
# 30000 rows in 469 seconds

bm <- data.frame(x = c(1,5,10,20,30) * 1000, y = c(1,15,56,209,469))
ggplot(bm, aes(x = x, y = y)) +geom_line()
mod1 <- lm(y ~ poly(x, 2), data = bm)
coef(mod1)
# Time in hours for a million rows:
predict(mod1, newdata = data.frame(x = 10 ^ 6))  / 3600
# Time in days for 50 million rows
predict(mod1, newdata = data.frame(x = 50 * 10 ^ 6))  / 3600 / 24

#============microbenchmarks when developing new version==========
nyt_samp <- nyt_M[sample(1:nrow(nyt_M), 1000), ]
microbenchmark(
  write_stm_svm(nyt_samp, file = tempfile()),
  write_stm_svm_bm(nyt_samp, file = tempfile())
)



dim(nyt_M)






# Import to H2O
h2o.init(nthreads = -1, max_mem_size = "10G")

