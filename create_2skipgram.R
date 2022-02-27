# Make "skip gram" from sentence/last_word ngram data.table object

# E.g.
# the 3gram + last_word data.table below
# count     sentence   last_word
# 509    you have to          do
# become when skipping the -1 word in sentence
# count     sentence   last_word
# 509       you have          do
# become when skipping the -2 word in sentence
# count     sentence   last_word
# 509       you to            do

# Note : rows' which sentences became identical are merged.

# remove either n-1 or n-2 word in sentence (type = 1 or 2)
make_skip_gram <- function(ngram, type = 1){
  # for the moment use only with 4 grams (3gram sentence+lastword)  
  if(length(strsplit(ngram[1][,sentence], " ")[[1]]) != 3){
    stop("Please pass a 4 gram data.table")
  }
  if(type == 1){
    # simply remove the last word from the "sentence"
    ngram[, c("sentence") := tstrsplit(sentence, " (\\w+$)", perl=TRUE) ]
  }else if(type==2){
    # extract sentence's 1st and last word. Merge and reassign to "sentence"
    ngram[, c("skip1","skip3") := tstrsplit(sentence, ' \\w+ ', perl=TRUE)]
    ngram[, sentence:=paste(skip1,skip3,sep = " ")]
    ngram[, skip1:=NULL]
    ngram[, skip3:=NULL]
  }
  # Merge within the data.table the entries that became identical
  return(ngram[, .(count = sum(count)), by = c("sentence","last_word")])
}


# Merge the skipgram datatable together
# merge rows with same couple sentence-last_word, add up counts
merge_skipgram <- function(skipgram1, skipgram2){
  skipgram_all <- rbind(skipgram1, skipgram2)
  return(skipgram_all[, .(count = sum(count)), by = c("sentence","last_word")])
}

# load data.table for 3 corpus
blog4 <- read_rds("~/R/DataScience_Capstone/4-gram/blog_all_4-gram.Rdata")
news4 <- read_rds("~/R/DataScience_Capstone/4-gram/news_all_4-gram.Rdata")
twit4 <- read_rds("~/R/DataScience_Capstone/4-gram/twit_all_4-gram.Rdata")


blog4 <- make_skip_gram(blog4)
news4 <- make_skip_gram(news4)
twit4 <- make_skip_gram(twit4)

skip4 <- merge_skipgram(blog4,news4)
skip4 <- merge_skipgram(skip4,twit4)

blog42 <- make_skip_gram(blog4, type = 2)
news42 <- make_skip_gram(news4, type = 2)
twit42 <- make_skip_gram(twit4, type = 2)


skip42 <- merge_skipgram(blog42,news42)
skip42 <- merge_skipgram(skip42,twit42)

saveRDS(skip4, file = "~/R/DataScience_Capstone/ngrams_Rdata_files/skip-1gram.Rdata")
saveRDS(skip42, file = "~/R/DataScience_Capstone/ngrams_Rdata_files/skip-2gram.Rdata")


#testing purpose
blog4 <- read_rds("~/R/DataScience_Capstone/4-gram/blog_all_4-gram.Rdata")
forskip_test <- blog4[count %between% c(500,530)]
forskip_test2 <- blog4[count %between% c(500,530)]
forskip_test
forskip_test2
make_skip_gram(forskip_test, type = 1)
make_skip_gram(forskip_test2, type = 1)
forskip_test2[count==500, sentence:= "at the"]




