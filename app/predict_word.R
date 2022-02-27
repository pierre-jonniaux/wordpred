# PREDICT WORD : 
# - Load ngram data and predict next word (with score) for a given phrase.
# - using 5/4/3/2 grams previously generated from blog/news/twitter ~600mb corpus.
# - Backoff method is used to pit the user phrase against the ngram data.
# - backoff_scoring return a 2col tibble : proposal & score.
# - merge_score_ngram fun. is used to perform slightly more complex backoff.
# - backoff including search through length 2 skip gram data is available.

library(data.table)
library(tools)
library(stringr)
library(tibble)
library(dplyr)
library(readr)
library(quanteda)

# LOADING ngram files
# loading ngrams. R.data files with 3col data.table: sentence, last_word, count.
if (!exists("ngram_list")){
  ngram_files <- paste("data/ngram",list.files("data/ngram"),sep="/")
  ngram_list   <- list()
  for (ngram_file in ngram_files){
    n <- str_extract(file_path_sans_ext(basename(ngram_file)), "\\d+")
    ngram_list[[n]] <- read_rds(ngram_file)
  }
  # load skipgram anyway
  skip_1gram <- read_rds("data/skipgram/skip-1gram.Rdata")
  skip_2gram <- read_rds("data/skipgram/skip-2gram.Rdata")
}


# CLEANING query
# clean/format user query the same way the corpus were
clean_query <- function(query){
  query <- tokens(query, 
                  remove_punct = TRUE, remove_symbols = TRUE,
                  remove_numbers = TRUE, remove_url = TRUE)
  query <- tokens_keep(query, pattern = "^[a-zA-Z]+$", valuetype = "regex")
  query <- tokens_tolower(query)
  return(query)
}


# FORMATING query 
# for skip gram search only
make_skip_query <- function(query_part, to_skip = 1){
  if (to_skip == 2){
    return (paste(strsplit(query_part, " \\w+ ")[[1]], collapse = " "))
  } else {
    return (paste(strsplit(query_part, " \\w+$")[[1]], collapse = " "))
  }
}


# SCORING query
# Get a score for a sentence
# ngram_dat : data.table with count/sentence/last_word columns
# query : sentence typed by the user to be completed
# nerf_factor :  number of time the score must be multiplied by 0.4 (see backoff)
# return a 2col tibble "proposal" & "score"
score_ngram <- function(ngram_dat, query, nerf_factor = 0){
  nerf_factor <- ifelse(nerf_factor == 0, 1, 0.4^nerf_factor)
  matching <- ngram_dat[sentence == query]
  scored_ngram <- tibble(proposal = matching[,last_word], 
                         score = matching[,count*nerf_factor])
  return(scored_ngram)
}


# MERGING score_ngram
# Merge two tibble from score_ngram
merge_score_ngram <- function(score_ngram1,score_ngram2){
  merged_score <- full_join(score_ngram1, score_ngram2, by="proposal", all.xy=TRUE) %>% 
                  mutate(across(where(is.numeric), coalesce, 0)) %>% 
                  mutate(score=score.x+score.y) %>% 
                  select(proposal, score) %>% 
                  arrange(desc(score))
  return(merged_score)
}


# BACKOFF SEARCH
# - search continues to 2gram, even if finding some match before, scores for
#   a particular proposal are summed. 
# - scores are ponderated by a 0.4 factor when going down a level of gram length
# - search trough "skip gram" of length 2 is an option. Meaning the query and
#   the ngram data are adapted in order to consider not only "n-2 n-1" pattern
#   but also "n-3 n-1" and "n-3 n-2" pattern.
backoff_scoring <- function(query, use_skipgram = FALSE){
  query <- unlist(clean_query(query),use.names = FALSE)
  # adjust query lenght (shorten if too big)
  #query <- str_split(query, " +")[[1]]
  query_lenght <- ifelse(length(query) > as.numeric(max(names(ngram_list))),
                         as.numeric(max(names(ngram_list))), length(query))
  scores = tibble(proposal = character(), score = numeric())
  nerf <- 0
  # Search from 4gram to 2grams. Keep going with 0.4 nerf per step down.
  for (i in c(query_lenght:2)){
    # Process the query, compute proposals with their score
    cat(i,query_lenght,"\n")###
    query_part <- tail(query, n = i)
    query_part <- paste(query_part, collapse = " ")
    print(query_part)###
    score <- score_ngram(ngram_list[[i]], query_part, nerf)
    print(score) ###
    # Accummulate proposals and their score
    scores <- merge_score_ngram(scores, score)
    # raise nerf factor
    nerf <- nerf+1
    # but before going to next n-1 step :
    # Add skipgram scores if asked. Only for 3gram query (so 2gram with skip).
    # ( in a "wd1 wd2 wd3" pattern "wd2 wd3" is the regular 2-gram step 
    #   done next. skip-1gram = "wd1 wd2" and skip-2gram = "wd1 wd3" )
    if(use_skipgram == TRUE & i == 3){
      skip_1_query <- make_skip_query(query_part, to_skip = 1)
      skip_2_query <- make_skip_query(query_part, to_skip = 2)
      # nerf level is already set to next (n-1) level
      scoreskip_1 <- score_ngram(skip_1gram, skip_1_query, nerf)
      scoreskip_2 <- score_ngram(skip_2gram, skip_2_query, nerf)
      print(skip_1_query)
      print(scoreskip_1)
      scores <- merge_score_ngram(scores, scoreskip_1)
      scores <- merge_score_ngram(scores, scoreskip_2)
      print(skip_2_query)
      print(scoreskip_2)
    }
  }
  # if not enough proposals then go for the unigrams...
  if (nrow(scores) < 3){
    print('unigram!')
    scores <- score_ngram(ngram_list[[1]], tail(query,n=1), nerf)
  }
  if (nrow(scores) == 0){
    return(NULL)
  }else{
    return(scores)
  }
}


# Old stuff : "stupid" backoff
# BACKOFF SEARCH
# stupid_backoff_scoring <- function(query){
#   # adjust query lenght (shorten if too big)
#   query <- unlist(clean_query(query),use.names = FALSE)
#   query_lenght <- ifelse(length(query) > as.numeric(max(names(ngram_list))), 
#                          as.numeric(max(names(ngram_list))), length(query))
#   scores <- tibble()
#   nerf <- 0
#   for (i in c(query_lenght:1)){
#     #cat(i,query_lenght,"\n")###
#     query_part <- tail(query, n = i)
#     query_part  <- paste(query_part, collapse = " ")
#     #print(query_part)###
#     score <- score_ngram(ngram_list[[i]], query_part, nerf)
#     #print(score)
#     # easy back-off : we stop as soon as we get something 
#     if (nrow(score) != 0){
#       print(query_part)
#       score <- score %>% arrange(desc(score))
#       return(score)
#     }
#     nerf <- nerf +1
#   }
#   return(NULL)
# }
