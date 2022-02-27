library(data.table)
library(readr)


# MERGE NGRAM - SPLIT INTO SENTENCE/LASTWORD
HOME <- "~/R/DataScience_Capstone"
ngram_folder <- paste(HOME,"ngrams_Rdata_files", sep="/")
ngram_files <- list.files(ngram_folder)
ngram_files <- paste(ngram_folder,ngram_files,sep="/")


# 1-1 MERGING FUNCTION
merge_ngram <- function(ngram1,ngram2){
  # Merge them. ngram1/ngram2 "count" col are renamed count.x/count.y respectively 
  ngram_all <- merge.data.table(ngram1, ngram2, all = TRUE, by="ngram")
  # Replace NA by O (setnafill doesnt work yet with data.table including strings)
  ngram_all[is.na(count.x), count.x:=0]
  ngram_all[is.na(count.y), count.y:=0]
  # Sum the ngram freq\count, remove the original count columns
  ngram_all[,count:=(count.x+count.y)]
  ngram_all[,count.x:=NULL]
  ngram_all[,count.y:=NULL]
  return(ngram_all)
}


# 1-2 LOOPING THROUGH NGRAM FILES TO MERGE THEM
ngram_all <- read_rds(ngram_files[1])
for (i in c(2:length(ngram_files))){
  ngram2 <- read_rds(ngram_files[i])
  ngram_all  <- merge_ngram(ngram_all, ngram2)
}
rm(ngram2)


# 2 VARIOUS CLEANING 
# check a bit the ngram distribution (loosely zipf looking or not)
hist(log(ngram_all[]$count), breaks = 20)
# 6-gram blog
# remove blog entries for 6-gram coming from amazon disclaimer (anomaly found after plotting)
ngram_all <- ngram_all[!(count==213 | count==214),]
ngram_all <- ngram_all[!(ngram %like% "(amazon|as is|advertising|linking|fees|service|llc|content|website|eu|associate|subject|provide|site|removal|change|appears|participant)" & (count==213 | count==214)),]

# remove "vested interest" repeat from blog
ngram_all <- ngram_all[!ngram %like% "interests vested interests vested interests"]

# 6-gram news : nothing to do


# 3 SPLITTING NGRAM into sentence and last word
#a_ngram <- read_rds(ngram_files[1]) # testing purpose
ngram_all[, c("sentence","last_word") := tstrsplit(ngram, ' (?=[^ ]+$)', perl=TRUE )]
ngram_all[,ngram:=NULL]
#ngram_all <- ngram_all[, list(words=list(last_word), score= list(count)) , by = .(sentence)]



# STORE in a .Rdata file which contains all the ngrams for a n-size 
# and for a type of corpus (blog,news,twitter) together with their count
setkey(ngram_all, sentence)
ngram_file <- paste(ngram_folder, "/", corpus_type, "_all_", ngram_size,
                    "-gram", ".Rdata", sep = "")
saveRDS(ngram_all, file = ngram_file)

# GET A SCORE FOR A SENTENCE
# ngram_dat : data.table with count/sentence/last_word columns
# usr_input : sentence typed by the user to be completed
# nerf_factor :  number of time the score must be multiplied by 0.4 (see backoff)
score_ngram <- function(ngram_dat, usr_input, nerf_factor = 0){
  nerf_factor <- ifelse(nerf_factor==0, 1, nerf_factor*0.4)
  matching <- ngram_dat[sentence==usr_input]
  return( tibble(proposal=matching[,last_word], score=matching[,count*nerf_factor]) )
}


# MERGE SCORE TABLE
# Merge two tibble from score_ngram
merge_score_ngram <- function(score_ngram1,score_ngram2){
  full_join(score_ngram1, score_ngram2, by="proposal", all.xy=TRUE) %>% 
  mutate(across(where(is.numeric), coalesce, 0)) %>% 
  mutate(score=score.x+score.y) %>% 
  select(proposal, score) %>% 
  arrange(desc(score))
}


# GET BEST X PROPOSALS
propositions <- score_ngram(ngram_all, "let me know what you")
propositions
# get les 5 premieres 
propositions %>% arrange(desc(score)) %>% slice(1:5)



# PREDICT WORD
# Backoff method
# 1 - Search pattern in longest Ngram
# 2 - If found a match : compute score and store it
# 3 - Search in N-1gram.
# 4 - If found a match : compute score * 0.4 and store it
# 5 - Restart from step 3 until Ngram==2


