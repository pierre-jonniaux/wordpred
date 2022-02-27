######  THIS GENERATE N-GRAM OBJECTS NEEDED FOR WORD-PRED APP  #######

# 1 - Working on small files due to hardware limitation (4gb RAM)
# 2 - File transformation into corpus
# 3 - Cleaning corpus (from punctuation, profanity?, upper case, numbers)
# 4 - Extract N-grams as data.table with 2 col (ngram, count>cutoff).
# 5 - Store N-grams object as .Rdata
# 6 - Merge all N-grams object (intersect merge so again data reduction)
# 7 - Format N-grams data.table in 3 col (ngram[0:-2],ngram[-1],count)

library(quanteda)
library(readr)
library(tibble)
library(data.table)
library(tools)

###   PARAMETERS - GLOBAL   ###
# HOME dir
HOME <- "~/R/DataScience_Capstone"
# CORPUS dir :
corpus_files_folder <- paste(HOME, "separated_files/en_US.twitter", sep="/")
# SIZE of n-gram :
ngram_size <- 2
# TYPE of corpus : (for naming purpose)
corpus_type <- "twit" #c("blog","news","twit")
# PATH to output folder :
ngram_folder <- paste(HOME,"ngrams_Rdata_files", sep="/")
# CUTOFF : count/frequency of ngram to retain (set to 0 if RAM isn't an issue)
ngram_cutoff <- 2
# NGWORDS : Text to be removed (blog disclaimer, profanity...)
strip_out <- paste(HOME, "to_remove_from_corpus.txt", sep="/")


### READ FILE   ###
# Chunk by chunk (not useful since working on small files finally). 
# Saved in a tibble.
make_tbl <- function(file_to_use){
  corp_tbl <- read_lines_chunked(file = file_to_use,
                                 callback = DataFrameCallback$new(function(text, pos) {
                                   tibble(text = text)
                                 }))
  return(corp_tbl)
}
#corpus_files <- list.files(corpus_files_folder)
#corpus_files <- paste(corpus_files_folder,corpus_files,sep="/")
#corp_tbl <- make_tbl(corpus_files[1])


### CORPUS CREATION - TOKENISATION   ###
# Takes a tibble with corpus text return a quanteda token objet
make_tokens <- function(corp_tbl){
  corp_corp <- corpus(corp_tbl)
  rm(corp_tbl, envir=.GlobalEnv)
  corp_tokens <- tokens(corp_corp, 
                        remove_punct   = TRUE, 
                        remove_symbols = TRUE,
                        remove_numbers = TRUE, 
                        remove_url     = TRUE)
  rm(corp_corp)
  corp_tokens <- tokens_keep(corp_tokens, pattern = "^[a-zA-Z]+$", valuetype = "regex")
  corp_tokens <- tokens_tolower(corp_tokens)
  print("Made clean token object")
  gc()
  return(corp_tokens)
}
#corp_tokens <- make_tokens(corp_tbl)


### MAKE NGRAM   ###
# We store the ngram as data.table object for speed and memory purpose.
make_ngram <- function(corp_tokens, ngram_size = 2, ngram_cutoff = 2){
  # Ngrams cannot be transformed from token into a data.table.
  # Ngram are created from token as a named&nested list.
  toks_ngram <- tokens_ngrams(corp_tokens, ngram_size, concatenator = " ")
  rm(corp_tokens, envir=.GlobalEnv)
  # Unesting. Discarding names (text1...). Ngram are now "character" class.
  ngram_datt <- unlist(toks_ngram, use.names = FALSE)
  rm(toks_ngram)
  # transform into list again so it can be set to a data.table
  ngram_datt <- list(ngram_datt)
  setDT(ngram_datt)
  # change col name
  setnames(ngram_datt, "V1", "ngram")
  # add count column
  ngram_datt <- ngram_datt[, .(count = .N), keyby = .(ngram)]
  ngram_datt <- ngram_datt[count>=ngram_cutoff]
  # we can order (desc) based on the freq/count
  #setorder(ngram_datt,-count)
  gc()
  return(ngram_datt)
}
#ngram_datt <- make_ngram(corp_tokens, ngram_size)


### SAVE NGRAM INTO .RDATA FILE   ###
save_ngram <- function(ngram_datt, ngram_file){
  saveRDS(ngram_datt, file = ngram_file)
  cat("ngram saved in:", ngram_file)
  rm(ngram_datt, envir=.GlobalEnv)
  gc()
  return(TRUE)
}
#ngram_file <- paste(ngram_folder,"/",corpus_type,"_",ngram_size,"-gram",".Rdata", sep="")
#save_ngram(ngram_datt, ngram_file)


###   MERGING 2 NGRAM   ###
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


# PART 1 - Create all the ngram .Rdata files 
# Input files (stores corpus .text files)
corpus_files <- list.files(corpus_files_folder)
corpus_files <- paste(corpus_files_folder,corpus_files,sep="/")
# Output directory (will store ngram .Rdata files)
if (!dir.exists(ngram_folder)){
  dir.create(ngram_folder)
}
# looping through corpus files to generate all ngram files 
for (corpus_file in corpus_files){
  # Name output file
  ngram_file <- paste(ngram_folder,"/",
                      file_path_sans_ext(basename(corpus_file)),"_",ngram_size,
                      "-gram",".Rdata", sep="")
  cat("Processing : ",corpus_file,"\n")
  corp_tbl    <- make_tbl(corpus_file)
  corp_tokens <- make_tokens(corp_tbl)
  ngram_datt  <- make_ngram(corp_tokens, ngram_size)
  save_ngram(ngram_datt, ngram_file)
  gc()
}

# -----------------

# PART 2 - Creating the .Rdata file storing all ngram merged together
ngram_files <- list.files(ngram_folder)
ngram_files <- paste(ngram_folder,ngram_files,sep="/")

# looping through the ngram to merge them
ngram_all <- read_rds(ngram_files[1])
for (i in c(2:length(ngram_files))){
  ngram2 <- read_rds(ngram_files[i])
  ngram_all  <- merge_ngram(ngram_all, ngram2)
}
rm(ngram2)


# cleaning ngrams
# check a bit the ngram distribution (loosely zipf looking or not)
hist(log(ngram_all[]$count), breaks = 20)
# blog
# remove blog entries for 6-gram coming from amazon disclaimer (anomaly found after plotting)
ngram_all <- ngram_all[!(ngram %like% "(amazon|as is|advertising|linking|fees|service|llc|content|website|eu|associate|subject|provide|site|removal|change|appears|participant)" & (count==213 | count==214)),]
# remove "vested interest" repeat from blog
ngram_all <- ngram_all[!(ngram %like% "vested interests" & (count==251 | count==250))]
# news : nothing instantly remarkable
# twitter profanity - remove badwords
ngram_all <- ngram_all[!ngram %like% "(nigga|niggas|bitch|bitches)"]

# splitting ngram into sentence and last word
#a_ngram <- read_rds(ngram_files[1]) # testing purpose
ngram_all[, c("sentence","last_word") := tstrsplit(ngram, ' (?=[^ ]+$)', perl=TRUE )]
ngram_all[,ngram:=NULL]
#ngram_all <- ngram_all[, list(words=list(last_word), score= list(count)) , by = .(sentence)]

# storing in a .Rdata file which contains all the ngrams for a n-size 
# and for a type of corpus (blog,news,twitter) together with their count
setkey(ngram_all, sentence)
ngram_file <- paste(ngram_folder, "/", corpus_type, "_all_", ngram_size,
                    "-gram", ".Rdata", sep = "")
saveRDS(ngram_all, file = ngram_file)

# clean ram
rm(list=ls())
gc()
cat("\nAll done !\n")
.rs.restartR()



###   Small utilities   ###
## Clearing env. and calling gc() does not clear much ram (!?)
## but restarting rstudio session on top of it does...
# rm(my_item)
# gc()
# .rs.restartR()

## Print size of a file, of an object
# utils:::format.object_size(file.size(test_file), "auto")
# print(object.size(corp_tbl),units="auto")

## Split big file into small ones in terminal
# split -d en_US.blogs.txt blog_ -n l/20 --additional-suffix=.txt
############################








