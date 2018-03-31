library (Rfacebook)
library (data.table)
library (dplyr)
library(tidyverse)
library(tidytext)
library (stopwords)

Sys.setlocale (category = "LC_ALL", locale = "en_US.UTF-8")
DIR <- paste0("/home/teddy", "/fbtext")   #had to be changed from paste0(Sys.getenv("HOME"), "/fbtext") due to jenkins
FBOAUTH <-    #specify file for saved Facebook token

load (file = FBOAUTH)

######################################################## START ###########################################

stp_en <- get_stopwords ("en")

Vendors <- c('ericsson', 'huawei', 'nokia', 'ZTECorp', 'google', 'IBM')
StartDate <- as.Date("2017-01-01")
Today   <- Sys.Date()

AllPostsComments <- NULL
for (Vendor in Vendors) {
  
  ### Read posts from StartDate
  theDate <- StartDate
  VendorPosts <- NULL
  while (theDate < Today){
    SinceDate <- theDate
    UntilDate <- min (theDate + 5, Today -1)
    #read fb
    if (SinceDate < UntilDate) {dt <- data.table(
      getPage (page=Vendor, token = fb_oauth, 
               since = as.character(SinceDate), 
               until = as.character(UntilDate), 
               feed=TRUE, reactions = TRUE))
    }
    if (length(dt) > 0) {   #add Year/Quarter and Year_month to posts
      dt [, created_Q := paste(format(as.Date(created_time), "%y"), 
                               sprintf("Q%02i", (as.POSIXlt(created_time)$mon) %/% 3L + 1L), sep="/")]
      dt [, created_month := paste(format(as.Date(created_time), "%y"),
                                   strftime(as.character(created_time), "%m"), sep = "_")] 
      
    }
    
    theDate <- theDate + 6
    VendorPosts <- rbind (VendorPosts, dt)
  }   #end of while (read posts)
  VendorPosts [,"Vendor"] <- Vendor
  
  ### Collecting comments
  ### getting comments for posts
  VendorCommentsTable <- NULL
  for (i in VendorPosts$id) { 
    dt <- getPost(post=i, n=2000, token=fb_oauth, likes=FALSE)   #likes=FALSE: otherwise run Error, prob. bug 
    if (length(dt$comments$id) > 0) {
      dt <- data.table ( dt$comments) 
      dt [, created_wk := paste(format(as.Date(created_time), "%y"),    #1st jan is always wk1, not ISO8601
                                sprintf("w%02i", as.numeric(strftime(created_time, "%j")) %/% 7L + 1L), sep="::")]
      dt [,"PostID"] <- i
      VendorCommentsTable <- rbind (VendorCommentsTable, dt)
    }
  }
  ### Renaming columns in comment table: adding "Comment_" to the beginning
  tempnames <- names(VendorCommentsTable)[seq(1,length(VendorCommentsTable)-1)]   #all but last column, which is PostID (added last)
  VendorCommentsTable [, (paste0("Comment_",tempnames )) := .SD , .SDcols=tempnames]
  VendorCommentsTable <- VendorCommentsTable[,mget(c(names(VendorCommentsTable)[grep("^Comment_.*",names(VendorCommentsTable))], "PostID"))]
  
  ### extracting headline from news link, if message is empty
  VendorPosts_womessage <- VendorPosts%>%
    filter(is.na(message)) %>%
    #filter (!str_detect (link, "www\\.facebook")) %>%
    mutate (Headline = str_extract (link, "/(\\w+-){3,}[\\w]+")) %>%    #search for patterns like "/5g-connected-trials-c.."
    mutate (Headline = str_replace (Headline, "^/","")) %>%   #strip leading / 
    mutate (Headline = str_replace_all (Headline, "-", " ")) 
  ### copy message to headline for the rest
  VendorPosts_wmessage <- VendorPosts %>% 
    filter(!is.na(message)) %>%
    mutate (Headline = message) %>%
    mutate (Headline = str_replace (Headline, "htt.*$", "")) %>%    #remove http.....
    mutate (Headline = str_replace (Headline, ":\\s*$", ""))# %>%     #remove :   , especially inherited from : htt...
  ### now we have all posts with healine, wherever available, assembling them sorting by time
  VendorPosts <- rbind (VendorPosts_wmessage, VendorPosts_womessage) %>% arrange (created_time)
  
  ### remove "learn more" and "read more", as typical in post endings
  VendorPosts <- VendorPosts %>%
    mutate (Headline = str_replace_all( Headline, "read more", "")) %>%
    mutate (Headline = str_replace_all (Headline, "learn more", "")) %>%
    mutate (Headline = str_replace_all (Headline, "can find", ""))
  
  ### merge Posts and Comments
  VendorPosts <- VendorPosts %>% left_join(VendorCommentsTable, by = c("id"="PostID"))
  
  ### Putting to one list  
  AllPostsComments <- rbind (AllPostsComments, VendorPosts)
  
}

fwrite (AllPostsComments, paste0(DIR, "/", "AllPostsComments.csv"))
VendorPosts <- NULL     #even on small EC2 instance script run from jenkins faced memory problem


######################## PREPARING FOR PRESENTING STUFF 
library ("rlist")
stp_spc <- data.table(word = list.append(Vendors,c("goo.gl", "zte", "ibm", "g.co", 
                                                   "zte’s", "heygoogle", "ibm's",
                                                   "learn", "read", "know", "can")))



######################## PRESENTING STUFF ---------------   TRACK G: COMBINING UNI AND BIGRAMS


tidyPosts <- AllPostsComments %>%
  group_by(id) %>%                #from here until ungroup: get rid of duplicate posts
  filter(row_number() == 1) %>%
  ungroup %>%
  unnest_tokens (word, Headline) %>%
  filter(!is.na(word)) %>%
  anti_join (stp_en) %>%
  anti_join (stp_spc) %>%
  mutate(word = str_replace(word, "\\w+[^\\w]\\w*", "")) %>%   #"zte's" was not propoerly filtered by stp_spc
  filter(word != "") 

proportion <- tidyPosts %>%
  count (Vendor, created_Q, word, sort = TRUE) %>%   #count words per vendor per Q
  group_by (Vendor, created_Q) %>%
  mutate (prop =  n/sum(n) * 100) %>%
  arrange (desc(prop)) %>%
  arrange (Vendor, created_Q)

bigramPosts <- AllPostsComments %>%
  group_by(id) %>%                #from here until ungroup: get rid of duplicate posts
  filter(row_number() == 1) %>%
  ungroup %>%
  unnest_tokens (bigram, Headline, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%   #storing bigram words separately
  filter(!word1 %in% stp_en$word) %>%
  filter(!word2 %in% stp_en$word) %>%
  filter(!word1 %in% stp_spc$word) %>%
  filter(!word2 %in% stp_spc$word) %>%
  mutate(word1 = str_replace(word1, "\\w+[^\\w]\\w*", "")) %>%   #"zte's" was not propoerly filtered by stp_spc
  filter(word1 != "")  %>%
  mutate(word2 = str_replace(word2, "\\w+[^\\w]\\w*", "")) %>%   #"zte's" was not propoerly filtered by stp_spc
  filter(word2 != "")  %>%
  unite(word, word1, word2, sep = " ") 

proportion_bi <- bigramPosts %>%   #same as for unigrams, not collapsed as these have different weights
  count (Vendor, created_Q, word, sort = TRUE) %>%   #count words per vendor per month
  group_by (Vendor, created_Q) %>%
  mutate (prop =  n/sum(n) * 100) %>%
  arrange (desc(prop)) %>%
  arrange (Vendor, created_Q) 


prop_top5 <- rbind (
  proportion  %>%
    filter (row_number () <6L),
  proportion_bi  %>%
    filter (row_number () <6L)
)
fwrite (prop_top5, paste0(DIR, "/", "prop_top5.csv"))


proportion <- rbind (proportion, proportion_bi)
fwrite (proportion, paste0(DIR, "/", "proportion.csv"))


################################### COMMENTS SENTIMENTS

bing <- (get_sentiments( "bing"))


tidy_word_Comments <- AllPostsComments %>%
  unnest_tokens (word, Comment_message) %>%
  filter(!is.na(word)) %>%
  #  anti_join (stp_en) %>%
  #  anti_join (stp_spc) %>%
  filter(word != "") 

uni_res <- tidy_word_Comments %>%
  inner_join (bing) %>%
  count (Vendor, as.Date(Comment_created_time), sentiment) %>%     #Comment_created_wk for weekly display version[]
  spread (sentiment, n, fill = 0) %>%
  mutate (sentiment = positive - negative) 


## bi-grams to take care of negations
negation_words <- c("not", "no", "never", "without", "isn't", "aren't")


tidy_bigram_Comments <- AllPostsComments %>%
  unnest_tokens (bigram, Comment_message, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%   #storing bigram words separately
  #  filter(!word1 %in% stp_en$word) %>%
  #  filter(!word2 %in% stp_en$word) %>%
  #  filter(!word1 %in% stp_spc$word) %>%
  #  filter(!word2 %in% stp_spc$word) #%>%
  
  
  negated_words_res <- tidy_bigram_Comments %>%
  filter(word1 %in% negation_words) %>%
  inner_join(bing, by = c("word2" = "word")) %>%
  count (Vendor, as.Date(Comment_created_time), sentiment) %>%   #Comment_created_wk for weekly display version
  mutate(sentiment = recode (sentiment, "positive" = "negative", "negative" = "positive")) %>%    #doing the inversion
  spread (sentiment, n, fill = 0) %>%
  mutate (delta_sentiment = positive - negative) %>%
  select (-c(negative, positive))

#joining the result of bigram and calculate sentiment
comment_sent <- left_join(uni_res, negated_words_res) %>%
  mutate (delta_sentiment = ifelse(is.na(delta_sentiment),0,delta_sentiment)) %>%
  mutate (sentiment = sentiment + 2 * delta_sentiment)

fwrite (comment_sent, paste0(DIR, "/", "comment_sent.csv"))
write.csv ((unique(proportion$Vendor)), paste0(DIR, "/", "vendors.csv"))
write.csv ((unique(proportion$created_Q)), paste0(DIR, "/", "quarters.csv"))
######################################################### END ############################################
