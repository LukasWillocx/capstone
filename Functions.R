#Function 1: check the amount of words we're working with
### Takes a character input and counts the amount of words
### This is required for our prediction function to know what ngrams to look for

wordCount <- function(x){
  length(unlist((strsplit(x,' '))))
}

#Function 2: ngram frequency dataframe creator

### takes the frequency matrix, obtained from the document term matrix
### and turns it into a dataframe with three headers,
### the prov header: provided sequence to match with
### A frequency of occurence (so we take the most likely one)
### a pred header, with the last word in that respective ngram

ngram_df_maker<-function(x){
  ngram_df<-data.frame(prov=character(length(x)),pred=character(length(x)), freq=integer(length(x)))
  
  for (i in 1:length(x)) {
    #Extract full expression
    ngram <- names(x)[[i]]
    #Split ngram into words
    ngram_words<-unlist(strsplit(ngram," "))
    #Extract count
    freq=x[[i]]
    #Create a row for the dataframe
    ngram_row<-c(paste(ngram_words[1:length(ngram_words)-1],collapse=' '), #all words except the last
                  ngram_words[[length(ngram_words)]], #last word
                  freq) #frequency of occurrence
    #Add the row to the dataframe
    ngram_df[i,]<-ngram_row
  }
  ngram_df
}

#Function 3: predicts the word
predictWord <- function(x,uni,bi,tri,quad) {
  x<-(tolower(x))
  if (wordCount(x) == 0){
    return("no words provided")}
  predWord<-character(0)
  
  if (wordCount(x) >= 3) {
    phrase <- paste(unlist(str_split(x,' '))[(wordCount(x)-2):wordCount(x)],collapse=' ')
    predWord <- quad$pred[quad$prov == phrase] 
  }
    if (length(predWord)>0){return(predWord[1:3])}
  
  if (wordCount(x) >= 2) {
    phrase <- paste(unlist(str_split(x,' '))[(wordCount(x)-1):wordCount(x)],collapse=' ')
    predWord <- tri$pred[tri$prov == phrase] 
  }
   if  (length(predWord)>0){return(predWord[1:3])}
  
  if (wordCount(x) >= 1) {
    phrase <- paste(unlist(str_split(x,' '))[wordCount(x)],collapse=' ')
    predWord <- bi$pred[bi$prov == phrase] 
  }
  if  (length(predWord)>0){return(predWord[1:3])}
     else predWord <- sample(uni$pred,1)
  return(predWord[1:3])
}
