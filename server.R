require(devtools)
library(shiny)
#install.packages("tm")
suppressMessages(library(tm))
suppressMessages(require(stringr))
#install.packages("stringi")
suppressMessages(library(stringi))
#install.packages("wordcloud")
suppressMessages(library(wordcloud))
#install.packages("RWeka")
suppressMessages(require(RWeka))

#Read in files of ngrams
corder<-read.table("twograms.txt")
corder3<-read.table("threegrams.txt")
corder4<-read.table("fourgrams.txt")

#Below are functions that the server will use

#extract last word from phrase
extractlastword<-function(phrase)
{
  words<-str_extract_all(phrase, "\\S+")
  wordsvector<-as.vector(words[[1]])
  length<-length(wordsvector)
  wordsvector[length]
}

#extract last n words from phrase
extractlastnwords<-function(phrase, n=integer)
{
  if(n<1){n<-1}
  words<-str_extract_all(phrase, "\\S+")
  wordsvector<-as.vector(words[[1]])
  length<-length(wordsvector)
  first<-(length-n+1)
  if(first>0)
  {paste(wordsvector[first: length], collapse=" ")}
  else
  {paste(wordsvector[1:length], collapse=" ")}
}

#predict next word based on last word of phrase
predictnextword1<-function(phrase)
{
  onegram<-extractlastword(phrase)
  onegramstart<-paste("^", onegram, sep="")
  onegramstart<-paste(onegramstart, " ", sep="")
  logic<-grepl(onegramstart, corder$ctoken, ignore.case=TRUE, )
  followedbyword<-sum(logic)
  if(followedbyword==0)
  {
    if((stripWhitespace(phrase)=="") || (stripWhitespace(phrase)==" ")){print("")}
    else{print("the")}
   #print("no 2gram match")
  }
  else{
    top<-corder[logic==1, ]
    v2<-as.vector(top[1, 1])
    print(extractlastword(v2))
    #print("used one word")
  }
}

#predict next word based on last two words of phrase
predictnextword2<-function(phrase)
{
  onegram<-extractlastnwords(phrase, 2)
  onegramstart<-paste("^", onegram, sep="")
  onegramstart<-paste(onegramstart, " ", sep="")
  logic<-grepl(onegramstart, corder3$ctoken3, ignore.case=TRUE, )
  followedbyword<-sum(logic)
  if(followedbyword==0)
  {
    predictnextword1(phrase)
  }
  else{
    top<-corder3[logic==1, ]
    v2<-as.vector(top[1, 1])
    print(extractlastword(v2))
    #print("used two words")
  }
}

#predict next word based on last three words of phrase
predictnextword3<-function(phrase)
{
  onegram<-extractlastnwords(phrase, 3)
  onegramstart<-paste("^", onegram, sep="")
  onegramstart<-paste(onegramstart, " ", sep="")
  logic<-grepl(onegramstart, corder4$ctoken4, ignore.case=TRUE, )
  followedbyword<-sum(logic)
  if(followedbyword==0)
  {
    predictnextword2(phrase)
  }
  else{
    top<-corder4[logic==1, ]
    v2<-as.vector(top[1, 1])
    print(extractlastword(v2))
    #print("used three words")
  }
}


shinyServer(
  function(input, output){
    prediction<-reactive({predictnextword3(input$phrasebegin)})
    output$lastword<- renderText({paste(c("The next word is \"", prediction(), "\"."), collapse="")})
    output$phraseend <- renderText({paste(c("In other words, the completed phrase is \"", input$phrasebegin, " ", prediction(), "\"."), collapse="")})
  }  
)