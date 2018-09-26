#' @title softTFIDF method for approximate string comparision
#'
#' @description Soft-TFIDF has been implemented as softer version of TFIDF where it takes 2 strings and generates scores. This score is helpful in string comparison as it tends be accurate.
#' @param string1 character string to be matched
#' @param string2 character string to be matched against
#' @import stringdist stringr dplyr
#' @return Returns a comparision score between the two strings in the range of 0 to 1, where 0 indicates no similarity between the two strings and 1 represents a match between two strings.
#' @examples
#' #Calculating softTFIDF comparision score
#' softTFIDF("kaspersky is awesome","kasper are antivirus")
#' @export
softTFIDF<- function(string1, string2) {
  ##SoftTFIDF function to calculate score

  #removing the punctuations out and taking all to lower caps
  m<-tolower(gsub('[[:punct:] ]+',' ',string1))
  n<-tolower(gsub('[[:punct:] ]+',' ',string2))

  #breaking string into separate character for each term
  sp <- unlist(str_match_all( m, "\\S+" ) )
  sp2 <- unlist(str_match_all( n, "\\S+" ) )

  #creating an empty data frame to store count of each term and total no of terms in given strings
  str_df <- data.frame( "string"=character(),"word" = character(), "count" = integer(), "total"=integer() , stringsAsFactors=FALSE)

  #setting count flag
  flag<- 0

  #counting the no. of repitition of each term in string string1 and adding to data frame str_df
  for(j in 1:length(sp))
  {
    for(i in 1:length(sp))
    {
      if(sp[j]==sp[i])
      {
        flag<- flag+1
      }
    }
    str_df[nrow(str_df) + 1, ] <- c( "string1",sp[j], flag,length(sp) )
    ##print(paste(n[j],flag))
    flag<-0
  }


  #counting the no. of repitition of each term in string string2 and adding to data frame str_df
  for(j in 1:length(sp2))
  {
    for(i in 1:length(sp2))
    {
      if(sp2[j]==sp2[i])
      {
        flag<- flag+1
      }
    }
    str_df[nrow(str_df) + 1, ] <- c( "string2",sp2[j], flag,length(sp2) )
    ##print(paste(n[j],flag))
    flag<-0
  }

  #removing repeated rows from data frame
  FinalCount<- unique( str_df)

  #calculating inverse document frequency
  IDF  <-FinalCount %>% group_by(word) %>% count() %>% mutate( idf= log(2/n) )

  #Adding calculated IDF value to the main data frame
  TF_Idf <-left_join(FinalCount,IDF, by="word")

  #creating function to get scalar values
  scalar1 <- function(x) {x / sqrt(sum(x^2))}

  #calculating the Term Frequency and weighted TF/IDF score for each row of terms
  TF_IDF_Final <- TF_Idf %>% mutate(TF=as.numeric(count)/as.numeric(TF_Idf$total), TF_IDF= scalar1(log(1+as.numeric(count))* log(2/as.numeric(n))   ))

  #creating an empty similarity matrix with string string1 on columns and string string2 on rows
  sim<- matrix(data = 0, nrow = length( unique(sp2)), ncol =length( unique( sp)), dimnames = list( unique(sp2),unique(sp)))

  #taking unique characters in each strings
  usp<-unique(sp)
  usp2<- unique(sp2)

  #variable to add final comparison score
  value<-0

  #calculation of final comparison score
  for(i in 1:length(usp2))
    for(j in 1:length(usp))
    {
      deg<-stringdist::stringsim(usp[j],usp2[i] ,"jw") #use of jaro winkler to obtain degree
      if( deg >= 0.5){ #setting threshold for score
        sim[i,j] <-  deg * TF_IDF_Final[j,8] * TF_IDF_Final[length(usp)+i,8]
      }
      else {
        sim[i,j]<- 0
      }
      value <- value + sim[i,j]
    }

  value<-1-value
  #checking condition for perfectly matching strings
  if(is.nan(value))
  {
    value<- 1
  }
  #final comparison score
  return(value)
}
