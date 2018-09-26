#' @title Needleman Wunsch Method for Approximate Text String Comparison
#'
#' @description Needleman Wunsch algorithm has been implemented to generate score by finding the best alignment for every pair of token A and B taken from strings String 1 and String 2 respectively.
#' @param string1 character string to be matched
#' @param string2 character string to be matched against
#' @import stringr
#' @return Returns a comparison score in form of negative or positive number, more negative the number there is high chance of mismatch and more positive the number there is a high chance of match.
#' @examples
#' #Calculating needlemanWunsch comparision score
#' needlemanWunsch("GCATGCU","GACTTBD")
#' @export
needlemanWunsch<- function(string1, string2) {
## function to compare two strings string1 and string2 using needleman wunsch and generating the final score.

  #removing punctuation marks
m<-tolower(gsub('[[:punct:] ]+',' ',string1))
n<-tolower(gsub('[[:punct:] ]+',' ',string2))

#sepearting each string into individual terms
sp <- unlist(str_match_all( m, "\\S+" ) )
sp2 <- unlist(str_match_all( n, "\\S+" ) )


#generating and empty similarity matrix
sim<- matrix(data = 0, nrow = length( unique(sp2)), ncol =length( unique( sp)), dimnames = list( unique(sp2),unique(sp)))

#taking unque terms
usp<-unique(sp)

usp2<- unique(sp2)

#initiating variable to save score
value<-0

NMWScore <- function(A,B) {
  ##Function to generate score for two individual tokens being compared using Needleman Wunsch when called from NWComparator function

  #spliting into single characters for each term A and B
  strA<- strsplit(A,"")[[1]]
  strB<-strsplit(B,"")[[1]]

  #generating an empty data frame
  sim<- matrix(data = 0, nrow = length(strB), ncol =length(strA), dimnames = list(strB,strA))

  #creating a similarity matrix
  for(i in 1:length(strB))
    for(j in 1:length(strA))
    {
      if(strB[i]==strA[j]) #checking if characters match
        sim[i,j]<-1 #putting 1 for each matching character
      else
        sim[i,j]<--1 #putting -1 for each mismatch
    }

  #creating an empty score matrix
  F <- matrix(0, nrow =length(strB)+1  , ncol =length(strA)+1)

  #adding calue for gap
  d<- -1

  #generating initial value for each row
  for ( i in 0:length(strB))
    F[i+1,1]<- d*i

  #generating initial value for each column
  for ( j in 0:length(strA))
    F[1,j+1]<- d*j


  #generating score for each position by comparing with similarity matrix
  for(i in 1:length(strB))
    for(j in 1:length(strA))
    {
      match  = F[i,j] + sim[i,j]
      delete = F[i,j+1] + d
      insert = F[i+1,j] + d
      F[i+1,j+1] = max(match, delete, insert)

    }

  #captturing the final score
  score<- F[i+1,j+1]

  #returning the score
  return(score)

}


#loop to perform algorithm on each individual term from each string
for(i in 1:length(usp2))
  for(j in 1:length(usp))
  {
    #calling NMWScore function with score for term comparison
    sim[i,j] <-  NMWScore(usp[j],usp2[i])

    #storing values generated
    value <- value + sim[i,j]
  }
#returning the values
return(value)
}
