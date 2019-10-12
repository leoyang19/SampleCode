#@title: Counting Vowels
#@description: counts how many vowels there are in a given string 
#@param: string
#@return: numeric value (count)


vowels <- c('a','e','i','o','u')
count_vowels <- function(x){
  if(is.character(x)){
    vowels = c('a','e','i','o','u')
    counts <- rep(0,5)
    x <- tolower(unlist(strsplit(x, split = '')))
    print(x)
    for (i in seq_along(vowels)){
      print(i)
      counts[i] <- sum(x == vowels[i])
    }
    names(counts) <- vowels
    return(counts)
  }
  else{
    stop("invalid input; a string was expected")
  }
}




