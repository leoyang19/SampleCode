#@title: Reverse Characters
#@description: This function inputs a string and outputs the same string in reverse order
#@param: String
#@return: String

reverse_chars<- function(x){
  reverse <- nchar(x):1
  split<- strsplit(x, split ='')
  word_reversed <- split[[1]][reverse]
  paste(word_reversed, collapse = '')
}

