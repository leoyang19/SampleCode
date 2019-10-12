valid_hex <- c(0,1,2,3,4,5,6,7,8,9,'a','b','c','d','e','f')

#@title: Colors in Hexadecimal Notation
#@description: This checks whether an input string is a valid color in hexadecimal notation
#@param: string
#@return: True, False Boolean logical



is_hex <- function(x){
  if(is.character(x) != TRUE){
    stop("invalid")
  }
  if(nchar(x) !=7){
    return(FALSE)
  }
  str_characters = tolower(strsplit(x,'')[[1]])
  valid_characters = c(0:9, 'a','b','c','d','e','f')
  if(str_characters[1] =='#'){
    for (i in 2:nchar(x)){
      if (!str_characters[i] %in% valid_characters){
        return(FALSE)}
    }
  }
  return(TRUE)
}



#@title: Colors in Alpha Hexadecimal Notation
#@description: to check whether an input string is a valid hex color with an alpha trasparency value
#@param: string
#@return: True, False boolean Logical


is_hex_alpha <- function(x){
  if(is.character(x) != TRUE){
    stop("invalid")
  }
  if(nchar(x) !=9){
    return(FALSE)
  }
  str_characters = tolower(strsplit(x,'')[[1]])
  valid_characters = c(0:9, 'a','b','c','d','e','f')
  if(str_characters[1] =='#'){
    for (i in 2:nchar(x)){
      if (!str_characters[i] %in% valid_characters){
        return(FALSE)}
    }
  }
  return(TRUE)
}
