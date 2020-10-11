clean_text = function(text, accent = FALSE){
  clean_text = trimws(toupper(gsub("[[:punct:]]|[0-9]*", "", text)))
  clean_text = clean_text[clean_text != ""]
  clean_text = clean_text[length(clean_text) >1]
  clean_text = gsub("\"", "", clean_text)
  clean_text = gsub("'", "", clean_text)
  clean_text = gsub("-", " ", clean_text)
  clean_text = clean_text[length(clean_text) >1]

  if(accent == FALSE){
    clean_text = iconv(clean_text, from = "UTF-8", to = "ASCII//TRANSLIT")
  }

  return(clean_text)
}
