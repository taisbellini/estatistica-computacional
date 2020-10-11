clean_string = function(text){
  clean_text = trimws(toupper(gsub("[[:punct:]]|[0-9]*", "", text)))
  return(clean_text)
}
