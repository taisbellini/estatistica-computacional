#' @title Clean Text
#' @name clean_text
#'
#' @description Limpa um texto.
#'
#' @param text Texto a ser limpado
#' @param accent logical. Se FALSE a função tira os acentos do texto. Se TRUE, ela deixa. Default é
#' tirar os acentos.
#'
#' @return O texto limpo.
#'
#' @author Taís Bellini e Juliana Sena de Souza
#'
#' @export
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
