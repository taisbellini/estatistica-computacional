#' @title Encode Text
#' @name encode_subs
#'
#' @description Uma função que encripta um texto.
#'
#' @param key o nome do local e arquivo do qual os dados devem ser lidos.
#' @param text_str string do texto.
#' @param alpha vetor com o alfabeto. Default é as 26 letras do alfabeto e espaço.
#'
#' @return Retorna vetor com a string encriptada.
#'
#' @author Taís Bellini e Juliana Sena de Souza
#'
#' @export
encode_subs = function(key, text_str, alpha = c(LETTERS, " ")){
  text = unlist(strsplit(text_str, split = "", fixed = TRUE))
  encoded = sapply(text, function(x) key[match(x, alpha)])
  return(encoded)
}
