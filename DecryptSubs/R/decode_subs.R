#' @title Decode Text
#' @name decode_subs
#'
#' @description Uma função que decripta um texto.
#'
#' @param key vetor com a chave.
#' @param text_str string de texto.
#' @param alpha vetor com o alfabeto. Default é as 26 letras do alfabeto e espaço.
#'
#' @return Retorna vetor com a string decriptada.
#'
#' @author Taís Bellini e Juliana Sena de Souza
#'
#' @export
decode_subs = function(key, text_str, alpha=c(LETTERS, " ")) {
  text = unlist(strsplit(text_str, split = "", fixed = TRUE))
  decoded = sapply(text, function(x) alpha[match(x, key)])
  return(decoded)
}
