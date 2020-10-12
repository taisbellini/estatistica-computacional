#' @title Get Bigrams From Text
#' @name get_bigrams_from_text
#'
#' @description Cria bigramas de caracteres a partir do texto.
#'
#' @param text_vector Vetor de strings ou string.
#'
#' @return Vetor de bigramas.
#'
#' @author Taís Bellini e Juliana Sena de Souza
#'
#' @export
get_bigrams_from_text = function(text_vector){
  bigrams = bigramr::ConvertStrings2Bigrams(text_vector)
  bigrams = unlist(bigrams)
  return(unname(bigrams))
}
