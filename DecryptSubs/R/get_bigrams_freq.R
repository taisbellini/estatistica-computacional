#' @title Get Bigrams Frequency
#' @name get_bigrams_freq
#'
#' @description Funcao para gerar a tabela de frequência dos bigramas.
#'
#' @param bigrams vetor de bigramas.
#'
#' @return data frame de frequências.
#'
#' @author Taís Bellini e Juliana Sena de Souza
#'
#' @export
get_bigrams_freq = function(bigrams){
  return(as.data.frame(table(bigrams)))
}
