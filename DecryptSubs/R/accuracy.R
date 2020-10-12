#' @title Accuracy
#' @name accuracy
#'
#' @description Função que calcula a acurácia entre duas chaves.
#'
#' @param key1 Chave real de encriptação do texto.
#' @param key2 Chave a ser comparada.
#'
#' @return A acurácia entre as duas chaves.
#'
#' @details A acurácia entre duas chaves é definida como a proporção de vezes que os carracteres
#' na mesma posição do texto são iguais nas duas chaves.
#'
#' @author Taís Bellini e Juliana Sena de Souza
#'
#'
#' @export
accuracy = function(key1, key2){
  count = 0
  for (i in 1:length(key2)){
    if(key1[i] == key2[i])
      count = count + 1
  }
  return(count/length(key2))
}
