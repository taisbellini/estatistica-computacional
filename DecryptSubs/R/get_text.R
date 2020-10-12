#' @title Get Text
#' @name get_text
#'
#' @description Uma função que lê o arquivo de texto em UTF-8.
#'
#' @param path o nome do local e arquivo do qual os dados devem ser lidos.
#'
#' @return O arquivo de texto.
#'
#' @author Taís Bellini e Juliana Sena de Souza
#'
#' @seealso \code{\link[base]{readLines}}
#'
#' @export
get_text = function(path){
  text = readLines(path, encoding = 'UTF-8')
  return(text)
}
