#' @title Score Function
#' @name funcao_p
#'
#' @description Função que calcula o valor da função escore baseado no texto encriptado.
#'
#' @param encoded_text texto encriptado.
#' @param reference_bigrams Vetor de bigramas do texto de referência.
#' @param reference_freq vetor de frequência dos bigramas do texto de referência.
#' @param pipower multiplicador para suavizar a função escore e evitar erros numéricos.
#' Default é 0.25, o mesmo de Chen & Rosenthal (2012).
#'
#' @return Retorna o valor da função escore dado os argumentos fornecidos.
#'
#' @author Taís Bellini e Juliana Sena de Souza
#'
#' @references CHEN,J.J. & ROSENTHAL,J.(2012). Decrypting classical cipher text using markov chain monte carlo.Statisticsand Computing22, 397–413.
#'
#' @export
funcao_p = function(encoded_text, reference_bigrams, reference_freq, pipower = 0.25){

  decoded_str = paste(unlist(encoded_text), collapse='') #texto
  bigrams_decoded = get_bigrams_from_text(decoded_str) #pega os bigramas
  decoded_bigrams_freq = get_bigrams_freq(bigrams_decoded) #conta a frequencia

  p_result = 0

  #for each bigram, compute its frequency, lookup in reference frequency, add to product
  for (i in 1:(length(decoded_bigrams_freq$bigrams))){ #olhar cada bigrama

    bg = as.character(decoded_bigrams_freq[i,1]) #bigrama i
    f = decoded_bigrams_freq[i,2] #frequencia do bigrama i

    r = bigramr::Bigram2Frequency(bg, reference_bigrams, reference_freq) + 1

    if (length(r) == 0) r = 1
    p_result = p_result + f*log(r)
  }

  return (p_result*pipower) # multiplicador da implementacao dos autores do artigo
}
