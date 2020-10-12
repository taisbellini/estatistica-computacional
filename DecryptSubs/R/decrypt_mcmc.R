#' @title Decrypt Substitution Cipher With MCMC
#' @name decrypt_mcmc
#'
#' @description Realizada o Markov Chain Monte Carlo (MCMC) para cifras de substituição.
#'
#' @param encoded texto encriptado.
#' @param key0 chave inicial.
#' @param reference_bigrams vetor de bigramas do texto de referência.
#' @param reference_freq vetor de frequência dos bigramas do texto de referência.
#' @param pr.method = 1, nr = 10000, p = 1, alpha = c(LETTERS, ' ')
#' @param nr número inteiro que indica o número de iterações do algoritmo. Default é 10.000.
#' @param p número inteiro que indica o valor do parâmetro de escala. Default é 1.
#' @param alpha vetor com o alfabeto. Default é as 26 letras do alfabeto e espaço.
#'
#' @return Retorna uma lista com o argmax da função escore, a respectiva chave e a
#' quantidade de vezes que a proposta foi aceita.
#'
#' @author Taís Bellini e Juliana Sena de Souza
#'
#' @references CHEN,J.J. & ROSENTHAL,J.(2012). Decrypting classical cipher text using markov chain monte carlo.Statisticsand Computing22, 397–413.
#'
#' @importFrom stats runif
#'
#'
#'
#'
#' @export
decrypt_mcmc = function(encoded, key0, reference_bigrams, reference_freq, pr.method = 1, nr = 10000, p = 1, alpha = c(LETTERS, ' ')) {

  prob_accept = function(pi_old, pi_new){
    diff = exp(pi_new - pi_old)^p

    prob = min(diff, 1) #teste

    return(prob)
  }


  proposal = function(key, method = pr.method){

    it = 1

    if(method == 2){
      it = 2
    }

    if(method == 3){
      it = sample(c(1,2,3), 1)
    }

    for (i in 1:it){
      swap_index = sample(1:length(key), 2, replace = T)
      new_key = key
      new_key[swap_index[1]] = key[swap_index[2]]
      new_key[swap_index[2]] = key[swap_index[1]]
      key = new_key
    }
    return(new_key)
  }

  f_max = 0
  key_old = key_max = key0
  decoded_old = decode_subs(key_old, encoded, alpha)
  f_old = f_max = funcao_p(decoded_old, reference_bigrams, reference_freq)
  accept = 0

  for (i in 1:nr){
    key_new = proposal(key_old, method = pr.method)
    decoded_new = decode_subs(key_new, encoded, alpha)
    f_new = funcao_p(decoded_new, reference_bigrams, reference_freq)

    prob_accept = prob_accept(f_old, f_new, p=p)

    if(runif(1) < prob_accept){
      accept = accept + 1
      if(length(f_new) > 0) {
        if (f_new > f_max){
          f_max = f_new
          key_max = key_new
        }
        key_old = key_new
        decoded_old = decoded_new
        f_old = f_new
      }
    }
  }
  return (list ("f_max" = f_max,
                "key_max" = key_max,
                "accept" = accept))
}
