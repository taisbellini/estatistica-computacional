% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encode_subs.R
\name{encode_subs}
\alias{encode_subs}
\title{Encode Text}
\usage{
encode_subs(key, text_str, alpha = c(LETTERS, " "))
}
\arguments{
\item{key}{o nome do local e arquivo do qual os dados devem ser lidos.}

\item{text_str}{string do texto.}

\item{alpha}{vetor com o alfabeto. Default é as 26 letras do alfabeto e espaço.}
}
\value{
Retorna vetor com a string encriptada.
}
\description{
Uma função que encripta um texto.
}
\author{
Taís Bellini e Juliana Sena de Souza
}
