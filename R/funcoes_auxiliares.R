#' Formata p-valores com estrelas
#'
#' Essa função é usada internamente para formatar p-valores de forma consistente,
#' adicionando estrelas conforme os níveis de significância estatística.

#'
#' @param p Um valor numérico representando o p-valor.
#' @return Uma string formatada do p-valor com as estrelas correspondentes aos níveis de significância.
#' @keywords internal
pvalor = function (p) {
  if (is.numeric(p) == F) res = "" else
    if (is.na(p) == T) res = "" else
      if (p > 0.05) res = formatC(round(p, 3), format = "f", digits = 3) else
        if (p > 0.01) res = paste0(formatC(round(p, 3), format = "f", digits = 3), "\\*") else
          if (p >= 0.001) res = paste0(formatC(round(p, 3), format = "f", digits = 3), "\\*\\*") else res = "<0.001\\*\\*\\*"
  return(res)
}



#' Função para separar um vetor em partes com base em um corte inicial
#'
#' Essa função divide cada elemento de um vetor de texto em substrings, utilizando um corte inicial e espaços como delimitadores. É útil para formatar ou dividir texto de maneira estruturada.
#'
#' @param vec Vetor de strings a ser dividido. Cada elemento do vetor será tratado separadamente.
#' @param corteinicial Valor numérico indicando a posição inicial para o corte do texto.
#'
#' @return Retorna um vetor de strings com as partes separadas por cortes. Caso o texto não possa ser dividido, retorna `NA`.
#' @keywords internal
vetor_comsep_c <- function(vec,corteinicial){
    require(stringr)
    res = c()
    for (vetor in vec) {
      a = T
      espacos = c(stringr::str_locate_all(vetor, " ")[[1]][, 1], nchar(vetor) + 1)
      inicio = 1
      corte = corteinicial
      fim = ifelse(espacos[1] > corte, espacos[1] - 1, espacos[max(which(espacos - 1 <= corte))] - 1)
      newvec <- NULL
      ena=FALSE
      while (a) {
        if(is.na(vetor)==T) {ena = T; a=F} else {
          newvec = c(newvec, stringr::str_sub(vetor, inicio, fim))
          inicio = fim + 2
          if (inicio > nchar(vetor))
            a = F
          corte = inicio + corteinicial
          fim = espacos[max(which(espacos - 1 <= corte))] - 1
          if (fim < inicio) {
            fim = espacos[max(which(espacos - 1 <= corte)) + 1] - 1
          }
        }}
      if(ena) newvec=NA else newvec = paste0(newvec, collapse = "\n")
      res = c(res, newvec)
    }
return(res)}


#' Substitui ponto por vírgula em um vetor
#'
#' Esta função substitui todos os pontos (".") por vírgulas (",") em um vetor de strings, com base no parâmetro `virgula`.
#'
#' @param vetor Um vetor de strings onde os pontos serão substituídos por vírgulas, caso o parâmetro `virgula` seja `TRUE`.
#' @param virgula Um valor lógico que determina se a substituição de ponto por vírgula deve ocorrer. O valor padrão é `FALSE` (não substitui).
#' @return O vetor original com os pontos substituídos por vírgulas (se `virgula` for `TRUE`).
#' @keywords internal
ponto_para_virgula = function (vetor, virgula = F) {
  if(virgula==T) res = str_replace_all(vetor, "\\.", "\\,") else res=vetor
  return(res)
}
