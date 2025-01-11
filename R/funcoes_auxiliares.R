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

#' Gera um relatório formatado a partir de uma lista.
#'
#' Esta função percorre uma lista de objetos e gera um relatório de acordo com o tipo de cada item. Para data.frames, a função imprime uma tabela formatada (com kable). Para listas, chama a própria função recursivamente. Para vetores de caracteres, imprime os itens no formato de texto (com cat), e para outros objetos, apenas imprime o valor.
#'
#' @param a Um vetor ou lista contendo os objetos a serem relatados. Pode incluir data.frames, listas, vetores de caracteres ou outros tipos de objetos.
#' @param pularprimeiro Um valor lógico (default = TRUE). Se TRUE, o primeiro elemento da lista/vetor é ignorado.
#' @param dig Um número inteiro (default = 2). Define o número de casas decimais a serem exibidas nas tabelas de data.frames.
#'
#' @details A função utiliza o pacote `knitr` para formatar as tabelas quando encontra objetos do tipo `data.frame`. Quando encontra listas, a função é chamada recursivamente para gerar um relatório para cada item. Para objetos do tipo `character`, os valores são impressos em formato de texto.
#'
#' @examples
#' # Exemplo de uso:
#' lista_exemplo <- list("",
#'   data.frame(A = 1:3, B = c(4.567, 5.678, 6.789)),
#'   "Texto de exemplo",
#'   list(data.frame(C = 1:2, D = c(7.890, 8.901)))
#' )
#' relatorio(lista_exemplo)
#'
#' @import knitr
relatorio <- function(a, pularprimeiro = TRUE, dig = 2) {
  require(knitr)
  if (pularprimeiro == TRUE) comeco = 2 else comeco = 1
  for (i in comeco:length(a)) {
    if (is.null(a[[i]]) == TRUE) tantofaz = 0 else {
      if (class(a[[i]])[1] == "data.frame") print(kable(a[[i]], row.names = FALSE, digits = dig)) else
        if (class(a[[i]])[1] == "list") relatorio(a[[i]], FALSE) else
          if (class(a[[i]])[1] == "character") cat(a[[i]], sep = "\n") else
            print(a[[i]])
    }
  }
}



