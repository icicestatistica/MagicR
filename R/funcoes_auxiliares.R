#' Formata números com casas decimais personalizadas
#'
#' Esta função formata números com a quantidade desejada de casas decimais,
#' permitindo configurar o separador decimal.
#'
#' @param numero Um valor numérico a ser formatado.
#' @param dig Um valor inteiro indicando o número de casas decimais. O padrão é `2`.
#' @param virgula Um valor lógico. Se `TRUE`, utiliza vírgula como separador decimal; se `FALSE`, utiliza ponto. O padrão é `FALSE`.
#' @return Uma string representando o número formatado com o separador decimal definido.
#' @examples
#' magic_format(3.14159) # "3.14"
#' magic_format(3.14159, dig = 3) # "3.142"
#' magic_format(3.14159, virgula = TRUE) # "3,14"
magic_format = function(numero,dig=2,virgula=F) {
  dm = ifelse(virgula==F,".",",")
  res=formatC(numero,digits = dig,decimal.mark = dm, format = "f")
  return(res)
}


#' Gera uma sequência de estrelas para significância estatística
#'
#' Esta função retorna uma sequência de asteriscos conforme o número especificado,
#' com opção de incluir barras invertidas.
#'
#' @param n Um valor inteiro indicando o número de asteriscos desejados.
#' @param barra Um valor lógico. Se `TRUE`, inclui barras invertidas (`\\*`) nos asteriscos; se `FALSE`, usa apenas `*`. O padrão é `TRUE`.
#' @return Uma string com a sequência de asteriscos formatada.
#' @examples
#' asteriscos(3) # "\\*\\*\\*"
#' asteriscos(2, barra = FALSE) # "**"
astericos = function(n,barra=T){
  res=ifelse(barra==T,paste0(rep("\\*",n),collapse=""),paste0(rep("*",n),collapse=""))
  return(res)
}


#' Formata p-valores com estrelas e opções adicionais
#'
#' Essa função formata p-valores com diferentes níveis de significância estatística,
#' adicionando estrelas conforme os níveis de significância. Oferece opções para configurar
#' a exibição de barras, igualdade e separador decimal.
#'
#' @param p Um valor numérico representando o p-valor.
#' @param barras Um valor lógico. Se `TRUE`, utiliza barras (`\\*`) para exibir as estrelas; se `FALSE`, não utiliza barras.
#' @param igual Um valor lógico. Se `TRUE`, adiciona um sinal de igualdade (`=`) antes do valor do p-valor formatado.
#' @param virgula Um valor lógico. Se `TRUE`, utiliza vírgula como separador decimal; se `FALSE`, utiliza ponto.
#' @return Uma string formatada do p-valor com as estrelas correspondentes aos níveis de significância.
#' @keywords internal

pvalor = function (p,barras=T,igual=F,virgula=F) {
  ig=ifelse(igual==F,"","=")
  if (is.numeric(p) == F | is.na(p) == T) {
    res = "" } else {
      pv = magic_format(p,3,virgula)
      if (p < 0.001) {res = paste0("<0",dm,"001",asteriscos(3,barras))} else
        if (p < 0.01) {res=paste0(ig,pv,asteriscos(2,barras))} else
          if(p<0.05) {res=paste0(ig,pv,asteriscos(1,barras))} else
            res=paste0(ig,pv)
    }
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
#' @import stringr
#' @keywords internal
vetor_comsep_c <- function(vec,corteinicial){
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
#' @import stringr
#' @keywords internal
ponto_para_virgula = function (vetor, virgula = F) {
  if(virgula==T) res = stringr::str_replace_all(vetor, "\\.", "\\,") else res=vetor
  return(res)
}

#' Gera um relatório formatado a partir de uma lista.
#'
#' Esta função percorre uma lista de objetos e gera um relatório de acordo com o tipo de cada item. Para data.frames, a função imprime uma tabela formatada (com kable). Para listas, chama a própria função recursivamente. Para vetores de caracteres, imprime os itens no formato de texto (com cat), e para outros objetos, apenas imprime o valor.
#'
#' @param a Um vetor ou lista contendo os objetos a serem relatados. Pode incluir data.frames, listas, vetores de caracteres ou outros tipos de objetos.
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
relatorio <- function(a, dig = 2) {
  for (i in 1:length(a)) {
    if (is.null(a[[i]]) == TRUE) tantofaz = 0 else {
      if (class(a[[i]])[1] == "data.frame") print(knitr::kable(a[[i]], row.names = FALSE, digits = dig)) else
        if (class(a[[i]])[1] == "list") relatorio(a[[i]]) else
          if (class(a[[i]])[1] == "character") cat(a[[i]], sep = "\n") else
            print(a[[i]])
    }
  }
}


#' Tema customizado para gráficos do ggplot2
#'
#' Esta função define um tema customizado baseado no \code{ggthemes::theme_clean()},
#' com ajustes específicos para facilitar a visualização de gráficos horizontais
#' ou verticais. O tema modifica o fundo, as grades e o alinhamento de textos no gráfico.
#'
#' @param sentido string (opcional, padrão = "v") Determina a orientação do gráfico:
#'  \code{"v"} para gráficos verticais e \code{"h"} para gráficos horizontais.
#'
#' @details
#' O tema \code{theme_icic} é projetado para melhorar a apresentação de gráficos
#' criados com o pacote \code{ggplot2}. Ele inclui:
#' - Fundo transparente para o painel e o gráfico.
#' - Título e subtítulo centralizados.
#' - Ajustes nas grades principais:
#'   - Para gráficos verticais (\code{sentido = "v"}): grades verticais com linhas pontilhadas.
#'   - Para gráficos horizontais (\code{sentido = "h"}): grades horizontais com linhas pontilhadas.
#'
#' @return
#' Um objeto \code{theme} do pacote \code{ggplot2} que pode ser aplicado diretamente
#' a um gráfico \code{ggplot}.
#'
#' @examples
#' library(ggplot2)
#'
#' dados = rnorm(100,50,10)
#'
#' # Aplicando o tema em gráficos verticais
#' p <- ggplot(NULL, aes(x=dados)) +
#'      geom_histogram()
#'
#' p1 = p + theme_icic(sentido = "v")
#' print(p1)
#'
#' # Aplicando o tema em gráficos horizontais
#' p <- ggplot(NULL, aes(x=dados)) +
#'      geom_histogram()
#'
#' p2 = p + theme_icic(sentido = "y")
#' print(p2)
#' @import ggthemes
#' @keywords internal
theme_icic <- function(sentido = "v") {
  if (sentido == "h") {
    add <- ggthemes::theme_clean() +
      ggplot2::theme(
        plot.background = element_rect(colour = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.background = element_rect(color = NA),
        panel.grid.major.y = element_line(linetype = 3, color = 'gray'),
        panel.grid.major.x = element_blank()
      )
  }
  if (sentido == "v") {
    add <- ggthemes::theme_clean() +
      ggplot2::theme(
        plot.background = element_rect(colour = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.background = element_rect(color = NA),
        panel.grid.major.x = element_line(linetype = 3, color = 'gray'),
        panel.grid.major.y = element_blank()
      )
  }
  return(add)
}

#' printvetor
#'
#' Esta função formata e retorna um vetor como uma string, unindo seus elementos
#' com uma conjunção apropriada, dependendo do idioma (Português ou Inglês). A função
#' também permite adicionar aspas ao redor dos elementos do vetor, caso desejado.
#'
#' @param vetor Vetor de caracteres a ser impresso. Os elementos serão unidos
#' através de uma conjunção.
#' @param idioma Idioma utilizado na conjunção entre os elementos. Pode ser
#' "PT" para português (padrão) ou "EN" para inglês.
#' @param aspas Lógico. Se `TRUE`, coloca aspas ao redor dos elementos. O valor
#' padrão é `TRUE`.
#'
#' @details
#' A função \code{printvetor} é útil para gerar listagens formatadas de itens,
#' com a possibilidade de ajustar a conjunção ("e" ou "and") e a formatação
#' (com ou sem aspas) dos elementos do vetor. Quando o vetor contém apenas um
#' elemento, ele é retornado sem a conjunção.
#'
#' @return
#' Uma string com os elementos do vetor unidos com a conjunção apropriada, e com
#' ou sem aspas, dependendo do valor do parâmetro \code{aspas}.
#'
#' @examples
#' vetor_exemplo <- c("maçã", "banana", "laranja")
#'
#' # Imprimindo os elementos com aspas e em português
#' printvetor(vetor_exemplo, idioma = "PT", aspas = TRUE)
#'
#' # Imprimindo os elementos sem aspas e em inglês
#' printvetor(vetor_exemplo, idioma = "EN", aspas = FALSE)
#'
#' @keywords internal
printvetor <- function(vetor, idioma="PT", aspas=TRUE) {
  size <- length(vetor)
  if (size == 0) {
    print <- ""
  } else {
    if (aspas == TRUE) {
      conec <- ifelse(idioma == "PT", "' e '", "' and '")
      if (size == 1) {
        print <- paste0("'", vetor[1], "'")
      } else {
        print <- paste(paste("'", vetor[1:(size - 1)], collapse="', ", sep=""), conec, vetor[size], "'", collapse=NULL, sep="")
      }
    } else {
      conec <- ifelse(idioma == "PT", " e ", " and ")
      if (size == 1) {
        print <- paste0(vetor[1])
      } else {
        print <- paste(paste(vetor[1:(size - 1)], collapse=", ", sep=""), conec, vetor[size], collapse=NULL, sep="")
      }
    }
  }
  return(print)
}

