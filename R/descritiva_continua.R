#' Descrição de Variáveis Contínuas
#'
#'
#' Esta função realiza uma análise descritiva de uma variável contínua, incluindo o cálculo de estatísticas básicas, teste de normalidade, e geração de textos interpretativos e gráficos. Além disso, permite personalizar a tabela de estatísticas e gerar gráficos QQ para avaliar a normalidade.
#'
#' @param vari Vetor numérico com os dados da variável contínua.
#' @param nome Nome da variável (como string) para ser utilizado nos gráficos e no texto gerado (obrigatório).
#' @param texto Se TRUE, gera uma descrição textual detalhada dos resultados. Caso contrário, não gera texto (padrão é TRUE).
#' @param grafico Se TRUE, gera gráficos para visualizar a distribuição dos dados. Caso contrário, não gera gráficos (padrão é TRUE).
#' @param digitos Número de dígitos para arredondamento nas estatísticas (padrão é 2).
#' @param idioma Idioma para a geração do texto ('PT' ou 'EN'; o padrão é 'PT').
#' @param virgula Se TRUE, utiliza vírgula como separador decimal no texto e gráficos (padrão é FALSE).
#' @param estat_tab Formato personalizado para as estatísticas apresentadas na tabela (padrão é 'auto').
#' @param nomes_tab Nomes personalizados para as estatísticas apresentadas na tabela (padrão é 'auto').
#' @param qqgraf Se TRUE, gera um gráfico QQ para avaliação da normalidade (padrão é FALSE).
#' @param shaptest Se TRUE, realiza o teste de Shapiro-Wilk para avaliação de normalidade, se as condições forem atendidas (padrão é TRUE).
#' @param ... Argumentos adicionais para personalização dos gráficos gerados.
#'
#' @return Retorna uma lista com os seguintes elementos:
#' \item{result}{Dataframe com as estatísticas descritivas da variável.}
#' \item{texto}{Texto interpretativo detalhado, se o parâmetro texto for TRUE.}
#' \item{interp}{Resumo interpretativo em formato de texto resumido.}
#' \item{grafico}{Gráficos gerados para visualizar a distribuição dos dados, se grafico for TRUE.}
#' \item{qqplot}{Gráfico QQ gerado, se qqgraf for TRUE.}
#'
#' @examples
#' # Criando um vetor de dados contínuos
#' dados <- rnorm(100)
#'
#' # Chamando a função com texto, gráfico e QQ plot
#' resultado <- desc_uni_continua(dados, nome = "Variável X", qqgraf = TRUE)
#'
#' # Visualizando os gráficos
#' resultado$grafico
#' resultado$qqplot
#'
#' @import stringr colorspace ggthemes patchwork ggrepel stats
#' @export
#'
desc_uni_continua <- function(vari,nome,texto=T,grafico=T,digitos=2, idioma="PT",virgula=F,estat_tab='auto',nomes_tab='auto',qqgraf=F,shaptest=T, ...){
  nf=""
  vari=unlist(vari)
  if(is.numeric(vari)==F & is.integer(vari)==F) stop("Erro: A entrada deve ser numérica!")
  if (missing(nome)) stop("Erro: O argumento 'nome' é obrigatório!")
  suma = summary(vari)
  if(length(suma)==6) {N=length(vari); na=0} else {N=length(vari);na=suma[7]}
  if(shaptest==T){
  if(N-na<3 |
     N-na>3000 |
     suma[1]==suma[6]) {
    p = "N/A";nf="Não foi possível realizar o teste shapiro-wilk para normalidade, uma vez que não há observações com n entre 3 e 3000 para fazê-lo.\n"} else {
      shap = stats::shapiro.test(vari)
      p=pvalor(shap$p.value,virgula=virgula)
      rej = ifelse(shap$p.value <0.05,"menor que 0.05, rejeitou","maior ou igual a 0.05, não rejeitou")
      rej2=ifelse(shap$p.value <0.05,"rejeitamos","não rejeitamos")
      shap_interp = paste0(" Por Shapiro-Wilk, ",rej2," a normalidade dos dados (p-valor",pvalor(shap$p.value,virgula=virgula,igual=T),")")}
  }

  iqr = magic_format(suma[5]-suma[2],digitos,virgula)
  ampl = magic_format(suma[6]-suma[1],digitos,virgula)

  if(sum(is.na(vari))==length(vari)) stop("Pelo menos um valor precisa ser não vazio.")

    sumaformat = magic_format(suma,digitos,virgula)
    dp=sd(vari,na.rm=T)
    cv=dp/suma[4]*100
    percentmissings = 100*na/N
    estatisticas = rlist::list.append(sumaformat[1:6],
                                      "dp"=magic_format(dp,digitos,virgula),
                                      "cv"=magic_format(unname(cv),digitos,virgula),
                                      "iqr"=unname(iqr),
                                      "ampl"=unname(ampl),
                                      "percentmissings"=magic_format(percentmissings,digitos,virgula),
                                      "N"=N,
                                      "na"=na,
                                      "n_real"=N-na,
                                      "p"=p)
    names(estatisticas)[1:6]=c("mini","Q1","mediana","média","Q3","maxi")

    format_tab=c("{N}",
             "{na} ({percentmissings}%)",
             "{n_real}",
             "{mini}-{maxi}",
             "{Q1}-{Q3}",
             "{mediana}",
             "{média}",
             "{dp}",
             "{cv}")
    if(shaptest==T) format_tab=c(format_tab,"{p}")

    if(estat_tab[1]!="auto") {format_tab=estat_tab;
                              if(nomes_tab[1]=="auto") {warning("Defina o nome das estatísticas personalizadas informadas em estat_tab através do argumento `nomes_tab`!")
                                nomes_tab=estat_tab}
                              ;
                              if(length(nomes_tab)!=length(estat_tab)) stop("Cada estatística definida em estat_tab precisa ter um nome correspondente em nomes_tab")}

    if(nomes_tab[1]=="auto") {nomes_tab = c("Total","N/A","N","Min-Máx","Q1-Q3","Mediana","Média","DP","CV")
                              if(shaptest==T) nomes_tab = c(nomes_tab,"SW")}

    chaves <- regmatches(format_tab, gregexpr("\\{(.*?)\\}", format_tab)) %>%
      unlist() %>%
      stringr::str_sub(start=2,end=-2)

      chaves_validas <- all(chaves %in% names(estatisticas))

      if (!chaves_validas) {
        stop("Erro: Algumas variáveis no formato não são válidas. As variáveis válidas são: ",
             paste(names(estatisticas), collapse = ", "))}


    variavel=sapply(format_tab,function(x) glue_data(estatisticas,x)) %>% unname()
    d <- data.frame("Característica"=nomes_tab,"Estatística"=variavel)

    tex=NULL

  outl = length(boxplot.stats(na.omit(vari))$out)

  texto_outliers = ifelse(outl==0,"Não há outliers.", paste("Há ",outl," outlier(s).",sep=""))
  texto_missings = ifelse(na==0,"Não há perda de dados.", paste("Há ",na," missing(s), ou seja, linha(s) com perda de dados.",sep=""))

  interpretacao = paste(" + A variável **'",nome,"'**, variou entre ",estatisticas['mini']," e ",estatisticas['maxi'],".",
  " Sua média foi ",estatisticas['média'],", com desvio padrão de ",estatisticas['dp'],".",
  " A mediana é ",estatisticas['mediana']," e o intervalo interquartil é ",estatisticas['iqr']," (Q1=",estatisticas['Q1'],"-Q3=",estatisticas['Q3'],"). ",texto_missings," ",texto_outliers, sep="")
  if(shaptest==T) interpretacao = paste0(interpretacao,shap_interp,".",collapse="")

  inter_resumo = paste0(nome," variou entre ",estatisticas['mini']," e ",estatisticas['maxi'],", com média ",estatisticas['média']," e desvio padrão ",estatisticas['dp'],".")

  if(texto==T){
    if(nf=="") nf=paste0("  + O teste de Shapiro-Wilk, com p-valor ",rej," a hipótese de normalidade dos dados (W=",magic_format(shap$statistic,digitos,virgula),", p-valor=",pvalor(shap$p.value,virgula=virgula),"). \n")
    if(cv>50) cvtexto = " Como isso não ocorreu, valores próximos à média podem não ter sido tão frequentes nos dados. \n" else cvtexto = " Como isso ocorreu, os dados tendem a se concentrar perto da média. \n"
    dife=as.numeric(suma[4]-suma[3])
    simetria = 5*(dife)/dp
    if(abs(simetria) > 1) { if(simetria >0) qt = "é significativa, indicando assimetria com concentração à esquerda e cauda à direita." else qt = "é significativa, indicando assimetria com concentração à direita e cauda à esquerda."} else qt = "não é significativa, indicando simetria."
    if(na==0) {tex <- c("* **",nome,": ** A variável '",nome,"' não teve perda de dados, também chamada de *\"missings\"*, portanto todas as ",N," linhas do banco estão preenchidas.")} else
    {if(percentmissings<=5) miss <- c("Como há menos de 5% de *missings* (",percentmissings,"%), não há motivos para se preocupar com a ausência de dados.") else { if(percentmissings<20) miss <- c("Como as não respostas representam ",percentmissings,"% das linhas, cabe perguntar-se se há algum tipo de viés (algum fator que influenciou essa ausência de forma sistemática).") else miss <- c("Ressaltamos que há uma grande quantidade de não respostas para essa variável (",percentmissings,"%), por isso recomendamos que algum tipo de explicação seja dada pela ausência desses dados.")}
    tex <- c("* **",nome,": ** Das ",N," linhas presentes no banco de dados, houve ",na, " não respostas,  também chamada \"missings\". Assim, totalizamos ",estatisticas['n_real']," observações no banco de dados. ", miss," \n")}
    tex <- c(tex, " Passamos a avaliar como os valores estão distribuídos: \n")
    tex <- c(tex, "  + Os dados variaram no intervalo (",glue_data(estatisticas,"{mini}-{maxi}") ,"), portanto sua amplitude (diferença entre o maior e o menor) foi ",ampl,"; \n",
             "  + Olhando para os quartis, percebemos que 25% dos valores foram menores que ",estatisticas["Q1"]," e 25% foram maiores que ",estatisticas["Q3"],". Assim, a metade \"central\"  dos dados se distribuiu ao longo de ",iqr," unidades. Esta quantia também é chamada \"Intervalo Interquartil\"; \n",
             "  + A mediana obtida foi ",estatisticas["mediana"], ", que indica que 50% dos dados estão abaixo desse valor e 50% estão acima. A diferença entre a média (",estatisticas["média"],") e a mediana (",estatisticas["mediana"],") ",qt," \n",
             "  + A variabilidade é medida pelo desvio padrão (",estatisticas["dp"],"), e indica quanto os dados variam da média obtida. \n",
             "  + O CV - Coeficiente de Variação (",estatisticas["cv"],"%) compara o desvio padrão com a média. O ideal é que este índice seja o mais baixo possível (idealmente menor que 50%).",cvtexto,
             ifelse(shaptest==T,nf,""))
    tex=paste(tex,collapse="")} else tex=NULL

  if(grafico==T) grafico=graficos_continua(var=vari,nome=nome,digitos=digitos,idioma=idioma,virgula=virgula,...) else grafico=NULL

  if(qqgraf==T) qqplot = magic_qqplot(vari, nome, virgula=virgula, digitos=digitos) else qqplot=NULL

  testes = data.frame(Nome1 = "", Nome2 = nome, tipo = "numeric", sig_ou_não = '-', resumo = inter_resumo, sup = NA)

  resultados = list("result"=d,"texto"=tex,"interp"=interpretacao,"grafico"=grafico,"qqplot"=qqplot)

  attr(resultados,"testes")=testes

  return(resultados)}

#' Função para Gerar Gráficos de Variáveis Contínuas
#'
#' Esta função gera gráficos de boxplot e histograma para visualizar a distribuição de uma variável contínua.
#'
#' @param var Vetor numérico com os dados da variável contínua.
#' @param nome Nome da variável (como string) para ser utilizado nos gráficos.
#' @param tipo Quais gráficos serão plotados. Sendo o padrão "ambos" (ambos: Histograma e boxplot alinhados); "hist" histograma e "box" boxplot
#' @param bins Número de bins para o histograma (padrão é 20).
#' @param cor Cor a ser usada nos gráficos (padrão é 'cyan4').
#' @param digitos Número de dígitos para arredondamento nas estatísticas (padrão é 2).
#' @param idioma Idioma para a geração do texto ('PT' ou 'EN'; o padrão é 'PT').
#' @param virgula Se TRUE, utiliza vírgula como separador decimal no texto e gráficos (padrão é FALSE).
#' @param xmin Valor mínimo do eixo x do gráfico (padrão é 'auto').
#' @param xmax Valor máximo do eixo x do gráfico (padrão é 'auto').
#'
#' @return Retorna um gráfico combinando um boxplot e um histograma com a distribuição dos dados.
#'
#' @examples
#' dados <- rnorm(100)
#' graficos_continua(dados, nome = "Variável X")
#'
#' @import ggplot2 ggthemes patchwork ggrepel stringr
#' @export
#'

graficos_continua = function (var, nome, tipo="ambos",bins = 20, cor = "cyan4", digitos = 2, idioma = "PT",
                              virgula = F,xmin='auto',xmax='auto',labels=T)
{
  d <- data.frame(var = as.numeric(unlist(var)))
  excess <- round((max(d$var, na.rm = T) - min(d$var, na.rm = T))/8,
                  0)
  min <- ifelse(xmin=='auto',min(d$var, na.rm = T) - excess,xmin)
  max <- ifelse(xmax=='auto',max(d$var, na.rm = T) + excess,xmax)
  dp <- sd(d$var, na.rm = T)
  media = mean(d$var, na.rm = T)
  if (idioma == "PT") medianomegraf = "Média=" else medianomegraf = "Mean="
  box <- ggplot(d) +
    geom_boxplot(aes(x = var), fill = cor) +
    ggthemes::theme_clean() +
    ggtitle(vetor_comsep_c(paste0(nome," (n=", length(d$var[!is.na(d$var)]), ")"), 40)) +
    labs(x="",y="") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          panel.grid.major.x = element_line(colour = "gray"),
          panel.grid.minor.x = element_line(colour = "lightgray"),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(colour = "white")) +
    xlim(min,max)
  if(labels==T) box = box +
    geom_label(data = data.frame("x" = c(summary(var)[c(1:3,5,6)]),
                                 "y" = c(-0.2,0.1,-0.1,0.2,-0.2),
                                 "label" = ponto_para_virgula(paste0(c("Min=","Q1=","Med=","Q3=","Max="), round(summary(var)[c(1:3,5,6)],digitos)), virgula)),
                                      aes(x = x, y = y, label = label),
                                      color = "black")


  histo <- ggplot(d, aes(x = var)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins, fill = cor) +
    geom_density(colour = NA, fill = cor, alpha = 0.5) +
    geom_errorbarh(data=data.frame("mmax"=c(media+dp),"mmin"=c(media-dp),"y"=c(0),"h"=max(density(d$var,na.rm = T)$y)/5),aes(xmax = mmax, xmin = mmin, y = y, height = h),inherit.aes = FALSE) +
    ggthemes::theme_clean() +
    labs(x=nome,y="",title=NULL) +
    coord_cartesian(xlim = c(min, max)) +
    geom_vline(xintercept = media,color = "black", linewidth = 1) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          plot.background = element_rect(colour = "white"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.major.x = element_line(colour = "gray"),
          panel.grid.minor.x = element_line(colour = "lightgray"),
          panel.grid.major.y = element_blank(),
          plot.title=element_text(hjust=0.5))

  if(labels==T) histo = histo +
    geom_label(data=data.frame(x = media,y = 0, label = ponto_para_virgula(paste0(medianomegraf,round(summary(var)[4], digitos)), virgula)),aes(x=x,y=y,label=label), color = "black")

  if(tipo=="ambos") res=box/histo else
    if(tipo=="box") res=box + theme_icic() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank())
     else
      if(tipo=="hist") res = histo +
    ggtitle(vetor_comsep_c(paste0(nome," (n=", length(d$var[!is.na(d$var)]), ")"), 40))

  return(res)
}




#' Gera um gráfico Q-Q plot com teste de normalidade
#'
#' Esta função cria um gráfico de probabilidade Q-Q (Q-Q plot) para uma amostra numérica,
#' adicionando uma anotação com os resultados do teste de Shapiro-Wilk de normalidade.
#'
#' @importFrom ggpubr ggqqplot
#' @importFrom stats shapiro.test
#'
#' @param x Vetor numérico com os dados da amostra.
#' @param nome Uma string que representa o nome da variável a ser exibida no título do gráfico.
#' @param virgula Um valor lógico. Se `TRUE`, utiliza vírgula como separador decimal nas anotações; se `FALSE`, utiliza ponto. O padrão é `FALSE`.
#' @param digitos Um valor inteiro indicando o número de casas decimais a ser utilizado na formatação do teste de normalidade. O padrão é `2`.
#' @return Um objeto gráfico gerado pelo pacote `ggpubr`, contendo o Q-Q plot com anotações do teste de normalidade.
#' @examples
#' set.seed(123)
#' dados <- rnorm(100)
#' magic_qqplot(dados, nome = "Amostra de teste")
#'
#' magic_qqplot(dados, nome = "Exemplo", virgula = TRUE, digitos = 3)
magic_qqplot = function(x, nome, virgula=F, digitos=2) {

  shap = shapiro.test(x)
  rnr=ifelse(shap$p.value<0.05,""," não")

  graf=ggpubr::ggqqplot(x) +
    labs(x="Quantis teóricos", y="Amostra", title=paste0(nome," (n=",length(na.omit(x)),")")) +
    annotate(geom="text", label=paste0("W=",magic_format(shap$statistic, digitos, virgula),
                                       "; p=", pvalor(shap$p.value, barras=F, virgula=virgula),
                                       "\n normalidade", rnr," rejeitada"), x=1, y=min(x)) +
    theme(plot.title = element_text(hjust=0.5))

  return(graf)
}

