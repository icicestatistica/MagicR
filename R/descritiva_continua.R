#' Descrição de Variáveis Contínuas
#'
#' Esta função realiza uma descrição estatística de uma variável contínua, incluindo o cálculo de estatísticas básicas, como média, desvio padrão, coeficiente de variação, e teste de normalidade. Também gera um texto interpretativo e gráficos para visualizar a distribuição dos dados.
#'
#' @param vari Vetor numérico com os dados da variável contínua.
#' @param nome Nome da variável (como string) para ser utilizado nos gráficos e no texto gerado.
#' @param bins Número de bins para o histograma (padrão é 20).
#' @param texto Se TRUE, gera uma descrição textual dos resultados. Caso contrário, não gera texto (padrão é TRUE).
#' @param grafico Se TRUE, gera gráficos para visualizar a distribuição dos dados. Caso contrário, não gera gráficos (padrão é TRUE).
#' @param cor Cor a ser usada nos gráficos (padrão é 'cyan4').
#' @param digitos Número de dígitos para arredondamento nas estatísticas (padrão é 2).
#' @param idioma Idioma para a geração do texto ('PT' ou 'EN'; o padrão é 'PT').
#' @param virgula Se TRUE, utiliza vírgula como separador decimal no texto e gráficos (padrão é FALSE).
#'
#' @return Retorna uma lista com os seguintes elementos:
#' \item{testes}{Dataframe com informações sobre a variável, incluindo tipo e sigla do teste.}
#' \item{result}{Dataframe com as estatísticas descritivas da variável.}
#' \item{texto}{Texto interpretativo gerado, se o parâmetro texto for TRUE.}
#' \item{interp}{Resumo interpretativo em formato de texto resumido.}
#' \item{grafico}{Gráficos gerados, se o parâmetro grafico for TRUE.}
#'
#' @examples
#' # Criando um vetor de dados contínuos
#' dados <- rnorm(100)
#'
#' # Chamando a função com texto e gráfico
#' resultado <- desc_uni_continua(dados, nome = "Variável X")
#'
#' # Visualizando os resultados
#' resultado$grafico
#'
#' @import stringr ggplot2 colorspace ggthemes patchwork ggrepel
#' @export
#'
desc_uni_continua <- function(vari,nome,bins=20,texto=T, grafico=T,cor='cyan4',digitos=2, idioma="PT",virgula=F){
  require(stringr)
  nf=""
  vari=unlist(vari)
  if(length(summary(vari))==6) {N=length(vari); na=0} else {N=length(vari);na=summary(vari)[7]}
  if(length(vari)-sum(is.na(vari))<3 | length(vari)-sum(is.na(vari))>3000 | min(vari,na.rm=T)==max(vari,na.rm=T)) {p = "N/A";nf="Não foi possível realizar o teste shapiro-wilk para normalidade, uma vez que não há observações suficientes para fazê-lo.  \n"} else {shap = shapiro.test(vari) ; p=magicR::pvalor(shap$p.value); if(shap$p.value <0.05) rej <- "menor que 0.05, rejeitou" else rej="maior ou igual a 0.05, não rejeitou"}
  cv=round(sd(vari,na.rm=T)/summary(vari)[4]*100,digitos)
  parametros <- c("Total","N/A","N","Min-Máx","Q1-Q3","Mediana","Média","DP","CV", "SW")
  iqr = round(summary(vari)[5]-summary(vari)[2],digitos)
  if(sum(is.na(vari))==length(vari)) variavel=c(N,paste0(na," (100%)"),0,"-","-","-","-","-","-","-") else {
    variavel <- c(N,
                  paste0(na," (",round(100*na/N,digitos),"%)"),
                  N-na,
                  paste0(round(summary(vari)[1],digitos),"-",round(summary(vari)[6],digitos)),
                  paste0(round(summary(vari)[2],digitos),"-",round(summary(vari)[5],digitos)),
                  round(summary(vari)[3],digitos),
                  round(summary(vari)[4],digitos),
                  round(sd(vari,na.rm=T),digitos),
                  paste0(cv,"%"),
                  p)}
  d <- data.frame("Característica"=parametros,"Estatística"=unlist(variavel))
  tex=NULL


  outl = length(boxplot.stats(na.omit(vari))$out)

  missings = as.numeric(d$Estatística[1])-as.numeric(d$Estatística[3])

  texto_outliers = ifelse(outl==0,"Não há outliers.", paste("Há ",outl," outlier(s).",sep=""))
  texto_missings = ifelse(missings==0,"Não há perda de dados.", paste("Há ",missings," missing(s), ou seja, linha(s) com perda de dados.",sep=""))

  interpretacao = paste(" + A variável **'",nome,"'**, variou entre ",round(summary(vari)[1],digitos)," e ",round(summary(vari)[6],digitos),". Sua média foi ",round(summary(vari)[4],digitos),", com desvio padrão de ",round(sd(vari,na.rm=T),digitos),". A mediana é ",round(summary(vari)[3],digitos)," e o intervalo interquartil é ",iqr," (Q1=",round(summary(vari)[2],digitos),"-Q3=",round(summary(vari)[5],digitos),"). ",texto_missings," ",texto_outliers,sep="")

  inter_resumo = paste0(nome,", variou entre ",round(summary(vari)[1],digitos)," e ",round(summary(vari)[6],digitos),", com média ",round(summary(vari)[4],digitos)," e desvio padrão ",round(sd(vari,na.rm=T),digitos),".")

  if(texto==T){
    if(nf=="") nf=c("  + O teste de shapiro wilk, com p-valor ",rej," a hipótese de normalidade dos dados (W=",round(shap$statistic,digitos),", p-valor=",magicR::pvalor(shap$p.value),"). \n") else shaptexto=nf
    if(cv>50) cvtexto = " Como isso não ocorreu, valores próximos à média podem não ter sido tão frequentes nos dados. \n" else cvtexto = " Como isso ocorreu, os dados tendem a se concentrar perto da média. \n"
    dif=as.numeric(d$Estatística[7])-as.numeric(d$Estatística[6])
    simetria = 5*(dif)/as.numeric(d$Estatística[8])
    if(abs(simetria) > 1) { if(simetria >0) qt = "é significativa, indicando assimetria com concentração à esquerda e cauda à direita." else qt = "é significativa, indicando assimetria com concentração à direita e cauda à esquerda."} else qt = "não é significativa, indicando simetria."
    if(d$Estatística[1]==d$Estatística[3]) {tex <- c("* **",nome,": ** A variável '",nome,"' não teve perda de dados, também chamada de *\"missings\"*, portanto todas as ",d$Estatística[1]," linhas do banco estão preenchidas.")} else
    {nr=eval(parse(text=str_sub(str_split(d$Estatística[2]," ")[[1]][2],2,-3)))
    if(nr<=5) miss <- c("Como há menos de 5% de *missings* (",nr,"%), não há motivos para se preocupar com a ausência de dados.") else { if(nr<20) miss <- c("Como as não respostas representam ",nr,"% das linhas, cabe perguntar-se se há algum tipo de viés (algum fator que influenciou essa ausência de forma sistemática).") else miss <- c("Ressaltamos que há uma grande quantidade de não respostas para essa variável (",nr,"%), por isso recomendamos que algum tipo de explicação seja dada pela ausência desses dados.")}
    tex <- c("* **",nome,": ** Das ",d$Estatística[1]," linhas presentes no banco de dados, houve ",str_split(d$Estatística[2]," ")[[1]][1], " não respostas,  também chamada \"missings\". Assim, totalizamos ",d$Estatística[3]," observações no banco de dados. ", miss," \n")}
    tex <- c(tex, " Passamos a avaliar como os valores estão distribuídos: \n")
    tex <- c(tex, "  + Os dados variaram no intervalo (",d$Estatística[4] ,"), portanto sua amplitude (diferença entre o maior e o menor) foi ",round(-eval(parse(text=d$Estatística[4])),digitos),"; \n",
             "  + Olhando para os quartis, percebemos que 25% dos valores foram menores que ",str_split(d$Estatística[5],"-")[[1]][1]," e 25% foram maiores que ",str_split(d$Estatística[5],"-")[[1]][2],". Assim, a metade \"central\"  dos dados se distribuiu ao longo de ", -eval(parse(text=d$Estatística[5]))," unidades. Esta quantia também é chamada \"Intervalo Interquartil\"; \n",
             "  + A mediana obtida foi ",d$Estatística[6], ", que indica que 50% dos dados estão abaixo desse valor e 50% estão acima. A diferença entre a média (",d$Estatística[7],") e a mediana (",d$Estatística[6],") ",qt," \n",
             "  + A variabilidade é medida pelo desvio padrão (",d$Estatística[8],"), e indica quanto os dados variam da média obtida. \n",
             "  + O CV - Coeficiente de Variação - (",d$Estatística[9],") compara o desvio padrão com a média. O ideal é que este índice seja o mais baixo possível (idealmente menor que 50%).",cvtexto,
             nf)
    tex=paste(tex,collapse="")} else tex=NULL

  if(grafico==T) grafico=graficos_continua(vari,nome,20,cor,digitos,idioma,virgula) else grafico=NULL

  testes = data.frame(Nome1 = "", Nome2 = nome, tipo = "numeric", sig_ou_não = '-', resumo = inter_resumo, sup = NA)

  return(list("testes"=testes,"result"=d,"texto"=tex,"interp"=interpretacao,"grafico"=grafico))}



#' Função para Gerar Gráficos de Variáveis Contínuas
#'
#' Esta função gera gráficos de boxplot e histograma para visualizar a distribuição de uma variável contínua.
#'
#' @param var Vetor numérico com os dados da variável contínua.
#' @param nome Nome da variável (como string) para ser utilizado nos gráficos.
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
#' grafico <- graficos_continua(dados, nome = "Variável X")
#' print(grafico)
#'
#' @import ggplot2 colorspace ggthemes patchwork ggrepel
#' @export
#'

graficos_continua = function (var, nome, bins = 20, cor = "cyan4", digitos = 2, idioma = "PT",
                              virgula = F,xmin='auto',xmax='auto')
{
  require(ggplot2)
  require(colorspace)
  require(ggthemes)
  require(patchwork)
  require(ggrepel)
  d <- data.frame(var = as.numeric(unlist(var)))
  excess <- round((max(d$var, na.rm = T) - min(d$var, na.rm = T))/8,
                  0)
  min <- ifelse(xmin=='auto',min(d$var, na.rm = T) - excess,xmin)
  max <- ifelse(xmax=='auto',max(d$var, na.rm = T) + excess,xmax)
  dp <- sd(d$var, na.rm = T)
  media = mean(d$var, na.rm = T)
  if (idioma == "PT") medianomegraf = "Média=" else medianomegraf = "Mean="
  box <- ggplot(d) +
    geom_boxplot(aes(x = var), fill = lighten(cor,0.2)) +
    theme_clean() +
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
    xlim(min,max) +
    geom_label_repel(data = data.frame("x" = c(summary(var)[c(1:3,5,6)]),
                                       "y" = c(-0.2,0.1,-0.1,0.1,-0.2),
                                       "label" = ponto_para_virgula(paste0(c("Min=","Q1=","Med=","Q3=","Max="), round(summary(var)[c(1:3,5,6)],digitos)), virgula)),
                     aes(x = x, y = y, label = label),
                     color = "black")

  histo <- ggplot(d, aes(x = var)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins, fill = cor) +
    geom_density(colour = NA, fill = cor, alpha = 0.5) +
    geom_errorbarh(data=data.frame("mmax"=c(media+dp),"mmin"=c(media-dp),"y"=c(0),"h"=max(density(d$var,na.rm = T)$y)/5),aes(xmax = mmax, xmin = mmin, y = y, height = h),inherit.aes = FALSE) +
    theme_clean() +
    labs(x=nome,y="") +
    coord_cartesian(xlim = c(min, max)) +
    geom_vline(xintercept = media,color = "black", size = 1) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          plot.background = element_rect(colour = "white"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.major.x = element_line(colour = "gray"),
          panel.grid.minor.x = element_line(colour = "lightgray"),
          panel.grid.major.y = element_blank()) +
    geom_label(data=data.frame(x = media,y = 0, label = ponto_para_virgula(paste0(medianomegraf,round(summary(var)[4], digitos)), virgula)),aes(x=x,y=y,label=label), color = "black")
  return(box/histo)
}
