#' Gera gráfico de barras ou pizza para variáveis categóricas
#'
#' Esta função gera um gráfico de barras ou gráfico de pizza para variáveis categóricas,
#' com a possibilidade de personalizar a cor das barras e a ordem das categorias.
#' O gráfico exibe a frequência de cada nível da variável, bem como o percentual
#' correspondente a cada categoria. A escolha do tipo de gráfico é feita automaticamente:
#' se a variável tiver mais de 2 níveis ou o parâmetro \code{ordenar} estiver configurado
#' como \code{TRUE}, o gráfico será de barras; caso contrário (apenas 2 níveis, sem ordenamento),
#' será gerado um gráfico de pizza.
#'
#' @param var vetor (obrigatório) A variável categórica que será analisada.
#' @param nome string (obrigatório) O nome da variável, que será exibido nos títulos dos gráficos.
#' @param niveis vetor de strings (opcional, padrão = "auto") Os níveis da variável categórica.
#'  Se definido como "auto", os níveis serão automaticamente extraídos da variável.
#' @param cor string (opcional, padrão = "cyan4") Cor das barras ou das fatias do gráfico.
#' @param ordenar lógico (opcional, padrão = TRUE) Se \code{TRUE}, as categorias serão ordenadas por
#'  frequência. Caso contrário, elas são apresentadas na ordem original.
#' @param virgula lógico (opcional, padrão = FALSE) Se \code{TRUE}, utiliza vírgula como separador decimal
#'  nos rótulos dos gráficos (caso contrário, usa ponto).
#'
#' @examples
#' ## Mais de dois níveis, ordenando
#' variavel = factor(c("A", "B", "C", "A", "A", "B", "C")) # Três níveis, ordenado do mais frequente para o menos frequente
#' grafico_categorica(var = variavel, nome = "Categoria", ordenar = TRUE)
#'
#' ## Mais de dois níveis, sem ordenar
#' variavel = factor(c("10 ou menos","11 a 20", "11 a 20","21 a 30"))
#' grafico_categorica(var = variavel, nome = "Faixa etária", ordenar = FALSE)
#' # Repare que é interessante não ordenar quando a variável for categórica ordinal, com objetivo de manter a ordem que faz sentido da variável
#'
#' ## Dois níveis
#' variavel = factor(c("Masculino", "Feminino", "Masculino"))
#' grafico_categorica(var = variavel, nome = "Gênero")
#'
#' @export
grafico_categorica <- function(var,nome, niveis='auto', cor='cyan4', ordenar=T,virgula=F){
  require(dplyr)
  require(forcats)
  var = unlist(var)
  if (niveis[1]=='auto') niveis = names(table(var))
  var = factor(var, levels=niveis)
  niveisnovo=vetor_comsep_c(niveis,11)
  levels(var)=niveisnovo
  tab <- data.frame(table(var),perc=paste0(table(var),paste0(" (",100*round(prop.table(table(var)),3),"%)")),prop=paste0(table(var),paste0("\n  (",100*round(prop.table(table(var)),3),"%)")))
  if(ordenar==T) {
    if(length(niveis) > 2) {
      result <- na.omit(tab) %>% mutate(var=fct_reorder(var, desc(Freq))) %>%
        ggplot() + geom_bar(aes(x=var,y=Freq),fill=cor,stat="identity")  + 
        ylim(0,max(table(var))*1.1)+theme_clean()  + ylab("") + xlab("") + ggtitle(paste0(vetor_comsep_c(nome,50)," (n=",length(na.omit(var)),")",collapse=""))+            geom_text(aes(x=var,y=Freq),label=ponto_para_virgula(tab$perc,virgula),vjust=-0.5) +
        theme(
          plot.background = element_rect(colour="white"),
          axis.text.x=element_text(size=12),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())} else
          {result = ggplot(tab, aes(x="",y=Freq,fill=var)) +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0) + theme_void(base_size=12) +
            labs(fill="",title=paste0(vetor_comsep_c(nome,50)," (n=",length(na.omit(var)),")")) +
            theme(plot.title = element_text(hjust = 0.5, size = ceiling(12 * 1.1), face = "bold"),
                  plot.subtitle = element_text(size = ceiling(12 * 1.05)),
                  plot.background = element_rect(colour="white")) + 
            geom_text(aes(label = ponto_para_virgula(prop,virgula)), color = "white", position = position_stack(vjust = 0.5)) +
            scale_fill_manual(labels = vetor_comsep_c(niveis,11),values=lighten(cor,seq(0,0.3,(0.3/(length(tab$var)-1)))))}}
  if(ordenar==F) {
    result <- ggplot(tab) + geom_bar(aes(x=var,y=Freq),fill=cor,stat="identity")  + ylim(0,max(table(var))*1.1)+theme_clean()  + ylab("") + xlab("") +
      ggtitle(paste0(vetor_comsep_c(nome,50)," (n=",length(na.omit(var)),")",collapse=""))+ geom_text(aes(x=var,y=Freq),label=ponto_para_virgula(tab$perc,virgula),vjust=-0.5) +
      theme(
        plot.background = element_rect(colour="white"),
        axis.text.x=element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())}
  return(result)}

