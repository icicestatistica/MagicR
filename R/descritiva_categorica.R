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
#' @import ggthemes forcats
#' @export
grafico_categorica <- function(var,nome, niveis='auto', cor='cyan4', ordenar=T,virgula=F){
  var = unlist(var)
  if (niveis[1]=='auto') niveis = names(table(var))
  var = factor(var, levels=niveis)
  niveisnovo=vetor_comsep_c(niveis,11)
  levels(var)=niveisnovo
  tab <- data.frame(table(var),perc=paste0(table(var),paste0(" (",100*round(prop.table(table(var)),3),"%)")),prop=paste0(table(var),paste0("\n  (",100*round(prop.table(table(var)),3),"%)")))
  if(ordenar==T) {
    if(length(niveis) > 2) {
      result <- na.omit(tab) %>% mutate(var=forcats::fct_reorder(var, desc(Freq))) %>%
        ggplot() + geom_bar(aes(x=var,y=Freq),fill=cor,stat="identity")  +
        ylim(0,max(table(var))*1.1)+ ggthemes::theme_clean()  + ylab("") + xlab("") + ggtitle(paste0(vetor_comsep_c(nome,50)," (n=",length(na.omit(var)),")",collapse=""))+            geom_text(aes(x=var,y=Freq),label=ponto_para_virgula(tab$perc,virgula),vjust=-0.5) +
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

#' Gera gráfico de barras horizontal para variáveis categóricas
#'
#' Esta função gera um gráfico de barras horizontal para variáveis categóricas,
#' apresentando a frequência relativa de cada nível da variável. O gráfico permite
#' personalização da cor, ordenação das categorias e definição dos níveis. Os rótulos
#' de cada barra exibem a frequência absoluta e o percentual correspondente.
#'
#' @param var vetor (obrigatório) A variável categórica que será analisada.
#' @param nome string (obrigatório) O nome da variável, que será exibido no título do gráfico.
#' @param niveis vetor de strings (opcional, padrão = "auto") Os níveis da variável categórica.
#'  Se definido como "auto", os níveis serão automaticamente extraídos da variável.
#' @param cor string (opcional, padrão = "cyan4") Cor das barras no gráfico.
#' @param ordenar lógico (opcional, padrão = TRUE) Se \code{TRUE}, as categorias serão ordenadas
#'  por frequência. Caso contrário, serão apresentadas na ordem definida pelo parâmetro \code{niveis}.
#' @param virgula lógico (opcional, padrão = FALSE) Se \code{TRUE}, utiliza vírgula como separador
#'  decimal nos rótulos do gráfico (caso contrário, usa ponto).
#'
#' @details
#' Esta função utiliza o pacote \code{ggplot2} para gerar um gráfico de barras horizontal,
#' ideal para visualizar a distribuição de categorias de forma proporcional. A frequência
#' absoluta (\code{Freq}) e relativa (\code{Freq_rel}) são calculadas internamente e usadas
#' para compor o gráfico e os rótulos.
#'
#' @return
#' Um objeto \code{ggplot} contendo o gráfico de barras horizontal.
#'
#' @examples
#' ## Variável categórica com mais de dois níveis, ordenando
#' variavel = factor(c("A", "B", "C", "A", "A", "B", "C"))
#' grafico_categorica_vert(var = variavel, nome = "Categorias", ordenar = TRUE)
#'
#' ## Variável categórica com ordem definida
#' variavel = factor(c("Idade: 0-10", "Idade: 11-20", "Idade: 11-20", "Idade: 21-30"))
#' niveis_definidos = c("Idade: 0-10", "Idade: 11-20", "Idade: 21-30")
#' grafico_categorica_vert(var = variavel, nome = "Faixa Etária", niveis = niveis_definidos, ordenar = FALSE)
#'
#' @export
grafico_categorica_vert = function (var, nome, niveis = "auto", cor = "cyan4", ordenar = T,
                                    virgula = F)
{
  require(ggplot2)
  require(dplyr)
  var = unlist(var)
  if (niveis[1] == "auto") niveis = names(table(var))
  df = table(var) %>% data.frame()
  df$Freq_rel = prop.table(table(var)) %>%
    data.frame() %>%
    select("Freq") %>%
    unlist()
  df$var = vetor_comsep_c(df$var, 25)
  if (ordenar == T) {
    df = df %>%
      arrange(desc(-Freq)) %>%
      mutate(var = factor(var, levels = unique(var)))} else {
        df = df %>% mutate(var = factor(var, levels = vetor_comsep_c(niveis,25)))}
  plot = df %>%
    ggplot(aes(y = var, x = Freq_rel)) +
    geom_bar(stat = "identity",fill = cor) +
    magicR::theme_icic("v") +
    geom_text(aes(label = ponto_para_virgula(paste0(Freq," (",100 *round(Freq_rel, 2), "%)"), virgula),x=Freq_rel+max(Freq_rel)/10)) +
    scale_x_continuous(labels = scales::percent_format(),
                       expand = expansion(mult = c(0, 0.05)), limits=c(0,max(df$Freq_rel)+0.13)) +
    labs(y = NULL,x = "Proporção",
         title = vetor_comsep_c(paste(nome," (n=", length(na.omit(var)), ")", sep = ""), 50))
  return(plot)
}

#' quiqua_aderencia
#'
#' Executa o teste qui-quadrado de aderência para verificar se as frequências observadas em uma variável categórica diferem significativamente de frequências esperadas iguais entre as categorias. Também calcula os resíduos padronizados e identifica categorias com frequências significativamente maiores ou menores.
#'
#' @param vetor Vetor ou lista contendo a variável categórica a ser analisada.
#' @param nomecat Nome da variável categórica para uso nos textos descritivos.
#' @param niveis Ordem dos níveis da variável categórica. Padrão é `"auto"`, que utiliza os níveis da variável.
#' @param ordenar Lógico. Se `TRUE`, os resultados são ordenados por resíduos padronizados. Padrão: `TRUE`.
#' @param dig Número de casas decimais para arredondamento. Padrão: `2`.
#'
#' @return Um objeto `list` com os seguintes elementos:
#' \item{texto}{Texto interpretativo do teste qui-quadrado e dos resíduos. Inclui significância, estatística do teste, V de Cramer (cramer_v pacote rstatix) e detalhes dos resíduos significativos.}
#' \item{tabela}{Data frame contendo:}
#' \tabular{ll}{
#'   \strong{Categoria} \tab \strong{Descrição} \cr
#'   Categoria \tab Os níveis da variável categórica. \cr
#'   Frequência observada \tab Proporção observada de cada categoria. \cr
#'   IC 95% \tab Intervalo de confiança da proporção. \cr
#'   Resíduos padronizados \tab Diferença entre as proporções observadas e esperadas, ajustada. \cr
#'   p-valor \tab Significância estatística dos resíduos, considerando a correção de Bonferroni. Resíduos significativos têm um `*`. \cr
#' }
#'
#' @examples
#' # Dados fictícios
#' dados <- sample(c("A", "B", "C"), size = 50, replace = TRUE, prob = c(0.1, 0.5, 0.4))
#'
#' # Rodando o teste qui-quadrado de aderência
#' resultado <- quiqua_aderencia(vetor = dados, nomecat = "Categorias")
#'
#' # Gerando o relatório
#' relatorio(resultado,pularprimeiro=F)
#'
#' @note
#' \item Quando o número de categorias é grande, o teste pode perder poder devido ao ajuste para comparações múltiplas.
#' \item As interpretações do tamanho do efeito baseiam-se no V de Cramer e variam com o número de graus de liberdade.
#' \item Caso a variável tenha apenas uma categoria, a função retorna uma mensagem informando a impossibilidade de realizar o teste.
#' \item As funções `chisq.test` e `prop.test` são usadas para o cálculo do teste qui-quadrado e dos intervalos de confiança, respectivamente.
#' \item A função `cramer_v` (do pacote `rstatix`) é utilizada para calcular o efeito do teste.
#'
#' @import stringr rstatix knitr
#' @export
quiqua_aderencia <- function(vetor,nomecat,niveis='auto',ordenar=T,dig=2){
  vetor = unlist(vetor)
  if(niveis[1]=='auto') niveis = names(table(vetor))
  vetor <- factor(vetor,levels=niveis)

  a=NULL
  texto2=c()
  tabela <- table(vetor)
  gl=length(tabela)-1
  if(gl==0) {texto=paste0(" * **",nomecat,":** Não é possível realizar testes estatísticos em variáveis com apenas uma categoria de resposta. \n",sep="")} else {

    quiqua <- chisq.test(tabela)

    ic <- c()
    for (i in 1:length(tabela)) {ic <- rbind(ic,paste("(",paste(round(100*prop.test(x=as.vector(tabela)[i],n=length(vetor),p=1/length(tabela))$conf.int[1:2],2),collapse="%, "),"%)",sep=""))}

    ef=round(rstatix::cramer_v(tabela, p = rep(1/length(tabela),length(tabela))),dig)

    if(gl==1){
      if(ef<0.1) efeito="pode ser considerado desprezível." else
        if(ef<0.3) efeito="pode ser considerado um efeito pequeno." else
          if(ef<0.5) efeito="pode ser considerado um efeito médio." else
            efeito="pode ser considerado um efeito grande."} else
            {if(gl==2){
              if(ef<0.07) efeito="pode ser considerado desprezível." else
                if(ef<0.21) efeito="pode ser considerado um efeito pequeno." else
                  if(ef<0.35) efeito="pode ser considerado um efeito médio." else
                    efeito="pode ser considerado um efeito grande."} else
                    {if(ef<0.06) efeito="pode ser considerado desprezível." else
                      if(ef<0.17) efeito="pode ser considerado um efeito pequeno." else
                        if(ef<0.29) efeito="pode ser considerado um efeito médio." else
                          efeito="pode ser considerado um efeito grande."}}

    if(quiqua$p.value>0.05) texto = paste0("* **",nomecat,":** O teste qui-quadrado de aderência apontou que não devemos rejeitar a hipótese de igualdade entre as frequências de todas as categorias (",paste("$\\chi^2$",collapse=NULL,sep=""),"(",quiqua$parameter,") = ", round(quiqua$statistic,dig),", p=", magicR::pvalor(quiqua$p.value),", V de Cramer=",ef,"). Assim, não rejeitamos que ",paste(paste("a proporção de ",names(tabela),sep=""),collapse=" é igual ")," = 1/",length(tabela)," = **",round(100*1/length(tabela),dig),"%**. O efeito foi medido pela estatística V de Cramer (",ef,"), que ",efeito," \n") else
      texto = paste0(" * **",nomecat,":** Através do teste qui-quadrado de aderência, rejeitamos a hipótese de igualdade entre todas as frequências (",paste("$\\chi^2$",collapse=NULL),"(",quiqua$parameter,") = ", round(quiqua$statistic,dig),", p=", magicR::pvalor(quiqua$p.value),", V de Cramer=",ef,"). Isso significa que pelo menos uma frequência difere de 1/",length(tabela)," = **",round(100*1/length(tabela),dig),"%**.  O efeito foi medido pela estatística V de Cramer (",ef,"), que ",efeito, " Passamos a analisar os resíduos do teste qui-quadrado para encontrar quais frequências não são compatíveis com a frequência esperada (",round(100*1/length(tabela),dig),"%). Tomando como base uma significância de 5%, como temos ",length(tabela)," categorias, o ponto de corte $\\alpha$ utilizado será 0.05/",length(tabela),"=",round(0.05/length(tabela),4),", resultando num valor crítico (bilateral) de ",abs(round(qnorm((0.05/length(tabela))/2),dig)),". Portanto, resíduos padronizados ajustados fora da região (",round(qnorm((0.05/length(tabela))/2),dig),",",-round(qnorm((0.05/length(tabela))/2),dig),") serão considerados estatisticamente significativos. Complementando a análise, calculamos o p-valor de cada resíduo. Lembrando que agora, utilizaremos o novo $\\alpha$ (",round(0.05/length(tabela),4),"), ou seja, o resíduo será considerado estatisticamente significativo se o p-valor for menor do que esse valor.")

    if (quiqua$p.value<0.05){
      a=data.frame(names(tabela),paste0(round(100*prop.table(tabela),dig),"%"),"IC 95%"=ic,"`Resíduos padronizados ajustados`"=round(quiqua$stdres,dig),
                   "p-valor"=round(2*(1-pnorm(abs(quiqua$stdres))),3))[,-c(4,6)]
      names(a)=c("Categoria","Frequência observada","IC 95%","Resíduos padronizados","p-valor")
      if(ordenar==T) a=a[order(a$`Resíduos padronizados`,decreasing=T),]

      a$`p-valor`[a$`p-valor`<0.001] <- "<0.001*"
      a$`p-valor`[a$`p-valor`<(0.05/length(tabela))] <- paste0(a$`p-valor`[a$`p-valor`<(0.05/length(tabela))],"*")


      maior=c();menor=c();nula=c()

      for (i in 1:(dim(a)[1])) {if(stringr::str_sub(a$`p-valor`[i],-1)=="*" & a$`Resíduos padronizados`[i]>0) maior <- c(maior,a$Categoria[i]) else
        if(stringr::str_sub(a$`p-valor`[i],-1)=="*" & a$`Resíduos padronizados`[i]<0) menor <- c(menor,a$Categoria[i]) else nula <- c(nula,a$Categoria[i])
      }

      texto2=c(texto2," Através da análise de resíduos, concluimos que: \n")
      if(length(maior)==1) texto2 <- c(texto2,c("  + A categoria ",magicR::printvetor(maior)," possui frequência **maior** do que era esperado sob hipótese de igualdade de proporções. \n"))
      if(length(maior)>1) texto2 <- c(texto2,c("  + As categorias ",magicR::printvetor(maior)," possuem frequência **maior** do que era esperado sob hipótese de igualdade de proporções."),"\n")
      if(length(menor)==1) texto2 <- c(texto2,c("  + A categoria ",magicR::printvetor(menor)," possui frequência **menor** do que era esperado sob hipótese de igualdade de proporções."),"\n")
      if(length(menor)>1) texto2 <- c(texto2,c("  + As categorias ",magicR::printvetor(menor)," possuem frequência **menor** do que era esperado sob hipótese de igualdade de proporções."),"\n")
      if(length(nula)==1) texto2 <- c(texto2,c("  + A categoria ",magicR::printvetor(nula)," **não difere** estatisticamente da frequência esperada sob hipótese de igualdade de proporções."),"\n") else {
        if(length(nula)==(dim(a)[1])) texto2 <- c(texto2,c("  + Apesar do teste ser significativo globalmente, nenhuma das categorias diferem estatisticamente da frequência esperada sob hipótese de igualdade de proporções."),"\n") else {
          if(length(nula)>1) texto2 <- c(texto2,c("  + As categorias ",magicR::printvetor(nula)," **não diferem** estatisticamente da frequência esperada sob hipótese de igualdade de proporções."),"\n")}}
      texto2 <- c(texto2,"\n Podemos verificar o valor dos resíduos na tabela a seguir: \n")
    }}
  resultado=list("texto"=paste(c(texto,texto2,"\n"),collapse=" "),"tabela"=a)

  return(resultado)}
