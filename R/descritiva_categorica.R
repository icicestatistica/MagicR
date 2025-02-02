#' Gera gráfico de barras ou pizza para variáveis categóricas
#'
#' Esta função gera um gráfico de barras ou gráfico de pizza para variáveis categóricas,
#' com a possibilidade de personalizar a cor das barras e a ordem das categorias.
#' O gráfico exibe a frequência de cada nível da variável, bem como o percentual
#' correspondente a cada categoria.
#'
#' @param var vetor (obrigatório) A variável categórica que será analisada.
#' @param nome string (obrigatório) O nome da variável, que será exibido nos títulos dos gráficos.
#' @param niveis vetor de strings (opcional, padrão = "auto") Os níveis da variável categórica.
#'  Se definido como "auto", os níveis serão automaticamente extraídos da variável.
#' @param cor string (opcional, padrão = "cyan4") Cor das barras ou das fatias do gráfico.
#' @param ordenar Opções: T, F e 'auto' (opcional, padrão = 'auto') Se \code{TRUE}, as categorias serão ordenadas por
#'  frequência. Caso contrário, elas são apresentadas na ordem original. Se 'auto', ordenará characters e não ordenará factors.
#' @param virgula lógico (opcional, padrão = FALSE) Se \code{TRUE}, utiliza vírgula como separador decimal nos rótulos dos gráficos (caso contrário, usa ponto).
#' @param digitos numérico (opcional, padrão = `1`) número de casas decimais no percentual nos labels dos gráficos.
#' @param forcarpizza lógico (opcional, padrão = FALSE) se devemos forçar um gráfico de pizza ou deixa ele escolher (pizza para dois níveis e barras para mais de dois)
#' @param orient opcional (padrão = "auto"). Se auto, opta por vertical se a quantidade de categorias for menor que 7 e o tamanho dos labels de todas categorias for menor que 25 e horizontal caso contrário. Outras opções são "v" (gráfico vertical) e "h" (gráfico horizontal)
#' @param nas Lógico. Se `TRUE`, inclui as frequências de valores ausentes (NA) no gráfico. Padrão: `FALSE`
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
#' @import ggthemes forcats grDevices
#' @export
grafico_categorica <- function(var,
                               nome,
                               niveis='auto',
                               cor='cyan4',
                               ordenar='auto',
                               virgula=F,
                               digitos=1,
                               forcarpizza=F,
                               orient="auto",
                               nas=F){


  get_contrast_color <- function(hex_color) {
    rgb <- col2rgb(hex_color) / 255  # Convertendo para valores entre 0 e 1
    luminance <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]  # Fórmula de luminância relativa
    ifelse(luminance > 0.5, "black", "white")  # Cor do texto: preta para cores claras, branca para escuras
  }
  if(ordenar=="auto") ordenar = ifelse(class(var)=="factor",F,T)
  var = unlist(var)
  if(is.numeric(var)==T & niveis[1]!="auto") var = rep(niveis,var)
  if (niveis[1]=='auto') niveis = names(table(var))
  if(nas) {var[is.na(var)]="N/A"; niveis = unique(c(niveis,"N/A"))}
  var = factor(var, levels=niveis)
  escolhaori = ifelse(orient=="v","v",ifelse(orient=="h","h",ifelse(orient=="auto",ifelse(length(niveis)>7 | max(sapply(niveis,nchar))>25,"h","v"))))
  niveisnovo=magicR::vetor_comsep_c(niveis,ifelse(escolhaori=="v",11,25))
  levels(var)=niveisnovo
  tab <- data.frame(table(var),perc=paste0(table(var),paste0(" (",round(100*prop.table(table(var)),digitos),"%)")),prop=paste0(table(var),paste0("\n  (",round(100*prop.table(table(var)),digitos),"%)")))
  cores = grDevices::colorRampPalette(cor)(length(niveis))
  contrast_color <- unlist(unname(sapply(cores, get_contrast_color)))

  if(ordenar==T) {
    if(escolhaori=="v") ord = -tab$Freq else ord=tab$Freq
    tab = na.omit(tab) %>%
    mutate(var=forcats::fct_reorder(var, ord))
    cores = cores[order(ord)]
    niveisnovo = niveisnovo[order(ord)]}

  ### gráficos de barras
  if(length(niveis) > 2 & forcarpizza==F) {
    if(escolhaori=="v"){

      ### vertical
      result <- ggplot(tab) +
        geom_bar(aes(x=var,y=Freq,fill=var),stat="identity")  +
        labs(x=NULL) +
        scale_y_continuous(name=NULL, limits=c(0,max(table(var))*1.1),expand=c(0.025,0),breaks = scales::pretty_breaks(n = 5)) +
        ggtitle(paste0(magicR::vetor_comsep_c(nome,50)," (n=",length(na.omit(var)),")",collapse=""))+
        geom_text(aes(x=var,y=Freq),label=magicR::ponto_para_virgula(tab$perc,virgula),vjust=-0.5) +
        scale_fill_manual(values=cores) +
        magicR::theme_icic("h") +
        theme(legend.position = "none",
              axis.text = element_text(size=12))} else

          ### horizontal
        {sobra = max(tab$Freq)*0.05
        result = ggplot(tab) +
          geom_bar(aes(y=var,x=Freq,fill=var),stat="identity")  +
          magicR::theme_icic("v")  +
          labs(y=NULL,x=NULL) +
          scale_x_continuous(expand=c(0.025,0),limits=c(0,max(table(var))*1.1),breaks = scales::pretty_breaks(n = 5)) +
          ggtitle(paste0(magicR::vetor_comsep_c(nome,50)," (n=",length(na.omit(var)),")",collapse=""))+
          geom_text(aes(y=var,x=Freq+sobra),label=magicR::ponto_para_virgula(tab$prop,virgula),lineheight = 0.9) +
          scale_fill_manual(values=cores) +
          theme(legend.position = "none",
                axis.text = element_text(size=12))}
  } else

    ### Gráfico de pizza
  {if(length(cor)==1) {cores = grDevices::colorRampPalette(c(cor,"white"))(length(niveis)+1)[-(length(niveis)+1)]
                      contrast_color <- unlist(unname(sapply(cores, get_contrast_color)))
                      if (ordenar==T) cores = cores[order(ord)]}

  result = ggplot(tab, aes(x="",y=Freq,fill=var)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    theme_void(base_size=12) +
    labs(fill="",title=paste0(magicR::vetor_comsep_c(nome,50)," (n=",length(na.omit(var)),")")) +
    theme(plot.title = element_text(hjust = 0.5, size = ceiling(12 * 1.1), face = "bold"),
          plot.subtitle = element_text(size = ceiling(12 * 1.05)),
          plot.background = element_rect(colour="white")) +
    geom_text(aes(label = ponto_para_virgula(prop,virgula)),color = contrast_color, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(labels = niveisnovo,values=cores)+
    scale_color_identity()}
  return(result)}

#' quiqua_aderencia
#'
#' Executa o teste qui-quadrado de aderência para verificar se as frequências observadas em uma variável categórica diferem significativamente de frequências esperadas iguais entre as categorias. Também calcula os resíduos padronizados e identifica categorias com frequências significativamente maiores ou menores.
#'
#' @param vetor Vetor ou lista contendo a variável categórica a ser analisada.
#' @param nomecat Nome da variável categórica para uso nos textos descritivos.
#' @param niveis Ordem dos níveis da variável categórica. Padrão é `"auto"`, que utiliza os níveis da variável.
#' @param ordenar Lógico. Se `TRUE`, os resultados são ordenados por resíduos padronizados. Padrão: `TRUE`.
#' @param dig Número de casas decimais para arredondamento. Padrão: `2`.
#' @param virgula Lógico. Se `TRUE`, utiliza vírgula como separador decimal. Padrão: `FALSE`.
#'
#' @return Um objeto `list` com os seguintes elementos:
#' \item{texto}{Texto interpretativo do teste qui-quadrado e dos resíduos.}
#' \item{tabela}{Dataframe contendo a proporção observada de cada categoria com seu intervalo de confiança, os resíduos padronizados e significância correspondente considerando a correção de bonferroni. Resíduos significativos são assinalados com `*`.}
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
#' - Quando o número de categorias é grande, o teste pode perder poder devido ao ajuste para comparações múltiplas.
#' - As interpretações do tamanho do efeito baseiam-se no V de Cramer e variam com o número de graus de liberdade.
#' - Caso a variável tenha apenas uma categoria, a função retorna uma mensagem informando a impossibilidade de realizar o teste.
#' - As funções `chisq.test` e `prop.test` são usadas para o cálculo do teste qui-quadrado e dos intervalos de confiança, respectivamente.
#'
#' @import stringr rstatix knitr
#' @export
quiqua_aderencia <- function(vetor,
                             nomecat,
                             niveis='auto',
                             ordenar=T,
                             dig=2,
                             virgula=F){
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
    for (i in 1:length(tabela)) {ic <- rbind(ic,magicR::ponto_para_virgula(paste("(",paste(round(100*prop.test(x=as.vector(tabela)[i],n=length(vetor),p=1/length(tabela))$conf.int[1:2],dig),collapse="%; "),"%)",sep=""),virgula))}

    ef=round(rstatix::cramer_v(tabela, p = rep(1/length(tabela),length(tabela))),dig)
    ef_plot = magic_format(ef,dig,virgula)

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

    if(quiqua$p.value>0.05) {

      texto = paste0("O teste qui-quadrado de aderência apontou que não devemos rejeitar a hipótese de igualdade entre as frequências de todas as categorias (",
                     paste("$\\chi^2$",collapse=NULL,sep=""),"(",quiqua$parameter,") = ", magic_format(quiqua$statistic,dig,virgula),", p", pvalor(quiqua$p.value,virgula = virgula,igual=T),", V de Cramer=",ef_plot,
                     "). Assim, não rejeitamos que ",paste(paste("a proporção de ",names(tabela),sep=""),collapse=" é igual ")," = 1/",length(tabela)," = **",magic_format(100*1/length(tabela),dig,virgula),"%**.",
                     "O efeito foi medido pela estatística V de Cramer (",ef_plot,"), que ",efeito," \n")} else

      texto = paste0("Através do teste qui-quadrado de aderência, rejeitamos a hipótese de igualdade entre todas as frequências (",
                     paste("$\\chi^2$",collapse=NULL),"(",quiqua$parameter,") = ", magic_format(quiqua$statistic,dig,virgula),", p", pvalor(quiqua$p.value,virgula=virgula,igual=T),", V de Cramer=",ef_plot,
                     "). Isso significa que pelo menos uma frequência difere de 1/",length(tabela)," = **",magic_format(100*1/length(tabela),dig,virgula),"%**.",
                     "  O efeito foi medido pela estatística V de Cramer (",ef_plot,"), que ",efeito,

                     ### Resíduos
                     " Passamos a analisar os resíduos do teste qui-quadrado para encontrar quais frequências não são compatíveis com a frequência esperada (",magic_format(100*1/length(tabela),dig,virgula),"%).",
                     "Tomando como base uma significância de 5%, como temos ",length(tabela)," categorias, o ponto de corte $\\alpha$ utilizado será ",magic_format(0.05,2,virgula),"/",length(tabela),"=",magic_format(0.05/length(tabela),max(2,dig+1),virgula),", resultando num valor crítico (bilateral) de ",magic_format(abs(qnorm((0.05/length(tabela))/2)),dig,virgula),
                     ". Portanto, resíduos padronizados ajustados fora da região (",magic_format(qnorm((0.05/length(tabela))/2),dig,virgula),";",magic_format(-qnorm((0.05/length(tabela))/2),dig,virgula),") serão considerados estatisticamente significativos.",
                     " Complementando a análise, calculamos o p-valor de cada resíduo. Lembrando que agora, utilizaremos o novo $\\alpha$ (",magic_format(0.05/length(tabela),max(2,dig+1),virgula),"), ou seja, o resíduo será considerado estatisticamente significativo se o p-valor for menor do que esse valor.")

    if (quiqua$p.value<0.05){
      pvals=2*(1-pnorm(abs(quiqua$stdres)))
      a=data.frame(names(tabela),
                   "Freq"=paste0(magic_format(100*prop.table(tabela),dig,virgula),"%"),
                   "IC 95%"=ic,
                   "Resíduos padronizados ajustados"=magic_format(quiqua$stdres,dig,virgula),
                   "p-valor"=formatC(pvals,digits=dig,format="f",decimal.mark = ifelse(virgula==T,",","."))
      )
      names(a)=c("Categoria","Frequência observada","IC 95%","Resíduos padronizados","p-valor")
      a$`p-valor`[pvals<0.001]=paste0("<",magic_format(0.001,dig=3,virgula))

      a$`p-valor`[pvals<(0.05/length(tabela))] <- paste0(a$`p-valor`[pvals<(0.05/length(tabela))],"*")

      if(ordenar==T) a=a[order(quiqua$stdres,decreasing=T),]


      maior=c();menor=c();nula=c()

      for (i in 1:(dim(a)[1])) {if(stringr::str_sub(a$`p-valor`[i],-1)=="*" & a$`Resíduos padronizados`[i]>0) maior <- c(maior,a$Categoria[i]) else
        if(stringr::str_sub(a$`p-valor`[i],-1)=="*" & a$`Resíduos padronizados`[i]<0) menor <- c(menor,a$Categoria[i]) else nula <- c(nula,a$Categoria[i])
      }

      a$`Resíduos padronizados` = magicR::ponto_para_virgula(a$`Resíduos padronizados`,virgula)
      a$`p-valor` = magicR::ponto_para_virgula(a$`p-valor`,virgula)

      comps = c()
      if(length(maior)>0) comps = c(comps,paste0(magicR::printvetor(maior)," com frequência maior"))
      if(length(menor)>0) comps = c(comps,paste0(magicR::printvetor(menor)," com frequência menor"))
      if(length(nula)>0) comps = c(comps,paste0(magicR::printvetor(nula)," sem diferença"))
      interpcomp=paste0(printvetor(comps,aspas=F)," em relação à hipótese de igualdade de proporções.")

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

  interp = paste0("O teste qui-quadrado de aderência ($\\chi^2$ (",quiqua$parameter,") = ", magic_format(quiqua$statistic,dig,virgula),", p", pvalor(quiqua$p.value,virgula = virgula,igual=T),", V de Cramer=",ef_plot,") revelou ",ifelse(quiqua$p.value<0.05,"desigualdade ","igualdade "),"nas proporções",ifelse(quiqua$p.value<0.05,paste0(", sendo ",paste0(interpcomp,collapse="")),"."))

  resultado=list("texto"=paste(c(texto,texto2,"\n"),collapse=" "),"tabela"=a,"interp"=interp)

  return(resultado)}

#' Descrição univariada de variável categórica
#'
#' Esta função realiza uma análise descritiva univariada para variáveis categóricas, incluindo tabelas de frequência,
#' interpretação textual e gráficos.
#'
#' @param variavel Vetor ou lista contendo a variável categórica a ser analisada.
#' @param nome Nome da variável categórica para uso nos textos descritivos.
#' @param niveis Ordem dos níveis da variável categórica. Padrão é `"auto"`, que utiliza os níveis detectados na variável.
#' @param nas Lógico. Se `TRUE`, inclui as frequências de valores ausentes (NA) nas informações. Padrão: `FALSE`.
#' @param label Lógico. Se `TRUE`, adiciona uma coluna com frequência absoluta e relativa concatenadas. Padrão: `FALSE`.
#' @param ordenar Lógico. Se `TRUE`, ordena os resultados em ordem decrescente de frequência. Padrão: `TRUE`.
#' @param acumula Lógico. Se `TRUE`, adiciona uma coluna com frequência acumulada. Padrão: `TRUE`.
#' @param quiqua Lógico. Se `TRUE`, executa o teste qui-quadrado de aderência na variável categórica. Padrão: `FALSE`.
#' @param digitos Número de casas decimais para arredondamento. Padrão: `2`.
#' @param virgula Lógico. Se `TRUE`, utiliza vírgula como separador decimal. Padrão: `FALSE`.
#'
#' @details
#' A função produz:
#'
#' - Uma tabela com frequência absoluta, relativa e opcionalmente acumulada.
#' - Um texto descritivo interpretativo da variável categórica.
#' - Opcionalmente, um gráfico de barras para visualização.
#'
#' Se o parâmetro `quiqua` for ativado, realiza-se o teste qui-quadrado de aderência para verificar se a distribuição da variável é uniforme.
#'
#' @return Um objeto `list` contendo os seguintes elementos:
#' \item{result}{Data frame com as frequências absolutas, relativas e acumuladas.}
#' \item{texto}{Texto interpretativo sobre os grupos da variável categórica, com resultados do teste qui-quadrado, se aplicável.}
#' \item{interp}{Interpretação textual resumida sobre a variável}
#' \item{grafico}{Objeto gráfico (ggplot2) com o gráfico de barras ou pizza}
#'
#' @examples
#' # Criando um vetor categórico de exemplo
#' variavel <- sample(c("A", "B", "C"), size = 100, replace = TRUE, prob = c(0.2, 0.5, 0.3))
#'
#' # Analisando a variável categórica
#' resultado <- desc_uni_categorica(
#'   variavel = variavel,
#'   nome = "Exemplo",
#'   ordenar = TRUE,
#'   quiqua = TRUE,
#'   grafico = TRUE
#' )
#'
#' relatorio(resultado)
#'
#' @export
desc_uni_categorica <- function(variavel,
                                nome,
                                niveis = 'auto',
                                nas = FALSE,
                                label = FALSE,
                                ordenar = TRUE,
                                acumula = TRUE,
                                quiqua = FALSE,
                                grafico = TRUE,
                                digitos = 2,
                                virgula = FALSE,
                                ...) {
  variavel <- unlist(variavel)
  if(is.numeric(variavel)==T & niveis[1]!="auto") variavel = rep(niveis,variavel)
  if (niveis[1] == 'auto') niveis <- names(table(variavel))
  var_ini = factor(variavel,niveis); niv_ini = niveis
  if(nas) {variavel[is.na(variavel)]="N/A"; niveis = c(niveis,"N/A")}
  variavel <- factor(variavel, levels = niveis)

  tablevar <- table(variavel)
  if (ordenar) tablevar <- table(factor(variavel, levels = names(tablevar)[order(tablevar, decreasing = TRUE)]))
  prop <- paste0(magicR::ponto_para_virgula(round(100*prop.table(tablevar), digitos),virgula),"%")

  descri = magicR::printvetor(paste0(names(tablevar), " (", prop, ")"),aspas = F)

  if (length(tablevar)==1) {
    interpretacao = paste0("Todas as observações pertencem à categoria ",descri,".")
    interp_resumo = paste0(nome, " foi exclusivamente ", descri,".")} else {
      interpretacao <- paste0(" + Em relação à **'", nome, "'**, tivemos os grupos ", descri,".")
      interp_resumo <- paste0(nome, " se dividiu nos grupos ", descri,".")
    }

  d <- data.frame(tablevar, prop)

  if (label) d <- data.frame(d, "Freq." = paste0(d[,2], " (", d[, 3], ")"))
  if (acumula)
    d <- data.frame(d, "Freq. Relativa Acumulada" = paste0(cumsum(d[, 2]), " (",
                                                          magicR::ponto_para_virgula(round(100 * cumsum(d[, 2]) / (cumsum(d[, 2])[nrow(d)]), digitos),virgula), "%)"))

  colnames(d) <- c("Característica", "Frequência", "Freq. Relativa", "Freq.", "Freq. Acumulada")[c(TRUE, TRUE, TRUE, label, acumula)]
  row.names(d) <- NULL

  if (!quiqua) {
    testectexto <- NULL
    testectabela <- NULL
  } else {
    testec <- magicR::quiqua_aderencia(var_ini, nome, niv_ini, ordenar, digitos,virgula)
    interpretacaoc = paste0(interpretacao," ",testec$interp)
    interp_resumo = paste0(interp_resumo," ",testec$interp)
    if (length(testec) == 1) {
      testectexto <- testec$texto
      testectabela <- NULL
    } else {
      testectexto <- testec$texto
      testectabela <- testec$tabela
    }
  }

  if (grafico) {
      graficoc <- magicR::grafico_categorica(variavel, nome, niveis, ordenar=ordenar, virgula=virgula,digitos=digitos,nas=nas,...)
  } else {
    graficoc <- NULL
  }

  testes <- data.frame(Nome1 = "", Nome2 = nome, tipo = ifelse(ordenar, "factor", "ordinal"),
                       sig_ou_não = '-', resumo = interp_resumo, sup = NA)

  resultados <- list("result" = d, "texto" = list(paste0(interpretacao," ",testectexto),testectabela),
                     "interp" = interpretacaoc, "grafico" = graficoc)

  attr(resultados,"testes")=testes

  return(resultados)
}

