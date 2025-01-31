% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descritiva_categorica.R
\name{desc_uni_categorica}
\alias{desc_uni_categorica}
\title{Descrição univariada de variável categórica}
\usage{
desc_uni_categorica(
  variavel,
  nome,
  niveis = "auto",
  nas = FALSE,
  label = FALSE,
  ordenar = TRUE,
  acumula = TRUE,
  teste = FALSE,
  grafico = TRUE,
  cor = "cyan4",
  digitos = 2,
  virgula = FALSE,
  forcarpizza = F,
  orient = "auto"
)
}
\arguments{
\item{variavel}{Vetor ou lista contendo a variável categórica a ser analisada.}

\item{nome}{Nome da variável categórica para uso nos textos descritivos.}

\item{niveis}{Ordem dos níveis da variável categórica. Padrão é `"auto"`, que utiliza os níveis detectados na variável.}

\item{nas}{Lógico. Se `TRUE`, inclui as frequências de valores ausentes (NA) nas informações. Padrão: `FALSE`.}

\item{label}{Lógico. Se `TRUE`, adiciona uma coluna com frequência absoluta e relativa concatenadas. Padrão: `FALSE`.}

\item{ordenar}{Lógico. Se `TRUE`, ordena os resultados em ordem decrescente de frequência. Padrão: `TRUE`.}

\item{acumula}{Lógico. Se `TRUE`, adiciona uma coluna com frequência acumulada. Padrão: `TRUE`.}

\item{teste}{Lógico. Se `TRUE`, executa o teste qui-quadrado de aderência na variável categórica. Padrão: `FALSE`.}

\item{grafico}{Lógico. Se `TRUE`, gera um gráfico de barras para a variável categórica. Padrão: `TRUE`.}

\item{cor}{Cor a ser utilizada no gráfico de barras. Padrão: `"cyan4"`. Pode ser um vetor de cores também.}

\item{digitos}{Número de casas decimais para arredondamento. Padrão: `2`.}

\item{virgula}{Lógico. Se `TRUE`, utiliza vírgula como separador decimal. Padrão: `FALSE`.}

\item{forcarpizza}{lógico (opcional, padrão = FALSE) se devemos forçar um gráfico de pizza ou deixa ele escolher (pizza para dois níveis e barras para mais de dois)}

\item{orient}{opcional (padrão = "auto"). Se auto, opta por vertical se a quantidade de categorias for menor que 7 e o tamanho dos labels de todas categorias for menor que 25 e horizontal caso contrário. Outras opções são "v" (gráfico vertical) e "h" (gráfico horizontal)}
}
\value{
Um objeto `list` contendo os seguintes elementos:
\item{result}{Data frame com as frequências absolutas, relativas e acumuladas.}
\item{texto}{Texto interpretativo sobre os grupos da variável categórica.}
\item{interp}{Interpretação textual resumida sobre a variável}
\item{tabela}{Resultado do teste qui-quadrado, se aplicável.}
\item{grafico}{Objeto gráfico (ggplot2) com o gráfico de barras ou pizza}
}
\description{
Esta função realiza uma análise descritiva univariada para variáveis categóricas, incluindo tabelas de frequência,
interpretação textual e gráficos.
}
\details{
A função produz:

- Uma tabela com frequência absoluta, relativa e opcionalmente acumulada.
- Um texto descritivo interpretativo da variável categórica.
- Opcionalmente, um gráfico de barras para visualização.

Se o parâmetro `teste` for ativado, realiza-se o teste qui-quadrado de aderência para verificar se a distribuição da variável é uniforme.
}
\examples{
# Criando um vetor categórico de exemplo
variavel <- sample(c("A", "B", "C"), size = 100, replace = TRUE, prob = c(0.2, 0.5, 0.3))

# Analisando a variável categórica
resultado <- desc_uni_categorica(
  variavel = variavel,
  nome = "Exemplo",
  ordenar = TRUE,
  teste = TRUE,
  grafico = TRUE
)

relatorio(resultado)

}
