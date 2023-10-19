
# Torres-Reyna, Oscar. 2014.

# Carregando os pacotes necessários
install.packages("haven") 
library(haven)

install.packages("plm")
library(plm)

install.packages("tidyverse")
library(tidyverse)

# Carregando a base de dados do Stata
url <- "/home/nepo26/git/previsao-leite/tarifasocial.csv"
#dados <- haven::read_dta(url)
dados <- read_csv(url)

# Visualizando as primeiras linhas da base de dados
head(dados)

# Calculando estatísticas descritivas
summary(dados)

# Convertendo a base de dados para um painel
painel <- pdata.frame(dados, index = c("country", "year"))

# Verificando a estrutura do painel
summary(painel)

# Criando a variável "treated"
treated <- ifelse(dados$country %in% c(5, 6, 7), 1, 0)

# Ordenando a base de dados pelo ID e pelo ano
dados <- dados[order(dados$country, dados$year),]

# Criando a variável "post"
post <- ifelse(dados$year > 1994, 1, 0)

# Realizando a análise de diferenças em diferenças
modelo <- plm(y ~ treated * post, data = painel, model = "within", effect = "twoways")

# Apresentando os resultados
summary(modelo)

# Realizando a análise de diferenças em diferenças com x1
modelo2 <- plm(y ~ treated * post + x1, data = painel, model = "within", effect = "twoways")

# Apresentando os resultados
summary(modelo2)

# Realizando a análise de diferenças em diferenças com x1
modelo3 <- plm(y ~ treated * post + x1 +x2 + x3, data = painel, model = "within", effect = "twoways")

# Apresentando os resultados
summary(modelo3)


# Carregando os pacotes necessários p/ análise gráfica
install.packages("ggplot2") 
library(ggplot2)


# Obtendo os resíduos do modelo
residuos <- resid(modelo3)

# Criando um gráfico de dispersão dos resíduos pelo tempo
ggplot(dados, aes(x = year, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Ano") +
  ylab("Resíduos") +
  ggtitle("Gráfico de Resíduos")

### TABELAS ###
install.packages("stargazer") 
library(stargazer)

# Criando a tabela dos modelos
tabela_modelos <- stargazer(modelo, modelo2, modelo3, title = "Resultados dos Modelos",
                            align = TRUE, style = "default", type = "text")

# Imprimindo a tabela
cat(tabela_modelos)
