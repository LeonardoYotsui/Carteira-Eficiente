# Preparando ambiente
library(quantmod)
library(ggplot2)
library(magrittr)
library(xts)
library(tseries)

# Atribuindo variáveis 

Acoes <- c('ABEV3.SA','B3SA3.SA','BBDC3.SA','CPLE6.SA','GGBR4.SA','ITSA4.SA','PETR4.SA','PSSA3.SA','SAPR11.SA','TAEE11.SA','VALE3.SA')

# Puxando informações das ações

ativos <- new.env()

getSymbols(Symbols = Acoes, env = ativos, src = 'yahoo', from = '2018-01-01', to = '2023-06-05', auto.assign = TRUE)

ativos <- as.list(ativos)

# Preparando as funções

Preco_Fechamento <- function(x){
  #Obtém o preço de fechamento
  x[,grepl(pattern = '\\.Close$$', x = dimnames(x)[[2]])]
}

# Filtrando apenas o preço de fechamento

ativos <- lapply(X = ativos, FUN = Preco_Fechamento)

# Calculando os retornos mensais

retornos_men <- lapply(
  X = ativos, 
  FUN = monthlyReturn
)

# Unindo tudo em um único objeto xts

list_to_xts <- function(list_xts){
  ## Transforma uma lista de objetos xts em um objeto xts
  nomes <- names(list_xts)
  res <- do.call(cbind.xts, list_xts)
  dimnames(res)[[2]] <- nomes
  
  return(res)
}

carteira <- list_to_xts(retornos_men)


# Transformando objeto xts em uma matriz e retirando dados perdidos

carteira <- as.matrix(carteira)
carteira <- na.omit(carteira)

# Criando função para portfolio.optim()

carteira_eficiente <- function(ativos, retorno_desejado, shorts = FALSE) {
  ## Calcula carteira otima
  
  s <- portfolio.optim(
    x = ativos,
    pm = retorno_desejado,
    shorts = shorts
  )
  
  pesos <- s[['pw']]
  names(pesos) <- colnames(ativos)
  
  res <- list(
    pesos = pesos,
    retorno = s[['pm']],
    risco = s[['ps']]
  )
  
  return(res)
}

# Aplicando a média e a variação dos retornos

retornos_esperados <- apply(carteira, MARGIN = 2, mean)
riscos <- apply(carteira, MARGIN = 2, sd)

ativos <- data.frame(
  acao = names(retornos_esperados),
  risco = riscos,
  retorno = retornos_esperados,
  row.names = NULL
)

# Calculando pontos de mínima e baixo considerando a média -+ desvios-padrões

retorno_min <- 0.5*min(retornos_esperados)
retorno_max <- 1.5*max(retornos_esperados)

# Estabelecendo uma sequência de retornos possíveis para gerar a curva

retornos_seq <- seq(retorno_min, retorno_max, length.out = 100)

# Estabelecendo a curva da fronteira eficiente

curva <- lapply(
  X = retornos_seq,
  FUN = carteira_eficiente,
  ativos = carteira, 
  shorts = TRUE
)

# Criando data frame da curva


get_retorno <- function(res){
  ##  Obtém o retorno da carteira ótima
  ## res: list
  
  sapply(res,'[[',i = 'retorno')
}

get_risco <- function(res){
  ## Obtém o risco da carteira otima
  ## res: list
  
  sapply(res,'[[',i = 'risco')
}

curva_plot <- data.frame(
  retorno = get_retorno(curva),
  risco = get_risco(curva)
)

# Plotando a curva

curva_plot %>%
  ggplot(aes(x = retorno, y = risco, color = 'HE7B800'))+
  geom_line()+
  geom_point(data = ativos, aes(x = retorno, y = risco, color = acao))+
  geom_text(data = ativos, aes(x = retorno, y = risco, label = acao, color = acao))+
  coord_flip()+
  labs(
    x = 'Retorno',
    y = 'Risco', 
    title = 'Fronteira Eficiente',
    color = ''
  )+
  theme(legend.position = 'none')

# Adicionando a carteira do cliente

Pesos <- c(0.042711,0.012926, 0.140723, 0.122644, 0.091918, 0.04668, 0.091098, 0.024296, 0.220527, 0.128901, 0.077578)

# Criando função igual a anterior utilizando a variável de pesos

Carteira_eficiente_pesos <- function(ativos, pesos, shorts = FALSE) {
  ## Calcula carteira otima com peso minimo
  
  s <- portfolio.optim(
    x = ativos,
    pw = pesos,
    shorts = shorts,
  )
  
  pesos <- s[['pw']]
  names(pesos) <- colnames(ativos)
  
  res <- list(
    pesos = pesos,
    retorno = s[['pm']],
    risco = s[['ps']]
  )
  
  return(res)
}

Carteira_cliente <- Carteira_eficiente_pesos(carteira, pesos, shorts = FALSE)

# Plotando carteira do cliente

cliente_plot <- data.frame(
  retorno = Carteira_cliente[2],
  risco = Carteira_cliente[3]
)

curva_plot %>%
  ggplot(aes(x = retorno, y = risco, color = 'HE7B800'))+
  geom_line()+
  geom_point(data = cliente_plot, aes(x = retorno, y = risco))+
  geom_text(data = cliente_plot, aes(x = retorno, y = risco, label = "Cliente"))+
  geom_text(data = ativos, aes(x = retorno, y = risco, label = acao, color = acao))+
  geom_point(data = ativos, aes(x = retorno, y = risco, color = acao))+
  coord_flip()+
  labs(
    x = 'Retorno',
    y = 'Risco', 
    title = 'Fronteira Eficiente',
    color = ''
  )+
  theme(legend.position = 'none')
