install.packages(tidyverse)
install.packages(sidrar)


library(tidyverse)
library(sidrar)

#baixando o dado do sidra

ipca_raw <- get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202")

#Para saber o número de colunas, linhas e nome das colunas

dplyr::glimpse(ipca_raw)

#selecionando apenas as colunas "valor" "mês(código)"
# para alterar o nome da coluna uso "select" do dplyr
# para alterar a data para year month usar "mutate" do dplyr e "ym" do lubridate
ipca <- ipca_raw |>
  dplyr::select("data"="Mês (Código)","ipca"="Valor")|>
  dplyr::mutate(data = lubridate::ym(data))|>
  dplyr::filter(data >= "2004-01-01")|>
  dplyr::as_tibble()
#para obter uma análise preliminar dos dados usamos

summary(ipca)

# análise exploratória (gráficos com ggplot2)

# gráfico de linha
ggplot2::ggplot(ipca)+
  ggplot2::aes(x = data, y = ipca)+
  ggplot2::geom_line()

#gráfico bloxpot
ggplot2::ggplot(ipca)+
  ggplot2::aes(y=ipca)+
  ggplot2::geom_boxplot()

#gráfico histograma
ggplot2::ggplot(ipca)+
  ggplot2::aes(y=ipca)+
  ggplot2::geom_histogram()

#Baixando os dados sobre emprego

desocupação_raw <- sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")

#limpeza de dados
# selecionando e alterando nome das colunas com "select",alterando a data com "mutate" e "ym"

desocupação <- desocupação_raw|>
  dplyr::select("data" = "Trimestre Móvel (Código)", "desocupação" = "Valor" )|>
  dplyr::mutate(data = lubridate::ym(data))|>
  dplyr::as_tibble()

#para unir as duas tabelas/objetos usaremos "inner_join" unimos ipca e desocupação pela 
#coluna data

df_dado <- inner_join(ipca, desocupação, by = "data")

gráficos

df_dado |>
  ggplot2::ggplot()+
  ggplot2::aes(x = data)+
  ggplot2::geom_line(aes(y=desocupação, color = "taxa de desocupação"))+
  ggplot2::geom_line(aes(y = ipca, color = "ipca"))+
  ggplot2::scale_color_manual(values = c("#282f6b", "#b22200"))

#fazendo uma regressão ols com a função "lm"

modelo_phillips <- lm(ipca ~ desocupação, data = df_dado)

summary(modelo_phillips)

