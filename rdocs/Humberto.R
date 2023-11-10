
library(readr)
vendas <- read_csv("banco/vendas.csv")
library(readr)
devolucao <- read_csv("banco/devolucao.csv")
View(devolucao)
View(vendas)
install.packages("tidyverse")
library(tidyverse)
caminho_Humberto <- "resultados"
ggsave(filename = file.path(caminho_Humberto, "Quantidade/dep.pdf"), width = 158, height = 93, units = "mm")

vendas <- vendas %>%
  distinct(`Unique ID`, .keep_all = TRUE)
# Análise 3.1(introdução): Faturamento Anual do shopping.
Inicialmente, é importante salientar o resultado do faturamneto total do shopping, que é demosntrado pelo seguinte valor:
  
  Tabela_1 <- vendas%>%
  filter(!is.na(Price))%>%
  summarise(faturamento = sum(Price))
view(Tabela_1)
 # Análise 3.2 : Análise: categoria ; quantidade de vendas.
Para introduzir , observa-se um histograma indicando a quantidade de vendas para cada setor de vestuário apresentado no banco:

  vendas <- vendas%>%  
filter(!is.na(Category))
Grafico_1  <- ggplot(vendas) +
  aes(x = Category) +
  geom_bar(colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Departamento de roupas ", y = "Frequência Absoluta") +
  theme_estat()
view(Grafico_1)
ggsave(ggsave(filename = file.path(caminho_Humberto, "Grafico_1.pdf"), width = 158, height = 93, units = "mm")
)

Ao analisar o gráfico acima percebe-se uma proximidade entre os valores, de maneira em que o número de vendas de cada departamento é proximo porém com uma certa vantagem da sessão infantil.

#Análise Gráfico de setores:
 Ainda ilustrando a comparação de vendas entre os departamentos, têm-se um gráfico de stores:
   
   contagem <- vendas %>% 
   group_by(Category) %>%
   summarise(Freq = n()) %>%
   mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
   arrange(desc(Category)) %>%
   mutate(posicao = cumsum(Prop) - 0.5*Prop)
 
 ggplot(contagem) +
   aes(x = factor(""), y = Prop , fill = factor(Category)) +
   geom_bar(width = 1, stat = "identity") +
   coord_polar(theta = "y") +
   geom_text(
     aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
     color = "black"
   ) +
   theme_void() +
   theme(legend.position = "top") +
   scale_fill_manual(values = cores_estat, name = 'DRV')
 ggsave(filename = file.path(caminho_Humberto, "Gráfico_2.pdf"), width = 158, height = 93, units = "mm")
 
A representação anterior apenas comprova o equlíbrio da venda de roupasn no shopping com uma leve
sobreposição da moda infantil.

# Análise : Preço/Categoria

Nesta sessão a relação entre as variáveis preço e categoria será trabalhada cautelosamente para entregar uma resposta produtiva acerca da adequação dos preços dos produtos.

Inicialmente será introduzida um quadro de medidas centrais dividido pelas categorias para evidenciar os números:
  
  quadro_resumo <- vendas%>%
  filter(!is.na(Price))%>%
  group_by(Category) %>% # caso mais de uma categoria
  summarize(Média = round(mean(Price),2),
            `Desvio Padrão` = round(sd(Price),2),
            `Variância` = round(var(Price),2),
            `Mínimo` = round(min(Price),2),
            `1º Quartil` = round(quantile(Price, probs = .25),2),
            Mediana = round(quantile(Price, probs = .5),2),
            `3º Quartil` = round(quantile(Price, probs = .75),2),
            `Máximo` = round(max(Price),2)) %>% t() %>% as.data.frame()%>%
  rename(Moda_infantil = V1)%>%
  rename(Moda_Masculina = V2)%>%
  rename(Moda_Feminina = V3)
  

xtable::xtable(quadro_resumo)

# Análise BoxPlot
O quadro referenciado assume medidas , novamente, muito próximas.Medidas de posição estreitadas indicam uma distribuição devalores,dentro das categorias quase idênticas.
Pela semelhança entre as quantidades de venda e a média de preços pode-se inferir um faturamento bem próximo. Entretanto essa inferência será demonstrada mais adiante. Nesta sessão, no intuito de clarear o resultado obtido anteriormente,
uma apresentação de box plots:
  
  ggplot(vendas) +
  aes(x = Category, y = Price) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Categoria", y = "Preço") +
  theme_estat()
ggsave(filename = file.path(caminho_Humberto, "Gráfico_3.pdf"), width = 158, height = 93, units = "mm")

O resultado observado comprova a disposição das medidas resumo e sua semelhança.Além disso, constata-se a existência de outliers na ala infantil, ou seja valores distantes da médiana dos dados.Isso explica a acentução da variância e do desvio padrão no mesmo.
Porém, esses outliers não prejudicam a exploração dos dados,tendo em vista que não causam alguma assimetria no boxplot.   
 
#Análise: Quantidade de vendas Anual:

Por meio do gráfico de barras implementa-se uma distribuição anual da quantidade de vendas. AValiando a quantidade de vendas por mês. 

vendas_2 <- filter(vendas,!is.na(vendas$`Data Venda`))
vendas_2$`Data Venda` <- mdy(vendas_2$`Data Venda`)
vendas_2$mes <- format(vendas_2$`Data Venda`,"%B")
 bl <- vendas_2%>%
  group_by(mes)%>%
  summarise(qtd = n())
ggplot(bl) +
  aes(x = fct_reorder(mes,qtd),y = qtd) +
  geom_bar(stat = "identity",colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Meses ", y = "Frequência Absoluta") +
  theme_estat()+
  coord_flip()
ggsave(filename = file.path(caminho_Humberto, "Grafico_4.pdf"), width = 158, height = 93, units = "mm")

Tendo observado o gráfico acima nota uma concentração discrepante no número de vendas nos meses de
dezembro e janeiro, meses que coincidem com o período de férias de uma grande parcela da população.

#Análise : Faturamento Anual:
Agora com relação ao faturamento em cada mês do ano se destaca o seguinte gráfico:
  

bl_2 <- vendas_2%>%
  group_by(mes)%>%
  filter(!is.na(Price))%>%
  summarise(faturamento = sum(Price))
ggplot(bl_2) +
  aes(x = fct_reorder(mes,faturamento),y = faturamento) +
  geom_bar(stat = "identity",colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Meses ", y = "Faturamento") +
  theme_estat()+
  coord_flip()
ggsave(filename = file.path(caminho_Humberto, "Grafico_5.pdf"), width = 158, height = 93, units = "mm")

Como esperado, o resultado segue a mesma proposta do gráfico anterior,se distinguindo pelo valor do eixo "x" que agora representa o faturamento.

#Análise Faturamento por categoria:

Nessa sessão o gráfico de barras exibirá o faturamento anual por categoria:

  tbl_4 <- vendas%>%
  group_by(Category)%>%
  filter(!is.na(Price))%>%
  filter(!is.na(Category))%>%
  summarise(Faturamento = sum(Price))
  ggplot(tbl_4) +
  aes(x = fct_reorder(Category,Faturamento),y = Faturamento) +
  geom_histogram(stat = "identity",colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Categoria", y = "Faturamento") +
  theme_estat()
  ggsave(filename = file.path(caminho_Humberto, "Grafico_6.pdf"), width = 158, height = 93, units = "mm")
  
  
#Análise Faturamento Anual por Categoria
  
  Categorias <- vendas%>%
    filter(!is.na(Category)) %>%
    count(Category) %>%
    mutate(
      freq = n %>% percent(),
    ) %>%
    mutate(
      freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
      label = str_c(n, " (", freq, ")") %>% str_squish()
    )
  
  ggplot(Categorias) +
    aes(x = fct_reorder(Category, n, .desc=T), y = n, label = label) +
    geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
    geom_text(
      position = position_dodge(width = .9),
      vjust = -0.5, #hjust = .5,
      size = 3
    ) + 
    labs(x = "Categorias", y = "Frequência") +
    theme_estat()


  summary(vendas$Price)
# Variação do preço por marca 
  A análise a seguir tem como objetivo acompanhar entender a variação preço de acordo com a marca do produto.
  Para esse estudo,utiliza-se as variáveis "Price" e "Brand",que são classificadas,respectivamente, como quantitativa contínua e qualitativa nominal.As observações da variável "Price", ou seja, os preços variam de 10 a 100 reais.
  
vendas <- vendas%>%
  filter(!is.na(Price))%>%
  filter(!is.na(Brand))
  Resultado_7 <- ggplot(vendas) +
    aes(x = fct_reorder(Brand,Price), y = Price) +
    geom_boxplot(fill = c("#A11D21"), width = 0.5) +
    stat_summary(
      fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
    ) +
    labs(x = "Marcas", y = "Preços") +
    theme_estat()
  ggsave(filename = file.path(caminho_Humberto, "Resultado_7.pdf"), width = 158, height = 93, units = "mm")
view(Resultado_7)
  library(dplyr)
  
  quadro_resumo <- vendas %>% 
    group_by(Brand) %>% 
    summarize(Média = round(mean(Price), 2),
              `Desvio Padrão` = round(sd(Price), 2),
              `Variância` = round(var(Price), 2),
              `Mínimo` = round(min(Price), 2),
              `1º Quartil` = round(quantile(Price, probs = 0.25), 2),
              Mediana = round(quantile(Price, probs = 0.5), 2),
              `3º Quartil` = round(quantile(Price, probs = 0.75), 2),
              `Máximo` = round(max(Price), 2)) %>%
    arrange(Média) %>%  # Ordenar pelo valor da média
    t() %>% as.data.frame()
  
  xtable::xtable(quadro_resumo)
  

Avaliando marca por marca, observa-se nos box plots e no quadro de medidas resumo uma certa proximidade
nos valores.Em todos os casos as medianas são semelhantes as médias,indicando uma distruibuição simétrica,onde não há muitos valores aberrantes.
Em todas as marcas, 50% dos valores se concentram abaixo dos 53 reais.Portanto, extrai-se desse estudo, que as marcas do empreendimento estão precificando seus produtos
similarmente.
  



# Análise 3
v <- vendas %>%
  group_by(Color)%>%
summarise(cont = n() )

A análise a seguir nos permite compreender 

vendas <- vendas%>%
filter(Category != "Kids' Fashion")
vendas <- vendas%>%
  filter(!is.na(Color))%>%
  filter(!is.na(Category))

Quadro <- vendas %>%
  mutate(Category = case_when(
    Category %>% str_detect("Men's Fashion") ~ "Masculina",
    Category %>% str_detect("Women's Fashion") ~ " Feminina"
  )) %>%
  group_by(Color,Category) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq/sum(freq)*100,1)
      
  )

porcentagens <- str_c(Quadro$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(Quadro$freq, " (", porcentagens, ")"))

ggplot(Quadro) +
  aes(
    x = fct_reorder(Category, freq, .desc = T), y = freq_relativa,
    fill = Color, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Cores por categoria", y = "Frequência") +
  theme_estat()

ggplot(Quadro) +
  aes(x= fct_reorder(Category,freq_relativa, .desc = TRUE), y = freq_relativa,fill = Category, label = legendas) +
  geom_bar(stat = "Identity")+
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .5),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  facet_wrap(Color ~ .) +
  labs(x="Categoria", y="Frequência relativa") +
  ylim(0,60)+
  theme_estat(
    strip.text = element_text(size=12),
    strip.background = element_rect(colour="black", fill="white")
  ) 
ggsave(filename = file.path(caminho_Humberto, "Grafico_8.pdf"), width = 128, height = 130, units = "mm")

  


# análise 4

Analisou-se nesse estudo como se comportamento das variáveis "Price" e "Rating".Ambas as variáveis são quantitativas contínuas, com a variável "Rating" representando as notas dos produtos, que varia de 1.056 a 4.183 pontos.


vendas <- vendas%>%
filter(!is.na(Price))%>%
filter(!is.na(Rating))

ggplot(vendas) +
  aes(x = Rating, y = Price) +
  geom_point(colour = "#A11D21", size = 2, alpha = 0.75) +
  labs(
    x = " Nota do produto",
    y = "Valor do produto "
  ) +
  theme_estat()
ggsave(filename = file.path(caminho_Humberto, "Grafico_9.pdf"), width = 158, height = 93, units = "mm")

 H <- cor.test(vendas$Price ,vendas$Rating ,method = "spearman")
print(H)

A partir da observação do gráfico de dispersão acima, é possível identificar um comportamento controlado da nuvem de pontos,ou seja,
os valores observados aparentam seguir uma proporcionalidade. Analogamente, o gráfico apresenta
 uma correlção intensa entre as variáveis analisadas, de maneira que o preço dos produtos sobe ou desce, conforme sua avaliação.
Com o objetivo de ratificar esse resultado, foi obtido o coeficiente de spearman para as variáveis em questão, apontando o valor de 0.9054.
Sabendo que, o teste de pearson varia de -1 a 1, sendo -1 e 1 os extremos que sinalizam uma maior correlação, vizualiza-se o valor de 0.9054 como um indicador de uma forte correlação entre as variantes em pauta.



theme_estat <- function(...) {
 theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}

 
cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")


  
 


