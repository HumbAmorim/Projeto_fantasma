
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
  
  

Por meio do gráfico de barras implementa-se uma distribuição anual da quantidade de vendas. AValiando a quantidade de vendas por mês. 
vendas_2 <- vendas%>%
  rename(Categoria = Category)
  vendas_2 <- vendas_2%>%
  mutate(Categoria = recode(Categoria, 
                            "Kids' Fashion" = "Moda Infantil",
                            "Women's Fashion" = "Moda Feminina",
                            "Men's Fashion" = "Moda Masculina"))
vendas_2 <- vendas_2%>%
  filter(!is.na(`Data Venda`))%>%
  filter(!is.na(Categoria))%>%
  filter(!is.na(Price))
vendas_2$`Data Venda` <- mdy(vendas_2$`Data Venda`)
vendas_2$Mês <- format(vendas_2$`Data Venda`,"%b")
  vendas_2$Mês <- factor(c(vendas_2$Mês),levels = c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez"))
dados <- vendas_2%>%
  group_by(Mês,Categoria)%>%
  summarise( Faturamento = sum(Price))

View(dados)

 
ggplot(dados) +
  aes( x = Mês, y = Faturamento, group = Categoria, colour = Categoria) +
  geom_line(size = 1.5) +
  geom_point(size =2 ) +
  scale_colour_manual(name = "Categoria", labels = c("Moda masculina", "Moda feminina","Moda infantil")) +
  labs(x = "Mês", y = "Faturamento") +
  theme_estat()+
 scale_y_continuous(breaks = seq(0,3800,500))
ggsave(filename = file.path(caminho_Humberto, "Gráfico_12.pdf"), width = 158, height = 93, units = "mm")

eu <- vendas_2%>%
group_by(Categoria)%>%
  summarise(faturamento = sum(Price)
    
  )    
  


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


# Variação do preço por marca 
  A análise a seguir tem como objetivo acompanhar entender a variação preço de acordo com a marca do produto.
  Para esse estudo,utiliza-se as variáveis "Price" e "Brand",que são classificadas,respectivamente, como quantitativa contínua e qualitativa nominal.As observações da variável "Price", ou seja, os preços variam de 10 a 100 reais.
  
vendas <- vendas%>%
  filter(!is.na(Price))%>%
  filter(!is.na(Brand))
    ggplot(vendas) +
    aes(x = fct_reorder(Brand,Price), y = Price) +
    geom_boxplot(fill = c("#A11D21"), width = 0.5) +
    stat_summary(
      fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
    ) +
    labs(x = "Marcas", y = "Preços") +
    theme_estat()
  ggsave(filename = file.path(caminho_Humberto, "grafico_13.pdf"), width = 158, height = 93, units = "mm")
view(Resultado_7)
  library(dplyr)
  
  bloo <- vendas %>% 
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
  
  xtable::xtable(bloo)
  

Avaliando marca por marca, observa-se nos box plots e no quadro de medidas resumo uma certa proximidade
nos valores.Em todos os casos as medianas são semelhantes as médias,indicando uma distruibuição simétrica,onde não há muitos valores aberrantes.
Em todas as marcas, 50% dos valores se concentram abaixo dos 53 reais.Portanto, extrai-se desse estudo, que as marcas do empreendimento estão precificando seus produtos
similarmente.
  



# Análise 3
v <- vendas %>%
  group_by(Color)%>%
summarise(cont = n() )


vendas <- vendas%>%
filter(Category != "Kids' Fashion")%>%
rename(Categoria = Category)
vendas <- vendas%>%
mutate(Color = recode(Color, 
                          "Green" = "Verde",
                          "Blue" = "Azul",
                          "Red" = "Vermelho",
                          "White" = "Branco",
                          "Black" = "Preto",
                          "Yellow" = "Amarelo"))
vendas <- vendas%>%
  filter(!is.na(Color))%>%
  filter(!is.na(Categoria))

Quadro <- vendas %>%
  mutate(Categoria = case_when(
    Categoria %>% str_detect("Men's Fashion") ~ "Masculina",
    Categoria %>% str_detect("Women's Fashion") ~ " Feminina"
  )) %>%
  group_by(Color,Categoria) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq/sum(freq)*100,1)
      
  )

porcentagens <- str_c(Quadro$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(Quadro$freq, " (", porcentagens, ")"))

ggplot(Quadro) +
  aes(
    x = fct_reorder(Categoria, freq, .desc = T), y = freq_relativa,
    fill = Color, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Cores por categoria", y = "Frequência relativa") +
  theme_estat()

ggplot(Quadro) +
  aes(x= fct_reorder(Categoria,freq_relativa, .desc = TRUE), y = freq_relativa,fill = Categoria, label = legendas) +
  geom_bar(stat = "Identity")+
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .5),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  facet_wrap(Color ~ .) +
  labs(x="Categoria", y="Frequência absoluta") +
  ylim(0,60)+
  theme_estat(
    strip.text = element_text(size=12),
    strip.background = element_rect(colour="black", fill="white")
  ) 
ggsave(filename = file.path(caminho_Humberto, "Grafico_8.pdf"), width = 128, height = 130, units = "mm")

  

summary(vendas)
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



## Análise 5

library(readr)
devolução_atualizado <- read_csv("banco/devolução_atualizado.csv")
View(devolução_atualizado)

vendas <- left_join(devolução_atualizado, vendas,by = "Unique ID")


vendas <- vendas%>%
  filter(!is.na(Brand))%>%
  filter(!is.na(`Motivo devolução`))

  Quadro_2 <- vendas %>%
  mutate( Brand = case_when(
    Brand %>% str_detect("Nike") ~ "Nike",
    Brand %>% str_detect("Zara") ~ " Zara ",
    Brand %>% str_detect("H&M") ~ " H&M",
    Brand %>% str_detect("Adidas") ~ "Adidas ",
    Brand %>% str_detect("Gucci") ~ "Gucci"
    
    )) %>%
  group_by(Brand, `Motivo devolução`) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq/sum(freq)*100,1)
    
  )


porcentagens <- str_c(Quadro_2$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(Quadro_2$freq, " (", porcentagens, ")"))

ggplot(Quadro_2) +
  aes(
    x = fct_reorder(Brand, freq, .desc = T), y = freq,
    fill =`Motivo devolução`, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Marca", y = "Frequência") +
  coord_flip()+
  theme_estat()
ggsave(filename = file.path(caminho_Humberto, "Grafico_10.pdf"), width = 158, height = 110, units = "mm")



#Análise 6 (extra)

Neste estudo busca-se um aprofundamento na correlação entre as variáveis "Rating" e "Brand", de maneira a verificar se a marca do produto influencia ativamente na sua avaliação.
As variantes avaliação e marca dessa análise, podem ser caracterizadas como quantitativa contínua e qualitativa nominal, respectivamente.


vendas <- vendas%>%
  filter(!is.na(Rating))%>%
  filter(!is.na(Brand))
Gráfico_11 <- ggplot(vendas) +
  aes(x = fct_reorder(Rating,Brand), y = Rating) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marcas", y = "Avaliação") +
  theme_estat()
ggsave(filename = file.path(caminho_Humberto, "Gráfico_11.pdf"), width = 158, height = 93, units = "mm")
Gráfico_11

quadro_resumo <- vendas %>% 
  group_by(Brand) %>% 
  summarize(Média = round(mean(Rating), 2),
            `Desvio Padrão` = round(sd(Rating), 2),
            `Variância` = round(var(Rating), 2),
            `Mínimo` = round(min(Rating), 2),
            `1º Quartil` = round(quantile(Rating, probs = 0.25), 2),
            Mediana = round(quantile(Rating, probs = 0.5), 2),
            `3º Quartil` = round(quantile(Rating, probs = 0.75), 2),
            `Máximo` = round(max(Rating), 2)) %>%
  
  arrange(Média) %>%  # Ordenar pelo valor da média
  t() %>% as.data.frame()

xtable::xtable(quadro_resumo)






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


  
 


