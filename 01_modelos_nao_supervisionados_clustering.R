# carregando pacotes necessários
# install.packages("nomedopacote")
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(randomForest)
library(PerformanceAnalytics)
library(corrplot)
library(cluster)
library(dbscan)
library(fclust)
library(ClusterR)
library(gridExtra)
library(cowplot)

# Insira o caminho do arquivo do Excel de demonstração
df <- read_excel("seudiretorio/demo_clientes.xlsx")

# Definir as variáveis a serem padronizadas
variaveis <- names( df[,-1] )

# Função para padronizar (z-score)
padronizar <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# Aplicar a padronização
df_padronizado <- df[,-1] %>%
  mutate(across(all_of(variaveis), padronizar),
         ref = 1) 

# Para evitar que os valores apareçam em notação cientfica
options(scipen = 999)

# Calculando as estatísticas
estatisticas <- df %>%
  pivot_longer(cols = -cliente, names_to = "variavel", values_to = "valor") %>%
  group_by(variavel) %>%
  summarise(
    Minimo = min(valor, na.rm = TRUE),
    Q1 = quantile(valor, 0.25, na.rm = TRUE),
    Media = mean(valor, na.rm = TRUE),
    Mediana = median(valor, na.rm = TRUE),
    Q3 = quantile(valor, 0.75, na.rm = TRUE),
    Maximo = max(valor, na.rm = TRUE)
  ) %>%
  arrange(desc(variavel))

# arrendondando para 2 casas decimais para melhor visualização
estatisticas <- estatisticas %>%
  mutate(across(where(is.numeric), ~round(., 2)))


# Criando o boxplot
boxplot <- df_padronizado %>%
  pivot_longer(cols = -ref, names_to = "variavel", values_to = "valor") %>%
  ggplot(aes(x = variavel, y = valor)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribuição das variáveis padronizadas", x = "Variável", y = "Valor") +
  coord_flip()

# Visualizando o gráfico
boxplot


# Segmentação - exemplo 1/3

# Ex1.1 - Pareto
df_pareto <- df %>%
  group_by(cliente) %>%
  summarise(total = sum(receita)) %>%
  arrange( desc(total) ) %>%
  mutate(percentual = total / sum(total) * 100,
         cum_percentual = cumsum(percentual),
         perc_cliente = row_number() / n() * 100 ) 
  
# Gráfico de Pareto
ggplot(df_pareto, aes(x = perc_cliente, y = cum_percentual)) +
  geom_point() +
  labs(title = " ", # Pode inserir o título do gráfico
       x = "% Clientes",
       y = "% Receita") +
  theme_minimal() # + geom_hline(yintercept = 80, linetype = "dashed", color = "red") + # Tire o # do início dessa linha caso queira ver a linha de corte no gráfico

# Ex1.2 - Dispersão
# Calculando as medianas das variáveis 'soc' e 'margem' usando o dataframe 'estatisticas'
mediana_soc <- estatisticas %>% filter(variavel == "soc") %>% pull(Mediana)
mediana_margem <- estatisticas %>% filter(variavel == "margem") %>% pull(Mediana)

ggplot(df, aes(x = soc, y = margem ) ) +
  geom_point() +
  geom_vline(xintercept = mediana_soc, color = "blue", linetype = "dashed", size = 0.8) +
  geom_hline(yintercept = mediana_margem, color = "blue", linetype = "dashed", size = 0.8) +
  labs(title = " ", # Pode inserir o título do gráfico
       x = "% share of customer",
       y = "% margem") +
  theme_minimal()


# Segmentação - exemplo 2/3

# Gamificação

# Segmentação - exemplo 3/3

# 3.1 Avaliando correlação entre as variáveis
corrplot(cor(df_padronizado), method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", # Adiciona os coeficientes de correlação
         number.cex = 0.8, # Aumenta o tamanho dos coeficientes
         col = colorRampPalette(c("darkred", "white", "darkblue"))(200), # Cores mais contrastantes
         cl.pos = "r", # Posição da legenda (direita)
         cl.cex = 1, # Aumenta o tamanho da legenda
         tl.cex = 1, # Aumenta o tamanho dos rótulos
         diag = FALSE
         )

# chart.Correlation(df_padronizado, histogram = TRUE, method = "pearson")
# chart.Correlation(df_padronizado[,1:6], histogram = TRUE, method = "pearson") # Apenas para simplificar a visualização

# 3.2 O método mais usual para inferir a quantidade de grupos

k_max <- 15

wss <- sapply( 1:k_max, 
               function(k){
                 kmeans( df_padronizado, k, nstart = 50, iter.max = 15 )$tot.withinss
               } )

ws <- wss %>% data.table()

names(ws) <- c("Soma total dos erros")
ws$k <- seq.int( nrow(ws) )
ws$ref <- round( ws$`Soma total dos erros` / lag( ws$`Soma total dos erros`, 1 ) - 1, 3 )

ggplot( data = ws, aes( y = `Soma total dos erros`, x = k ) ) + geom_line() + geom_point() +
  scale_y_continuous(labels = scales::comma) + 
  ggtitle(" ") + 
  theme( plot.title = element_text( size = 15 ) ) +
  theme_bw()

# Apesar do método do elbow chart, a definição do número do cluster deve ser uma definição de negócio, utilizando a análise para direcionar,
# mas, principalmente algo que seja factível ao contexto de negócio

####### K-means #######

# um dos algoritmos de clustering mais populares e simples. Ele divide os dados em K clusters, onde K é especificado previamente.
# O algoritmo funciona atribuindo cada ponto ao cluster cujo centróide (média) está mais próximo,
# e então recalcula os centróides. Este processo é repetido até que os centróides estabilizem. K-Means é eficiente e
# funciona bem para clusters globulares de tamanhos similares

set.seed(42)  # Escolha um número fixo

kmeans_resultado <- kmeans(df_padronizado, centers = 6 )

# Modelo usando o random forest para avaliar as variáveis mais relevantes
dados <- df_padronizado %>% mutate( cluster_means = kmeans_resultado$cluster )

rf_modelo <- randomForest(cluster_means ~ ., data = dados, importance = TRUE)

importancia <- importance(rf_modelo)
importancia_ordenada <- importancia[order(importancia[,"%IncMSE"], decreasing = TRUE),]

# Criar o dataframe para o plot
df_importancia <- data.frame(
  variavel = rownames(importancia_ordenada),
  importancia = importancia_ordenada[,"%IncMSE"]
)

ggplot(df_importancia, aes(x = reorder(variavel, importancia), y = importancia)) +
  geom_point() +
  geom_segment(aes(x = variavel, xend = variavel, y = 0, yend = importancia)) +
  coord_flip() +
  labs(title = "Importância das Variáveis (%IncMSE)",
       x = "Variáveis",
       y = "%IncMSE") +
  theme_minimal()

# Calcular estatísticas descritivas para as top 5 variáveis por cluster = ked - k-means estatística descritiva
top_5_variaveis <- names(sort(importancia[,"%IncMSE"], decreasing = TRUE))[1:5]

ked <- dados %>%
  group_by(cluster_means) %>%
  summarise(across(all_of(top_5_variaveis), 
                   list(n = ~n(),
                        media = ~mean(.x, na.rm = TRUE),
                        mediana = ~median(.x, na.rm = TRUE),
                        desvio_padrao = ~sd(.x, na.rm = TRUE),
                        minimo = ~min(.x, na.rm = TRUE),
                        maximo = ~max(.x, na.rm = TRUE))))

# Criar boxplots para as top 5 variáveis
dados_long <- dados %>%
  dplyr::select(cluster_means, all_of(top_5_variaveis)) %>%
  pivot_longer(cols = -cluster_means, names_to = "Variavel", values_to = "Valor")%>%
  mutate(cluster_means = as.factor(cluster_means))

ggplot(dados_long, aes(x = cluster_means, y = Valor, group = cluster_means)) +
  # geom_boxplot(aes(fill = cluster_means)) +
  geom_boxplot() +
  facet_wrap(~ Variavel, scales = "free_y", ncol = 3) +
  theme_minimal() +
  labs(title = "Distribuição das 5 Principais Variáveis por Cluster",
       x = "Cluster",
       y = "Valor Padronizado") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set3")

df$cluster <- kmeans_resultado$cluster

tabela_resumo <-df[,-1] %>%
  group_by(cluster) %>%
  summarise(across(all_of(variaveis), sum, na.rm = TRUE)) %>%
  mutate( perc = receita / sum(receita) * 100 )


# Análise dos Clusters baseada no boxplot das 5 principais variáveis

# Cluster 1: "Clientes de Baixa Margem e Volume"
# Margem: Levemente abaixo da média
# Mix de Produtos: Baixo
# Potencial: Médio-baixo
# Volume: Baixo
# YoY: Médio-alto

# Características: Clientes com margens e volume relativamente baixos, mas apresentam um crescimento YoY razoável. Têm potencial limitado, mas com espaço para crescimento no mix de produtos.

# Recomendação: Focar no aumento do mix de produtos para melhorar o potencial e o volume de compras.

# Cluster 2: "Clientes Diversificados com Potencial para Crescimento"
# Margem: Média
# Mix de Produtos: Alto
# Potencial: Médio
# Volume: Médio
# YoY: Levemente abaixo da média

# Características: Esse grupo possui um mix de produtos diversificado e volume estável, com uma margem razoável. Contudo, o crescimento YoY e o potencial ainda são moderados.

# Recomendação: Incentivar esses clientes a expandirem o volume de compras e melhorar o YoY com estratégias de retenção.

# Cluster 3: "Clientes de Alto Valor com Mix de Produtos Amplo"
# Margem: Muito alta
# Mix de Produtos: Alto
# Potencial: Alto
# Volume: Médio-alto
# YoY: Médio

# Características: Clientes com margens muito altas e bom volume de compras. Apresentam um mix de produtos bem diversificado e alto potencial. Estão em um bom estágio de desenvolvimento.

# Recomendação: Manter o foco em programas de fidelização e incentivar novos produtos para manter o alto valor desses clientes.

# Cluster 4: "Clientes em Expansão de Potencial"
# Margem: Moderada
# Mix de Produtos: Médio-alto
# Potencial: Alto
# Volume: Moderado
# YoY: Acima da média

# Características: Clientes com crescimento de volume, margem e mix em níveis adequados. Eles têm um potencial relativamente alto, mas com oportunidade de aumento de volume e mix.

# Recomendação: Expandir o mix de produtos e aumentar o volume de vendas por cliente para capitalizar o crescimento YoY positivo.

# Cluster 5: "Clientes de Alto Volume e Potencial Limitado"
# Margem: Muito alta
# Mix de Produtos: Muito baixo
# Potencial: Muito alto
# Volume: Muito alto
# YoY: Alto

# Características: Esses clientes têm margens e volume extremamente altos, mas com mix de produtos muito restrito. O potencial para crescimento futuro é grande, principalmente se puderem diversificar suas compras.

# Recomendação: Priorizar a expansão do mix de produtos e melhorar o envolvimento com outras categorias para aproveitar o grande potencial.

# Cluster 6: "Clientes Emergentes com Crescimento Acelerado"
# Margem: Baixa

# Mix de Produtos: Médio-baixo
# Potencial: Moderado
# Volume: Médio
# YoY: Muito alto

# Características: Clientes em fase de crescimento acelerado, com grande aumento YoY. No entanto, a margem ainda é baixa e o mix de produtos precisa ser expandido.

# Recomendação: Focar em estratégias para melhorar a margem e diversificar o mix de produtos, aproveitando o forte crescimento recente.

# Próximos passos:
# 1. Validar os perfis dos clusters com equipes de vendas e atendimento ao cliente.
# 2. Desenvolver estratégias de marketing e vendas personalizadas para cada cluster.
# 3. Monitorar a evolução dos clientes entre clusters ao longo do tempo.
# 4. Realizar análises mais profundas para entender os fatores que impulsionam o potencial e o crescimento.


####### Fuzzy C-Means #######

# O Fuzzy C-Means é uma técnica de clustering que permite que cada ponto de dados pertença a múltiplos clusters com 
# diferentes graus de pertencimento. Diferentemente do K-Means tradicional, onde cada ponto pertence a apenas um cluster, 
# o FCM atribui um grau de pertencimento para cada ponto em relação a todos os clusters. Isso é particularmente útil quando 
# os limites entre clusters não são bem definidos.

fcm_resultado <- FKM(df_padronizado, k = 6)

fcm_resultado[["clus"]][,1]

dados <- dados %>% mutate( cluster_fuzzymeans = fcm_resultado[["clus"]][,1] )

dados_long <- dados %>%
  dplyr::select(cluster_fuzzymeans, all_of(top_5_variaveis)) %>%
  pivot_longer(cols = -cluster_fuzzymeans, names_to = "Variavel", values_to = "Valor")%>%
  mutate(cluster_fuzzymeans = as.factor(cluster_fuzzymeans))

ggplot(dados_long, aes(x = cluster_fuzzymeans, y = Valor, group = cluster_fuzzymeans)) +
  geom_boxplot() +
  facet_wrap(~ Variavel, scales = "free_y", ncol = 3) +
  theme_minimal() +
  labs(title = "Distribuição das 5 Principais Variáveis por Cluster - Fuzzy",
       x = "Cluster",
       y = "Valor Padronizado") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set3")

####### Agglomerative Clustering #######

# Agglomerative Clustering é um método de clustering hierárquico que começa tratando cada observação como um cluster separado e, 
# em seguida, combina clusters progressivamente com base em uma medida de similaridade. No código fornecido, usamos o método "ward.D2", 
# que minimiza a variância dentro dos clusters. Este método é útil para descobrir estruturas hierárquicas nos dados e não requer a 
# especificação prévia do número de clusters

agglomerative_resultado <- hclust(dist(df_padronizado), method = "ward.D2")
dados <- dados %>% mutate( cluster_agglomerative = cutree(agglomerative_resultado, k = 6) )

dados_long <- dados %>%
  dplyr::select(cluster_agglomerative, all_of(top_5_variaveis)) %>%
  pivot_longer(cols = -cluster_agglomerative, names_to = "Variavel", values_to = "Valor")%>%
  mutate(cluster_agglomerative = as.factor(cluster_agglomerative))

ggplot(dados_long, aes(x = cluster_agglomerative, y = Valor, group = cluster_agglomerative)) +
  geom_boxplot() +
  facet_wrap(~ Variavel, scales = "free_y", ncol = 3) +
  theme_minimal() +
  labs(title = "Distribuição das 5 Principais Variáveis por Cluster - Agglomerative",
       x = "Cluster",
       y = "Valor Padronizado") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set3")
