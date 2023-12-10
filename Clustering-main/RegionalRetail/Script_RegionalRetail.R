## Clusters Analysis
## Source: Fávero e Belfiore, MANUAL DE ANÁLISE DE DADOS, Capítulo 09

# Course: MBA DSA USP ESALQ

# Prof. Wilson Tarantin Jr.
# Isabela Pereira Lima Dias

#Packages
set_packages <-c("plotly", 
                 "tidyverse",
                 "ggrepel", 
                 "knitr", "kableExtra",
                 "reshape2",
                 "misc3d", 
                 "plot3D", 
                 "cluster", 
                 "factoextra",
                 "ade4") 
if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  instalador <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}
#Load dataset
load(file="RegionalRetail.RData")

# Contextualization: average scores from regional retailers
# 18 stores and 3 measured attributes 
# the stores are divided into 3 regional retailers

#data visualization
RegionalVarejista %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)
#  3D Scatter Plot
rownames(RegionalVarejista) <- RegionalVarejista$loja

scatter3D(x=RegionalVarejista$atendimento,
          y=RegionalVarejista$sortimento,
          z=RegionalVarejista$organização,
          phi = 0, bty = "g", pch = 20, cex = 2,
          xlab = "Service",
          ylab = "Assortment",
          zlab = "Organization",
          main = "Stores",
          clab = "Average Score")>
  text3D(x=RegionalVarejista$atendimento,
         y=RegionalVarejista$sortimento,
         z=RegionalVarejista$organização,
         labels = rownames(RegionalVarejista),
         add = TRUE, cex = 1)

#Descriptive statistics
summary(RegionalVarejista$atendimento)
summary(RegionalVarejista$sortimento)
summary(RegionalVarejista$organização)

#-----------------------------Agglomerative Hierarchical Clustering
#dissimilarity matrix
matrix_D <- RegionalVarejista %>% 
  select(atendimento, sortimento, organização) %>% 
  dist(method = "euclidean")
#visualizing the matrix_D
data.matrix(matrix_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
#Cluster method single linkage or nearest neighbor
cluster_hier <- agnes(x = matrix_D, method = "single")

#the distances for the combination in each stage
coeff <- sort(cluster_hier$height, decreasing = FALSE) 
coeff

#Table 

scheme <- as.data.frame(cbind(cluster_hier$merge, coeff))
names(scheme) <- c("Cluster1", "Cluster2", "Coefficients")
scheme

#Visualization

# Visualização do esquema hierárquico de aglomeração
scheme %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
#dendogram
dev.off()
fviz_dend(x=cluster_hier)

# Defining 3 clusters
fviz_dend(x = cluster_hier,
          k = 3,
          k_colors = c("deeppink4", "darkviolet", "darkblue"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

#Creating a categorical variable to indicate the cluster (in the database)
RegionalVarejista$cluster_H <- factor(cutree(tree = cluster_hier, k = 3))

#Regional retail with  the respective cluster
RegionalVarejista %>%
  select(regional, cluster_H) %>% 
  arrange(regional) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 14)
#Descriptive statistics for each variable (service, assortment and organization)

#service
group_by(RegionalVarejista, cluster_H) %>%
  summarise(
    mean = mean(atendimento, na.rm = TRUE),
    sd = sd(atendimento, na.rm = TRUE),
    min = min(atendimento, na.rm = TRUE),
    max = max(atendimento, na.rm = TRUE))

# 'assortment'
group_by(RegionalVarejista, cluster_H) %>%
  summarise(
    mean = mean(sortimento, na.rm = TRUE),
    sd = sd(sortimento, na.rm = TRUE),
    min = min(sortimento, na.rm = TRUE),
    max = max(sortimento, na.rm = TRUE))

#'organization'
group_by(RegionalVarejista, cluster_H) %>%
  summarise(
    mean = mean(organização, na.rm = TRUE),
    sd = sd(organização, na.rm = TRUE),
    min = min(organização, na.rm = TRUE),
    max = max(organização, na.rm = TRUE))

# ANOVA FOR EACH VARIABLE 

#service
summary(anova_atendimento <- aov(formula = atendimento ~ cluster_H,
                                 data = RegionalVarejista))

# 'assortment'
summary(anova_atendimento <- aov(formula = sortimento ~ cluster_H,
                                 data = RegionalVarejista))


#'organization'
summary(anova_organiza <- aov(formula = organização ~ cluster_H,
                              data = RegionalVarejista))

#Now, we use the Manhattan distance
matriz_DM <- RegionalVarejista %>% 
  select(atendimento, sortimento, organização) %>% 
  dist(method = "manhattan")

cluster_hier_man <- agnes(x = matriz_DM, method = "complete")
dev.off()
fviz_dend(x = cluster_hier_man)

#3 clusters
fviz_dend(x = cluster_hier_man,
          k = 3,
          k_colors = c("deeppink4", "darkviolet", "darkblue"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

RegionalVarejista$cluster_H_man <- factor(cutree(tree = cluster_hier_man, k = 3))

#----------------------------K-MEANS METHOD ----------------------

cluster_kmeans <- kmeans(RegionalVarejista[,3:5],
                         centers=3
                         )
#creating a categorical variable in the database
RegionalVarejista$cluster_K <- factor(cluster_kmeans$cluster)

#Elbow method -> optimal number of clusters
fviz_nbclust(RegionalVarejista[,3:5], kmeans, method = "wss", k.max = 10)

RegionalVarejista %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 14)
# ANOVA

# Análise de variância de um fator (ANOVA)

#service
summary(anova_atendimento <- aov(formula = atendimento ~ cluster_K,
                                 data = RegionalVarejista))

#Sortment
summary(anova_sortimento <- aov(formula = sortimento ~ cluster_K,
                                data = RegionalVarejista))

#organization
summary(anova_organiza <- aov(formula = organização ~ cluster_K,
                              data = RegionalVarejista))

RegionalVarejista %>%
  select(regional, cluster_H, cluster_K) %>%
  arrange(regional) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)
