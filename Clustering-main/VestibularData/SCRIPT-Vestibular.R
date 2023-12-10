## Análise de Clusters
## Reference: Fávero e Belfiore, MANUAL DE ANÁLISE DE DADOS, Capítulo 09

# Curso: MBA DSA USP ESALQ

# Prof. Wilson Tarantin Jr.

#Isabela Pereira Lima Dias


#Packages

packages_set <-c("car",
                 "plotly", #graphs
                 "tidyverse", #load R packages
                 "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
                 #evitar sobreposição de textos
                 "knitr", "kableExtra", #format tables
                 "reshape2", # 'melt' function
                 "misc3d", # 3D graphs
                 "plot3D", # 3D graphs
                 "cluster", #hierarchical clusters
                 "factoextra", #função 'fviz_dend'  dendogram
                 "ade4") # distance matrices
if(sum(as.numeric(!packages_set %in% installed.packages())) != 0){
  instalador <- packages_set[!packages_set %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(packages_set, require, character = T) 
} else {
  sapply(packages_set, require, character = T) 
}

#load database
load(file = "Vestibular.RData")

#visualization

Vestibular %>%
  kable()  %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE,font_size = 18)

#3D PLOT 
# set row names as student names
rownames(Vestibular) <-Vestibular$estudante
#set axes as subjects (columns)
scatter3D(x=Vestibular$fisica, 
          y =Vestibular$matematica,
          z= Vestibular$quimica,
          phi = 0, bty = "g", pch = 20, cex = 2,
          xlab = "Fisica",
          ylab = "Matematica",
          zlab = "Quimica",
          main = "Vestibular",
          clab = "Nota de Quimica")>
  text3D(x=Vestibular$fisica, 
         y =Vestibular$matematica,
         z= Vestibular$quimica,
         labels = rownames(Vestibular),
         add = TRUE, cex=1 )

#DESCRIPTIVE STATISTICS
summary(Vestibular)

#if needed, Zscore (standard) function is used for standardizing scores on the same scale 
vest_padronizado <- as.data.frame(scale(Vestibular[,2:4]))
rownames(vest_padronizado) <- Vestibular$estudante

#Boxplots for each variable
ggplotly(
  Vestibular %>%
    melt() %>%
    ggplot(aes(label = estudante)) +
    geom_boxplot(aes(x = variable, y = value, fill = variable)) +
    geom_point(aes(x = variable, y = value), alpha = 0.5) +
    labs(x = "Variável",
         y = "Nota") +
    scale_fill_manual("Legenda:",
                      values = c("orange", "purple", "bisque4")) +
    theme_bw()
)


#------------------now, clustering methods ---------------------#

#-------- AGGLOMERATIVE HIERARCHICAL METHOD -------------------#

#dissimilarity matrix 
#PARAMETRIZATION
## "euclidean"
## "sqrt euclidean"
## "maximum"
## "manhattan"
## "canberra"
## "minkowski"

matriz_D <- Vestibular %>%
  select(matematica,fisica,quimica) %>%
  #distance matrix
  dist(method="euclidean")

#visualization

data.matrix(matriz_D) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

#Hierarchical clustering
## "complete": furthest neighbor or complete linkage
## "single": nearest neighbor or single linkage
## "average": between groups or average linkage

cluster_hier <- agnes(x=matriz_D,method = "single")

#distances of each stage
coeficientes <- sort(cluster_hier$height,decreasing=FALSE)

# Output interpretation:

## The rows are the clustering steps
## In the columns Cluster1 e Cluster2, how occurs the grouping
## Negative number-> Isolated observation 
## Positive number -> cluster grouped on the previous step 
## Coefficients: Distances

esquema <- as.data.frame(cbind(cluster_hier$merge,coeficientes))
names(esquema) <-c("Cluster1","Cluster2", "Coeficientes")
esquema

esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

#Dendogram
dev.off()
fviz_dend(x=cluster_hier)

#Cluster visualization
fviz_dend(x=cluster_hier,
          k=3,
          k_colors=c("deeppink3","darkviolet", "darkblue"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

#Categorical variables in clustering, where k is the number of clusters
Vestibular$cluster_H <- factor(cutree(tree=cluster_hier,k=3))
# Visualizing 
Vestibular %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 14)
#descriptive statistics for each variable
#variable matematica
group_by(Vestibular,cluster_H) %>%
  summarise(mean=mean(matematica, na.rm = TRUE),
            sd = sd(matematica, na.rm = TRUE),
            min = min(matematica, na.rm = TRUE),
            max = max(matematica, na.rm = TRUE))
#variable fisica
group_by(Vestibular,cluster_H) %>%
  summarise(mean=mean(fisica, na.rm = TRUE),
            sd = sd(fisica, na.rm = TRUE),
            min = min(fisica, na.rm = TRUE),
            max = max(fisica, na.rm = TRUE))
#variable quimica

group_by(Vestibular,cluster_H) %>%
  summarise(mean=mean(quimica, na.rm = TRUE),
            sd = sd(quimica, na.rm = TRUE),
            min = min(quimica, na.rm = TRUE),
            max = max(quimica, na.rm = TRUE))
# ANALYSIS OF VARIANCE (ANOVA).OUTPUT:

## Mean Sq: amount of variance between the groups
## Residual Mean Sq : amount of variance inside the groups
## F value:  (Sum Sq cluster_H / Sum Sq  Residuals)
## Pr(>F): statistical p-value 
## p-value < 0.05: statistical average differs from the other

## discriminant (> F value )
#anova (matematica)
summary(anova_matematica <- aov(formula=matematica ~ cluster_H, 
                                data= Vestibular))
#anova (fisica)
summary(anova_fisica <- aov(formula=fisica ~ cluster_H, 
                                data= Vestibular))
#anova (quimica)
summary(anova_quimica <- aov(formula=quimica ~ cluster_H, 
                                data= Vestibular))
# -------------------K-MEANS --------------------

cluster_kmeans <- kmeans(Vestibular[,2:4],
                         centers=3)

fviz_nbclust(Vestibular[,2:4],kmeans, method="wss",k.max=4)


Vestibular$cluster_K <-factor(cluster_kmeans$cluster)

Vestibular %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 14)

#ANOVA

# ANOVA 'matematica'
summary(anova_matematica <- aov(formula = matematica ~ cluster_K,
                                data = Vestibular))

# ANOVA'fisica'
summary(anova_fisica <- aov(formula = fisica ~ cluster_K,
                            data = Vestibular))

# ANOVA 'quimica'
summary(anova_quimica <- aov(formula = quimica ~ cluster_K,
                             data = Vestibular))

#  hierarchical vs kmeans
Vestibular %>%
  select(estudante, cluster_H, cluster_K) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)










  






          





