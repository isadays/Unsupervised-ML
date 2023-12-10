## Source: Fávero e Belfiore, MANUAL DE ANÁLISE DE DADOS, Capítulo 09

# Course: MBA DSA USP ESALQ

# Prof. Wilson Tarantin Jr. #Isabela Pereira Lima Dias

#Packages 

set_packages <- c("plotly",
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

#Load the dataset (.R)
load(file="BinarySearch.RData")

#Content: Answers for 50 questions, being 35 interviewees
#The interviewees are managers from 3 different sectors
View(PesquisaBinária)

#Counting categorical variables
vector_var <-names(PesquisaBinária)
map(PesquisaBinária[vector_var], ~ summary(as.factor(.)))

#---------------------- Agglomerative hierarchical clustering 

#Dissimilarity matrix
matrix_D <- PesquisaBinária %>%
  select(-setor)  %>%
  dist.binary(method=2)

# Method 2: Simple matching coefficient of Sokal & Michener (1958) . See Help material

#visualizing matrix_D 
#data.matrix (an optional data vector (including a list or expression vector).
#Non-atomic classed R objects are coerced by as.vector and all attributes discarded..)
# kable_styling This function provides a cleaner approach to modify the style of HTML tables
data.matrix(matrix_D) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)
 #average linkage 
cluster_hier <- agnes(x = matrix_D, method = "average")

#Distances for the joining in each step 
coeff <- sort(cluster_hier$height, decreasing = FALSE) 
coeff

schema <- as.data.frame(cbind(cluster_hier$merge, coeff))
names(schema) <- c("Cluster1", "Cluster2", "COEFFICIENTS")
schema

# Visualization
schema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)

#Dendrogram 

dev.off()
fviz_dend(x = cluster_hier)

#Dendrogram with 3 clusters
fviz_dend(x = cluster_hier,
          k = 3,
          k_colors = c("deeppink4", "darkviolet", "darkblue"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())
#The cluster number as categorical variable
PesquisaBinária$cluster_H <- factor(cutree(tree = cluster_hier,k=3))
#Now, the dataset visualization with the clusters

PesquisaBinária %>%
  select(setor, cluster_H) %>%
  arrange(setor) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 14)
