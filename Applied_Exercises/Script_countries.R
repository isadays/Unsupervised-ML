# Factor Analysis PCA + Clustering 

# Course: MBA DSA (USP ESALQ)

# Prof. Wilson Tarantin Jr.
# Isabela Pereira Lima Dias
# Source: https://www.kaggle.com/datasets/vipulgohel/clustering-pca-assignment?resource=download&select=Country-data.csv

set_packages <- c("plotly",
                    "tidyverse",
                    "ggrepel",
                    "knitr", "kableExtra",
                    "reshape2",
                    "PerformanceAnalytics",
                    "psych",
                    "Hmisc",
                    "readxl",
                    "cluster",
                    "factoextra")

if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  install_packages <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(install_packages)) {
    install.packages(install_packages, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}

countries <- read.csv("Countries.csv", sep = ",", dec = ".")

countries %>%
  ggplot() +
  geom_point(aes(x = income, y = life_expec),
             color = "deeppink2",
             size = 3) +
  geom_smooth(aes(x = income, y = life_expec),
              color = "darkblue", 
              method = "loess", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "income",
       y = "life_expec") +
  theme_bw()

countries %>%
  ggplot() +
  geom_point(aes(x = exports, y = gdpp),
             color = "deeppink2",
             size = 3) +
  geom_smooth(aes(x = exports, y = gdpp),
              color = "darkblue", 
              method = "loess", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "income",
       y = "life_expec") +
  theme_bw()

#Pearson correlation

rho <- rcorr(as.matrix(countries[,2:10]),type="pearson")
correl <- rho$r
sig_correl <- round(rho$P,4) #p value 


ggplotly(
  countries[,2:10] %>%
    cor() %>%
    melt() %>%
    rename(Correlation = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_text(aes(x = Var1, y = Var2, label = format(Correlation, digits = 1)),
              size = 5) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw())


chart.Correlation(countries[,2:10], histogram = TRUE, pch = "+")
#Barletts test of sphericity
cortest.bartlett(countries[,2:10])

factorial <- principal(countries[,2:10],
                       nfactors = length(countries[,2:10]),
                       rotate = "none",
                       scores=TRUE)

#Eigenvalues
eigenvalues <- round(factorial$values,5)
eigenvalues

round(sum(eigenvalues),2)
shared_variance <- as.data.frame(factorial$Vaccounted) %>% slice(1:3)
rownames(shared_variance) <- c("Eigenvalues",
                               "Variance Prop.",
                               "Cummul. Variance Prop")
round(shared_variance, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
weights_ <- as.data.frame(factorial$weights)

round(weights_, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)

factors_ <- as.data.frame(factorial$scores)

rho <- rcorr(as.matrix(factors_), type="pearson")
round(rho$r,4)

#loadings
loadings_ <- as.data.frame(unclass(factorial$loadings))

round(loadings_, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
#loss of variance
communalities <- as.data.frame(unclass(factorial$communality)) %>% rename(communalities=1)

round(communalities, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
k <- sum(eigenvalues>1)
print(k)

factorial2 <- principal(countries[,2:10],
                       nfactors = k,
                       rotate = "none",
                       scores=TRUE)
factorial2
communalities2 <- as.data.frame(unclass(factorial2$communality)) %>% rename(communalities=1)
#variance loss
round(communalities2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
loadings_[, 1:2] %>% 
  data.frame() %>%
  rownames_to_column("Variables") %>%
  ggplot(aes(x = PC1, y = PC2, label = Variables)) +
  geom_point(color = "darkorchid",
             size = 3) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "lightblue") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "lightblue") +
  expand_limits(x= c(-1.25, 0.25), y=c(-0.25, 1)) +
  theme_bw()

countries <- bind_cols(countries,
                    "factor_1" = factors_$PC1, 
                    "factor_2" = factors_$PC2,
                    "factor_3" = factors_$PC3)

#Clustering  average 0 and standard deviation 1
summary(countries[,11:13])
sd(countries[,11])
sd(countries[,12])
sd(countries[,13])

#Dissimilarity matrix
matrix_D <- countries[,11:13] %>% dist(method= "euclidean")
#complete linkage
cluster_hier <- agnes(x=matrix_D,method = "complete")

# Distances
coeff <-sort(cluster_hier$height,decreasing = FALSE)
coeff

schema <- as.data.frame(cbind(cluster_hier$merge,coeff))
names(schema) <-c("Cluster1","Cluster2", "Coefficients")

#Dendrogram
dev.off()
fviz_dend(x=cluster_hier,show_labels = FALSE)
#10 clusters
fviz_dend(x = cluster_hier,
          h = 3.0,
          show_labels = FALSE,
          color_labels_by_k = F,
          rect = F,
          rect_fill = F,
          ggtheme = theme_bw())
#categorical variable cluster_H
countries$cluster_H <- factor(cutree(tree = cluster_hier, k = 10))
#Fit an Analysis of Variance Model

# vanalys1
summary(vanalys1 <- aov(formula = factor_1 ~ cluster_H,
                             data = countries))

# vanalys2
summary(vanalys2 <- aov(formula = factor_2 ~ cluster_H,
                             data = countries))

# vanalys3
summary(vanalys3 <- aov(formula = factor_3 ~ cluster_H,
                             data = countries))


# gdpp
group_by(countries, cluster_H) %>%
  summarise(
    mean = mean(gdpp, na.rm = TRUE),
    sd = sd(gdpp, na.rm = TRUE),
    min = min(gdpp, na.rm = TRUE),
    max = max(gdpp, na.rm = TRUE),
    obs = n())

# health
group_by(countries, cluster_H) %>%
  summarise(
    mean = mean(health, na.rm = TRUE),
    sd = sd(health, na.rm = TRUE),
    min = min(health, na.rm = TRUE),
    max = max(health, na.rm = TRUE),
    obs = n())

# life_expec
group_by(countries, cluster_H) %>%
  summarise(
    mean = mean(life_expec, na.rm = TRUE),
    sd = sd(life_expec, na.rm = TRUE),
    min = min(life_expec, na.rm = TRUE),
    max = max(life_expec, na.rm = TRUE),
    obs = n())


