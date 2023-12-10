# PCA

# Course: MBA DSA (USP ESALQ)
# Prof. Wilson Tarantin Jr.

#Load packages

set_packages <-  c("tidyverse","ggrepel","reshape2","knitr","kableExtra", 
                   "PerformanceAnalytics","factoextra","psych","sp","tmap")

if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  instalador <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}

load(file="atlasambiental.RData")
#Database environmental atlas
atlasambiental %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)
summary(atlasambiental[,3:11])

rho <- cor(atlasambiental[,3:11])

atlasambiental[,3:11] %>% 
  cor() %>% 
  melt() %>% 
  rename(Correlation = value) %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_text(aes(x = Var1, y = Var2, label = format(Correlation, digits = 2)),
            size = 3) +
  scale_fill_gradient2(low = "darkblue", 
                       mid = "white", 
                       high = "brown",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

cortest.bartlett(atlasambiental[,3:11])

factorial <- principal(atlasambiental[,3:11],
                       nfactors = length(atlasambiental[,3:11]),
                       rotate = "none",
                       scores = TRUE
                       )
eigenvalues <- round(factorial$values,5)
sum(eigenvalues)

# eigenvalues > 1
k <- sum(eigenvalues>1)
print(k)

factorial <- principal(atlasambiental[,3:11],
                       nfactors = k,
                       rotate = "none",
                       scores = TRUE
)

shared_var <- as.data.frame(factorial$Vaccounted) %>% 
  slice(1:3)
rownames(shared_var) <- c("Eigenvalues",
                                       "Var Prop.",
                                       "Cum_var Prop.")
shared_var %>%
  slice(2) %>% 
  melt() %>% 
  ggplot(aes(x = variable, 
             y = value)) + 
  geom_col(fill = "deeppink", color = "black") +
  geom_text(aes(label = paste0(round(value*100, 2),"%") , vjust = -0.1))+
  labs(x = "Factor",
       y = "Shared Variance") +
  theme_bw()

loadings <- as.data.frame(unclass(factorial$loadings))
loadings %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 14)

communalities<- as.data.frame(unclass(factorial$communality))

communalities %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 14)

loadings %>%
  mutate(Communalities = rowSums(loadings ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
loadings %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(color = "darkblue") +
  geom_hline(yintercept = 0, color = "deeppink") +
  geom_vline(xintercept = 0, color = "deeppink") +
  geom_text_repel(label = row.names(loadings)) +
  theme_bw()

weights_ <- as.data.frame(factorial$weights)
weights_ %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

factors <- as.data.frame(factorial$scores)

atlasambiental <- bind_cols(atlasambiental,
                            "factor_1" = factors$PC1, 
                            "factor_2" = factors$PC2)

atlasambiental[,c(2, 12, 13)] %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

atlasambiental <- atlasambiental %>% 
  mutate(punctuation = factor_1 * shared_var$PC1[2])

atlasambiental[,c(2, 14)] %>%
  arrange(desc(punctuation)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Spatial visualization
load(file="mapa_sp.RData")
tm_shape(mapa_sp)+ tm_borders()

mapa_sp@data$COD_DIST <- as.numeric(mapa_sp@data$COD_DIST)

data_district <- merge(mapa_sp,
                         atlasambiental,
                         by.x = "COD_DIST",
                         by.y = "cod_ibge")


tmap_mode("view")  

tm_shape(data_district) +
  tm_fill("punctuation", midpoint = 0, palette = "RdBu", 
          style = "quantile", n = 10, legend.show = T) +
  tm_borders(alpha = 0.8) +
  tm_text("distrits")