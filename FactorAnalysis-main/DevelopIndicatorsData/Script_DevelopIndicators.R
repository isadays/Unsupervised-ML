#PCA
# Source:  Fávero and Belfiore, MANUAL DE ANÁLISE DE DADOS, Chapter 10
# Curso: MBA DSA (USP ESALQ)
# Prof. Wilson Tarantin Jr.
#Isabela Pereira Lima Dias

#Load packages
set_packages <-c("plotly",
                 "tidyverse",
                 "ggrepel",
                 "knitr",
                 "kableExtra",
                 "reshape2",
                 "PerformanceAnalytics",
                 "psych",
                 "ltm",
                 "Hmisc",
                 "readxl")
if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  instalador <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}

data_indicator <- read_xlsx("country_indicators.xlsx")

data_indicator %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 14)

summary(data_indicator)

data_indicator %>%
  ggplot() +
  geom_point(aes(x = escol1, y = pib_capita1),
             color = "deeppink",
             size = 3) +
  geom_smooth(aes(x = escol1, y = pib_capita1),
              color = "darkblue", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "Scholarity",
       y = "GPD") +
  theme_bw()

data_indicator %>%
  ggplot() +
  geom_point(aes(x = escol1, y = violência1),
             color = "deeppink",
             size = 3) +
  geom_smooth(aes(x = escol1, y = violência1),
              color = "darkblue", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "Scholarity",
       y = "Violence") +
  theme_bw()

# PCA FOR THE FIRST YEAR

# PEARSON CORRELATIONS
rho <- rcorr(as.matrix(data_indicator[,2:5]),type = "pearson")

corr_coeff <- rho$r #correlation matrix
corr_sig <- round(rho$P,5) # p-value 

#Heatmap

ggplotly(
  data_indicator[,2:5] %>%
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

chart.Correlation(data_indicator[, 2:5], histogram = TRUE, pch = "+")

# Barletts test of sphericity
cortest.bartlett(data_indicator[,2:5])

factorial <- principal(data_indicator[, 2:5],
                      nfactors = length(data_indicator[, 2:5]),
                      rotate = "none",
                      scores = TRUE)

factorial 
# eigenvalues
eigenvalues <- round(factorial$values, 5)
eigenvalues
round(sum(eigenvalues), 2)

#shared variance
shared_var <- as.data.frame(factorial$Vaccounted) %>% 
  slice(1:3)

rownames(shared_var) <- c("Eigenvalues",
                                       "Variance Prop.",
                                       "Cumul_var. Prop.")
round(shared_var, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)

#weigths
fact_weigths <- as.data.frame(factorial$weights)
round(fact_weigths, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
fact_scores <- as.data.frame(factorial$scores)

View(fact_scores)

rho <- rcorr(as.matrix(fact_scores), type="pearson")
round(rho$r, 4)

loadings <- as.data.frame(unclass(factorial$loadings))
round(loadings, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
communalities <- as.data.frame(unclass(factorial$communality)) %>%
  rename(comunalities=1)


round(communalities, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)
# Analysis for the first year - 

#eigenvalues
k <- sum(eigenvalues>1)
print(k)

factorial1 <- principal(data_indicator[, 2:5],
                       nfactors = k,
                       rotate = "none",
                       scores = TRUE)
factorial1

# Loadings
loadings1 <- as.data.frame(unclass(factorial1$loadings))

# Visualization of loadings
round(loadings1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

# Communalities ('k' = 1)
communalities1 <- as.data.frame(unclass(factorial1$communality)) %>%
  rename(communalities = 1)

# Visualizing communalities
round(communalities1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

# weights
weight_sc1 <- as.data.frame(factorial1$weights)

# Visualizing weights
round(weight_sc1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)

# factor for the first year
fact1 <- as.data.frame(factorial1$scores)

# Adding the factor in the dataset
data_indicator <- bind_cols(data_indicator,
                            "fator_ano1" = fact1$PC1)

# The same for the second year. 

cortest.bartlett(data_indicator[,6:9])

factorial2 <- principal(data_indicator[, 6:9],
                        nfactors = length(data_indicator[, 6:9]),
                        rotate = "none",
                        scores = TRUE)
#eigenvalues
eigenvalues2 <- round(factorial2$values, 5)
print(eigenvalues2)

k2 <- sum(eigenvalues2>1)
print(k2)

factorial2 <- principal(data_indicator[, 6:9],
                        nfactors = k2,
                        rotate = "none",
                        scores = TRUE)
factorial2

# Loadings
loadings2 <- as.data.frame(unclass(factorial2$loadings))

# Visualization of loadings
round(loadings2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

# Communalities ('k' = 1)
communalities2 <- as.data.frame(unclass(factorial2$communality)) %>%
  rename(communalities = 1)

# Visualizing communalities
round(communalities2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

# weights
weight_sc2 <- as.data.frame(factorial2$weights)

# Visualizing weights
round(weight_sc2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)

# factor for the first year
fact2 <- as.data.frame(factorial2$scores)

# Adding the factor in the dataset
data_indicator <- bind_cols(data_indicator,
                            "fator_ano1" = fact2$PC1)
# Comparing weights
score_comp <- round(cbind(weight_sc1, weight_sc2),3) %>% 
  rename(Fator_Ano1 = 1, Fator_Ano2 = 2)
rownames(score_comp) <- c("cpi",
                             "Violence",
                             "GDP",
                             "Scholarity")

# Comparing loadings
load_comp <- round(cbind(loadings1, loadings2),3) %>% 
  rename(Fator_Ano1 = 1, Fator_Ano2 = 2)
rownames(load_comp) <- c("cpi",
                              "Violence",
                              "GDP",
                              "Scholarity")


# Comparing data
fator_ano1

data_indicator <- data_indicator %>% 
  arrange(desc(fact1)) %>% 
  mutate(pos_ano1 = seq(fact1)) %>% 
  arrange(desc(fact2)) %>% 
  mutate(pos_ano2 = seq(fact2))





































































