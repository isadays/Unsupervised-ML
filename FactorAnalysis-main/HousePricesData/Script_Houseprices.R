# PCA

# Curso: MBA DSA (USP ESALQ)
#Source: https://www.kaggle.com/datasets/elakiricoder/jiffs-house-price-prediction-dataset
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

#Load dataset
houses <- read_excel("house_prices.xlsx")
#Descriptive statistics
summary(houses)

#Pearson correlations
rho <-rcorr(as.matrix(houses[,1:8]),type="pearson")

corr_coeff <-rho$r # correlation matrix
corr_sig <- round(rho$P,5) #matrix with p-value

#Heatmap of Pearson correlations
ggplotly(
  houses[,1:8] %>%
    cor() %>%
    melt() %>%
    rename(Correlation = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_text(aes(x = Var1, y = Var2, label = format(round(Correlation,3))),
              size = 3) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 6))

# Barletts test of sphericity
cortest.bartlett(houses[,1:8])

#  PCA
factorial <- principal(houses[,1:8],
                       nfactors = length(houses[,1:8]),
                       rotate = "none",
                       scores = TRUE)
factorial

# Eigenvalues 
eigenvalues <-round(factorial$values,5)
eigenvalues
#Shared variance in each factor
shared_variance <- as.data.frame(factorial$Vaccounted) %>% slice(1:3)
rownames(shared_variance) <- c("Eigenvalues",
                               "Variance Prop.",
                               "Cumul_var Prop.")
round(shared_variance, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)

#Restricting our analysis for eigenvalues>1
k <- sum(eigenvalues>1)
print(k)

# k is the number of factors in our analysis

factorial2 <- principal(houses[,1:8],
                        nfactors = k,
                        rotate = "none",
                        scores=TRUE)
factorial2

#Weights
fact_scores <- as.data.frame(factorial2$weights)
round(fact_scores,3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)

#Factor related to the scores

factors <- as.data.frame(factorial2$scores)
View(factors)


# Factor loadings

loadings <- as.data.frame(unclass(factorial2$loadings))
round(loadings,3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
# Communalities

communalities <- as.data.frame(unclass(factorial2$communality))  %>%
  rename(communalities=1)

round(communalities,3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
# Loading plot -> two first factors
loadings[, 1:2] %>%
  data.frame() %>%
  rownames_to_column("variables") %>%
  ggplot(aes(x = PC1, y = PC2, label = variables)) +
  geom_point(color = "darkorchid",
             size = 3) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "darkblue") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "darkblue") +
  expand_limits(x= c(-1.25, 0.25), y=c(-0.25, 1)) +
  theme_bw()
#Loading plot -> 1st and 3rd factors
loadings[, 1:3] %>%
  data.frame() %>%
  rownames_to_column("variables") %>%
  ggplot(aes(x = PC1, y = PC3, label = variables)) +
  geom_point(color = "darkorchid",
             size = 3) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "darkblue") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "darkblue") +
  expand_limits(x= c(-1.25, 0.25), y=c(-0.25, 1)) +
  theme_bw()

#Ranking 
houses$ranking <- factors$PC1 * shared_variance$PC1[2] +
  factors$PC2 * shared_variance$PC2[2] +
  factors$PC3 * shared_variance$PC3[2] 

# Checking if our ranking agrees with the house prices
corr_values <- rcorr(as.matrix(houses[,9:10]))

value_corr_coeff <- corr_values$r
value_corr_sig <- round(corr_values$P,5)














































































