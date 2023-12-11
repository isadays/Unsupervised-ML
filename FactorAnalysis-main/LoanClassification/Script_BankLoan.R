# Source https://www.kaggle.com/datasets/itsmesunil/bank-loan-modelling
# Bank loan modelling

#Packages
packages <- c("plotly", 
             "tidyverse",
             "ggrepel", 
             "knitr", "kableExtra", 
             "reshape2", 
             "PerformanceAnalytics",
             "psych",
             "ltm", 
             "Hmisc", 
             "readxl") 

if(sum(as.numeric(!packages %in% installed.packages())) != 0){
  install_packages <- packages[!packages %in% installed.packages()]
  for(i in 1:length(install_packages)) {
    install.packages(install_packages, dependencies = T)
    break()}
  sapply(packages, require, character = T) 
} else {
  sapply(packages, require, character = T) 
}

loan_indicators <- read_excel("loan_indicators.xlsx")
#Descriptive statistics
summary(loan_indicators[,2:7])
#Pearsons correlations
rho <- rcorr(as.matrix(loan_indicators[,2:7]),type= "pearson")

#Correlation Matrix
corr_coeff <-rho$r
corr_sig <-round(rho$P,5) #P-value

ggplotly(
  loan_indicators[,2:7] %>%
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

chart.Correlation(loan_indicators[,2:7],histogram=TRUE,pch="+")

#Bartletts test of sphericity
cortest.bartlett(loan_indicators[,2:7])

# PCA
factorial <- principal(loan_indicators[,2:7],
                       nfactors = length(loan_indicators[,2:7]),
                       rotate="none",
                       scores = TRUE)
factorial
#eigenvalues
eigenvalues <- round(factorial$values,5)
eigenvalues

round(sum(eigenvalues),2)

# Shared variance or variance accounted
shared_variance <- as.data.frame(factorial$Vaccounted) %>% slice(1:3)
rownames(shared_variance) <- c("Eigenvalues", "Variance Prop.", "Cum. Variance Prop")

round(shared_variance, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)

#--------------------------PCA EIGENVALUES>1
k <- sum(eigenvalues>1)
print(k)
factorial2 <- principal( loan_indicators[,2:7],nfactors = k, rotate="none", scores = TRUE)
factorial2

# Factor scores
factor_scores <- as.data.frame(factorial2$weights)
# Visualization
round(factor_scores, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
factors <- as.data.frame(factorial2$scores)
View(factors)
# Pearson 
rho <- rcorr(as.matrix(factors),type="pearson")
round(rho$r,4)

factor_loadings <- as.data.frame(unclass(factorial2$loadings))
round(factor_loadings, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)
# Communalities

communalities <- as.data.frame(unclass(factorial2$communality)) %>%
  rename(communalities = 1)
round(communalities, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 14)


# Loading plot 
factor_loadings[, 1:2] %>%
  data.frame() %>%
  rownames_to_column("Variables") %>%
  ggplot(aes(x = PC1, y = PC2, label = Variables)) +
  geom_point(color = "deeppink",
             size = 3) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "lightblue") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "lightblue") +
  expand_limits(x= c(-1.25, 0.25), y=c(-0.25, 1)) +
  theme_bw()
