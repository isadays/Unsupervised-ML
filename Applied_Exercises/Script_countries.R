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


