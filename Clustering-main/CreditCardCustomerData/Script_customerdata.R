# Curso: MBA DSA USP ESALQ

# Prof. Wilson Tarantin Jr.

#Isabela Pereira Lima Dias 

#Goal: Categorize credit card customers 

#Source:  https://www.kaggle.com/datasets/aryashah2k/credit-card-customer-data

set_packages <- c("plotly", 
                  "tidyverse", 
                  "ggrepel", 
                  "knitr", 
                  "kableExtra",
                  "reshape2",
                  "misc3d",
                  "plot3D", 
                  "cluster", 
                  "factoextra")

if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  instalador <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}

user_data <- read.csv("creditcard.csv")

standard_data <- as.data.frame(scale(user_data[,3:7]))

scatter3D(x=standard_data$Avg_Credit_Limit,
          y=standard_data$Total_Credit_Cards,
          z=standard_data$Total_visits_bank,
          phi = 1, bty = "g", pch = 20, cex = 1,
          xlab = "credit card limit",
          ylab = "Card N",
          zlab = "Total visits",
          main = "customers", 
          colkey = F)

dev.off()
fviz_nbclust(standard_data, kmeans, method = "wss", k.max = 10)
#----------------------------K-MEANS -----------------------------------
cluster_kmeans <- kmeans(standard_data,
                         centers = 4)

standard_data$cluster_K <- factor(cluster_kmeans$cluster)
user_data$cluster_K <- factor(cluster_kmeans$cluster)

# Graphs

ggplot(standard_data) +
  geom_point(aes(x = Avg_Credit_Limit, 
                 y = Total_Credit_Cards, 
                 color = cluster_K)) + 
  labs(x = "Average Limit",
       y = "Total Credit Cards")


ggplot(standard_data) +
  geom_point(aes(x = Avg_Credit_Limit, 
                 y = Total_visits_bank, 
                 color = cluster_K)) + 
  labs(x = "Average Limit",
       y = "Total Visits bank")

ggplot(standard_data) +
  geom_point(aes(x = Avg_Credit_Limit, 
                 y = Total_visits_online, 
                 color = cluster_K)) + 
  labs(x = "Average Limit",
       y = "Total Visits online")


# Descriptive statistics

analysis <- group_by(user_data, cluster_K) %>%
  summarise(limit = mean(Avg_Credit_Limit, na.rm = TRUE),
            total_cr = mean(Total_Credit_Cards, na.rm = TRUE),
            total_vb = mean(Total_visits_bank, na.rm = TRUE),
            total_vo = mean(Total_visits_online, na.rm = TRUE),
            total_ca = mean(Total_calls_made, na.rm = TRUE))

# Anovas

summary(anova_limit <- aov(formula = Avg_Credit_Limit ~ cluster_K,
                            data = standard_data))

summary(anova_cards <- aov(formula = Total_Credit_Cards ~ cluster_K,
                             data = standard_data))

summary(anova_visits <- aov(formula = Total_visits_bank ~ cluster_K,
                             data = standard_data))

summary(anova_online <- aov(formula = Total_visits_online ~ cluster_K,
                            data = standard_data))

summary(anova_calls <- aov(formula = Total_calls_made ~ cluster_K,
                          data =standard_data))

