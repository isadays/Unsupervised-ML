# Goal:
#To categorise the countries using socio-economic and health factors that 
#determine the overall development of the country.

#Source:https://www.kaggle.com/datasets/rohan0301/unsupervised-learning-on-country-data

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

#Importing dataset 
countries <-read.csv("datacountries.csv",sep=",", dec=".")
#Statistics summary
summary(countries)

# ---------------Exploratory data analysis---------------------

#Scaling (i.e., all variables in the same scale)

standardise_country <- as.data.frame(scale(countries[,2:10]))
# each line is a country 
rownames(standardise_country) <-countries$country
#Now, all variables have average 0 and standard deviation 1. 
#For instance, consider the variables exports and gdpp

round(mean(standardise_country$exports),3)
round(sd(standardise_country$exports))

round(mean(standardise_country$gdpp),3)
round(sd(standardise_country$gdpp))

#-------- AGGLOMERATIVE HIERARCHICAL METHOD -------------------

# dissimilarity matrix
matrix_D <-standardise_country %>%
  dist(method = "euclidean")
#-------------------------------------
#first test: agglomerative hierarchical method - single linkage
cluster_hier_single <- agnes(x=matrix_D,method = "single")

#dendrogram  
dev.off()
fviz_dend(x=cluster_hier_single,show_labels = F)

#Interpretation:as we can note, the above method does not provide a helpful clustering.
#Small distances between the observations 

#------------------------------------
#Second test: agglomerative hierarchical method: complete linkage
cluster_hier_complete <- agnes(x=matrix_D,method = "complete")

#dendrogram 
fviz_dend(x=cluster_hier_complete,show_labels = F)

#Interpretation: the results are better than the previous method(single linkage) but it is not optimal

#-------------------------------------------
#third test: agglomerative hierarchical method: average linkage
cluster_hier_average <- agnes(x = matrix_D, method = "average")

# Dendogram "average linkage"
fviz_dend(x = cluster_hier_average, show_labels = F)

#The complete linkage clustering method creates clusters with small observations.
#For this reason, we may create a dendrogrm with height = 5.5
fviz_dend(x = cluster_hier_complete,
          h = 5.5,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          rect_border = "black",
          lwd = 1,
          show_labels = F,
          ggtheme = theme_bw())
#The above code creates 12 clusters. Lets see this scheme in more details,

coeff <- sort(cluster_hier_complete$height,decreasing = F)
schema <- as.data.frame(cbind(cluster_hier_complete$merge,coeff))
names(schema) <-c("Cluster 1 ", "Cluster 2", "Coefficients")

#Generating a variable according to the 12 clusters
countries$cluster_H <- factor(cutree(tree=cluster_hier_complete,k=12))
standardise_country$cluster_H <-factor(cutree(tree = cluster_hier_complete,k=12))

# We can verify which variables contribute to the clusters formation,

summary(anova_child_mort <- aov(formula = child_mort ~ cluster_H,
                                data = standardise_country))

summary(anova_exports <- aov(formula = exports ~ cluster_H,
                             data = standardise_country))

summary(anova_health <- aov(formula = health ~ cluster_H,
                            data = standardise_country))

summary(anova_imports <- aov(formula = imports ~ cluster_H,
                             data = standardise_country))

summary(anova_income <- aov(formula = income ~ cluster_H,
                            data = standardise_country))

summary(anova_inflation <- aov(formula = inflation ~ cluster_H,
                               data = standardise_country))

summary(anova_lifeexpec <- aov(formula = life_expec ~ cluster_H,
                               data = standardise_country))

summary(anova_totalfer <- aov(formula = total_fer ~ cluster_H,
                              data = standardise_country))

summary(anova_gdpp <- aov(formula = gdpp ~ cluster_H,
                          data = standardise_country))

#Interpretatio: All variables help in the cluster formations

# Lets extract some information from the clusters:

analysis <- group_by(countries,cluster_H) %>%
  summarise(income=mean(income,na.rm = TRUE),
            gdpp = mean(gdpp,na.rm=TRUE),
            mort = mean(child_mort,na.rm=TRUE),
            health = mean(health,na.rm=TRUE),
            expec = mean(life_expec,na.rm=TRUE))
#Interpretation : countries from clusters 1 and 4 have 
#low average rent, low gdp per capita, high child mortality rate and low life expectancy.
