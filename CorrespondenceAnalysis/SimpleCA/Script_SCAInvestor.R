# Simple and Multiple Correspondence Analysis
#Source Fávero e Belfiore, MANUAL DE ANÁLISE DE DADOS, Chapter 11
# MBA DSA USP ESALQ
# Prof. Wilson Tarantin Jr.
#Isabela Pereira Lima Dias

set_packages <- c("plotly", 
             "tidyverse",  
             "ggrepel", 
             "knitr", "kableExtra", 
             "sjPlot", #Contingency table
             "FactoMineR",  
             "amap", # 'matlogic' and 'burt'functions for binary matrix 
             "ade4") #function 'dudi.acm' 

if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  install_packages <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(install_packages)) {
    install.packages(install_packages, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}

# load dataset
load(file="InvestorProfile.RData",)

investor_profile <- perfil_investidor_aplicacao
#rm(perfil_investidor_aplicacao)

investor_profile %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 14)
summary(investor_profile)

colnames(investor_profile)[colnames(investor_profile) == "estudante"] <- "student"

colnames(investor_profile)[colnames(investor_profile) == "aplicacao"] <- "investment"

colnames(investor_profile)[colnames(investor_profile) == "perfil"] <- "profile"


# Simple Correspondence Analysis
contingency_table <-table(investor_profile$profile,
                          investor_profile$investment)
contingency_table

# number of observations on the contingency table, or entries
n<-sum(contingency_table) # 100 lines

#Chi squared test 
chi2 <- chisq.test(x=contingency_table)
chi2
#contingency table with absolute frequencies
sjt.xtab( var.row = investor_profile$profile,
          var.col = investor_profile$investment,
          show.exp = TRUE)
#Residual  = absolute frequency observed – expected absolute frequency
chi2$observed - chi2$expected

#the values χ2
((chi2$observed - chi2$expected)^2)/chi2$expected

#standardized residuals
chi2$residuals
#  adjusted standardized residuals
chi2$stdres
#Heatmap of adjusted standardized residuals

data.frame(chi2$stdres) %>%
  rename(profile = 1,
         investment = 2) %>% 
  ggplot(aes(x = fct_rev(profile), y = investment,
             fill = Freq, label = round(Freq, 3))) +
  geom_tile() +
  geom_text(size = 5) +
  scale_fill_gradient2(low = "white", 
                       mid ="lightpink3", 
                       high = "deeppink2",
                       midpoint = 1.96) +
  labs(x = 'Profile', y = 'Investiment', fill = "Adj. Stand. Residuals") +
  coord_flip() +
  theme_bw()


#Elaboration and interpretation of the perceptual map
matrixA <- chi2$residuals/sqrt(n)
matrixA

matrixW <-t(matrixA) %*% matrixA

#dimensions
number_dimensions <- min(nrow(matrixW)-1,ncol(matrixW)-1)

#singular values
sv_av <- svd(matrixA,nu=number_dimensions,nv = number_dimensions)

singular_values <- sv_av$d[1:number_dimensions]
singular_values

#Eigenvalues (for each  dimension)
eigenvalues <-(singular_values)^2
eigenvalues

# the total principal inertia of each dimension
total_inertia <- as.numeric(chi2$statistic/sum(contingency_table))

#The greater the total principal inertia (and the χ2), the stronger the association is
#between the variables in analysis  - Explained variance
explained_variance <- eigenvalues/total_inertia

#Determine the row and column masses
#The masses represent the influence that each category exercises on the other
#categories of their variable, whether in the column or row profiles.

# column profiles mass
column_sum <- apply(contingency_table,MARGIN = 1,FUN=sum)
column_mass <- column_sum/n
# row mass
row_sum <-  apply(contingency_table,MARGIN = 2,FUN=sum)
row_mass <- row_sum/n

# EIGENVECTORS V and U
eigenvector_v <- sv_av$v
eigenvector_u <- sv_av$u

# What we know ... "% of Total Principal Inertia"

data.frame(Dimension = paste("Dimension", 1:number_dimensions),
           `Singular Value` = singular_values,
           `Principal Partial Inertia eigenvalues` = eigenvalues) %>%
  mutate(`Percentual of Total Principal Inertia` = (`Principal.Partial.Inertia.eigenvalues`/total_inertia) * 100,
         `Percentual of Cum Total Principal Inertia` = cumsum(`Percentual of Total Principal Inertia`),
         Chi2 = chi2$statistic[[1]] * `Percentual of Total Principal Inertia` / n,
         `Singular Value` = `Singular.Value`,
         `Principal Partial Inertia eigenvalues` = Principal.Partial.Inertia.eigenvalues)

#  Determine the coordinates of the categories

# Profile 
#Coordinates of abscissas (X)
coord_abscissas_profile <- sqrt(singular_values[1])* (column_mass^-0.5)*eigenvector_u[,1]
  
#Coordinates of ordinates (Y)
coord_ordinates_profile <-sqrt(singular_values[2])* (column_mass^-0.5)*eigenvector_u[,2]


# Investiment s
#Coordinates of abscissas (X)
coord_abscissas_investiment <- sqrt(singular_values[1])* (column_mass^-0.5)*eigenvector_v[,1]

#Coordinates of ordinates (Y)
coord_ordinates_investiment <-sqrt(singular_values[2])* (column_mass^-0.5)*eigenvector_v[,2]


#perceptual map
cbind.data.frame(coord_abscissas_profile, coord_ordinates_profile,
                 coord_abscissas_investiment, coord_ordinates_investiment) %>%
  rename(dim_1_profile = 1,
         dim_2_profile = 2,
         dim_1_investiment = 3,
         dim_2_investiment = 4) %>%
  rownames_to_column() %>%
  setNames(make.names(names(.), unique = TRUE)) %>%
  mutate(investiment = rownames(data.frame(coord_abscissas_investiment,
                                         coord_ordinates_investiment))) %>%
  rename(profile = 1,
         dim_1_profile = 2,
         dim_2_profile = 3,
         dim_1_investiment = 4,
         dim_2_investiment = 5) %>%
  ggplot() +
  geom_point(aes(x = dim_1_profile, y = dim_2_profile),
             color = "deeppink1",
             fill = "deeppink1",
             shape = 24,
             size = 4) +
  geom_text_repel(aes(x = dim_1_profile, y = dim_2_profile, label = profile)) +
  geom_point(aes(x = dim_1_investiment, y = dim_2_investiment),
             color = "darkblue",
             fill = "darkblue",
             shape = 21,
             size = 4) +
  geom_text_repel(aes(x = dim_1_investiment, y = dim_2_investiment, label = investiment)) +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimension 1:", paste0(round(explained_variance[1] * 100, 2),"%")),
       y = paste("Dimension 2:", paste0(round(explained_variance[2] * 100, 2),"%"))) +
  theme_bw()

# SCA - factor map

sca <- CA(contingency_table,graph=TRUE)
