#Multiple Correspondence Analysis
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

load(file="InvestorProfileCS.RData")

investor_profile <- perfil_investidor_aplicacao_estadocivil
rm(perfil_investidor_aplicacao_estadocivil)


colnames(investor_profile)[colnames(investor_profile) == "estudante"] <- "student"

colnames(investor_profile)[colnames(investor_profile) == "aplicacao"] <- "investment"

colnames(investor_profile)[colnames(investor_profile) == "perfil"] <- "profile"

colnames(investor_profile)[colnames(investor_profile) == "estado_civil"] <- "civil_status"


investor_profile %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 14)

summary(investor_profile)

sjt.xtab(var.row = investor_profile$profile,
         var.col = investor_profile$investment,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

sjt.xtab(var.row = investor_profile$profile,
         var.col = investor_profile$civil_status,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
sjt.xtab(var.row = investor_profile$investment,
         var.col = investor_profile$civil_status,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)


#Binary matrix
binary_matrix <- matlogic(investor_profile[,2:4])
binary_matrix
# The Burt matrix is defined as: B = Z' . Z
# It is possible to combine the contingency tables with the crossing of all variable pairs in a single
#matrix
# When considering the Burt matrix as a contingency table, it is possible to perform an Anacor and
#obtain the coordinates of the categories

burt_matrix <- burt(investor_profile[,2:4])
burt_matrix
check_burt <- t(binary_matrix) %*% binary_matrix #OK 


#MCA - Multiple Correspondence Analysis

ACM <- dudi.acm(investor_profile[,2:4], scannf = FALSE)

# Visualization 
round(ACM$co,3) %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)
round(ACM$c1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)
#Mass
ACM$cw
#Principal Inertia
ACM$eig # 5 dimensions


# % of explained_variance by dimension
explained_var <- (ACM$eig/sum(ACM$eig))** 100

data.frame(Dimension = paste("Dimension", 1:length(explained_var)),
           Variance = explained_var) %>%
  ggplot(aes(x = Dimension,
             y = Variance,
             label = paste0(round(Variance, 2),"%"))) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(vjust = 2.5, size = 5) +
  theme_bw()

n_categ <- apply(investor_profile[,2:4],
                 MARGIN=2,
                 FUN=function(x) nlevels(as.factor(x)))

df_ACM <- data.frame(ACM$c1, Variable = rep(names(n_categ),
                                            n_categ))

df_ACM %>%
  rownames_to_column() %>%
  rename(Category = 1) %>%
  mutate(Category = gsub("profile","", Category),
         Category = gsub("investiment","", Category),
         Category = gsub("civil status","", Category)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)
# Perceptual map

df_ACM %>%
  rownames_to_column() %>%
  rename(Category = 1) %>%
  mutate(Category = gsub("profile","", Category),
         Category = gsub("investiment","", Category),
         Category = gsub("civil status","", Category)) %>%
  ggplot(aes(x = CS1, y = CS2, label = Category, color = Variable)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimension 1:", paste0(round(explained_var[1], 2), "%")),
       y = paste("Dimension 2:", paste0(round(explained_var[2], 2), "%"))) +
  scale_color_manual("Variable",
                     values = c("turquoise3", "springgreen4", "deeppink1")) +
  theme_bw()

# coordinates 
ACM_coord_df <- data.frame(ACM$li)

ACM_coord_df %>% 
  ggplot(aes(x = Axis1, y = Axis2, label = investor_profile$student)) +
  geom_point(shape = 17, color = "red", size = 2) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey48") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey48") +
  geom_text_repel(max.overlaps = 100, size = 3) +
  geom_density2d(color = "gray") +
  geom_label_repel(data = df_ACM, 
                   aes(x = CS1, y = CS2, 
                       label = rownames(df_ACM), 
                       fill = Variable), 
                   color = "white") +
  labs(x = paste("Dimension 1:", paste0(round(explained_var[1], 2), "%")),
       y = paste("Dimension 2:", paste0(round(explained_var[2], 2), "%"))) +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")
