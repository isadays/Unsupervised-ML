#Multiple Correspondence Analysis
# Source: https://www.kaggle.com/code/jiagengchang/heart-disease-multiple-correspondence-analysis
# MBA DSA USP ESALQ
# Prof. Wilson Tarantin Jr.

#Age: age of the patient [years]
#Sex: sex of the patient [M: Male, F: Female]
#ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]
#RestingBP: resting blood pressure [mm Hg]
#Cholesterol: serum cholesterol [mm/dl]
#FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
#RestingECG: resting electrocardiogram results [Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria]
#MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]
#ExerciseAngina: exercise-induced angina [Y: Yes, N: No]
#Oldpeak: oldpeak = ST [Numeric value measured in depression]
#ST_Slope: the slope of the peak exercise ST segment [Up: upsloping, Flat: flat, Down: downsloping]
#HeartDisease: output class [1: heart disease, 0: Normal]


#Isabela Pereira Lima Dias

set_packages <-  c("plotly", 
                   "tidyverse", 
                   "ggrepel",
                   "knitr", "kableExtra", 
                   "sjPlot", 
                   "FactoMineR", 
                   "amap", 
                   "ade4",
                   "readxl")

if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  install_packages <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(install_packages)) {
    install.packages(install_packages, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}

data_heart <- read_excel("heart.xlsx")

View(data_heart)

data_heart <- data_heart %>% 
  mutate(age_cat = case_when(Age <= quantile(Age, 0.25, na.rm = T) ~ "younger ages",
                                 Age > quantile(Age, 0.25, na.rm = T) & Age <= quantile(Age, 0.75, na.rm = T) ~ "middle ages",
                                 Age > quantile(Age, 0.75, na.rm = T) ~ "older ages"))
data_heart <- data_heart %>% 
  mutate(restingBP_cat = case_when(RestingBP <= quantile(RestingBP, 0.25, na.rm = T) ~ "low RestingBP",
                                   RestingBP > quantile(RestingBP, 0.25, na.rm = T) & RestingBP <= quantile(RestingBP, 0.75, na.rm = T) ~ "medium RestingBP",
                                   RestingBP > quantile(RestingBP, 0.75, na.rm = T) ~ "high RestingBP"))


data_heart <- data_heart %>% 
  mutate(cholesterol_cat = case_when(Cholesterol <= quantile(Cholesterol, 0.25, na.rm = T) ~ "lower_cholesterol",
                                     Cholesterol > quantile(Cholesterol, 0.25, na.rm = T) & Cholesterol <= quantile(Cholesterol, 0.75, na.rm = T) ~ "medium_cholesterol",
                                  Cholesterol > quantile(Cholesterol, 0.75, na.rm = T) ~ "high_cholesterol"))

data_heart <- data_heart %>% 
  mutate(MaxHR_cat = case_when(MaxHR <= quantile(MaxHR, 0.25, na.rm = T) ~ "low_MaxHR",
                               MaxHR > quantile(MaxHR, 0.25, na.rm = T) & MaxHR <= quantile(MaxHR, 0.75, na.rm = T) ~ "medium_MaxHR",
                               MaxHR > quantile(MaxHR, 0.75, na.rm = T) ~ "high_MaxHR"))
# We created 4 variables and we can discard the previous (4 )

data_heart <- subset(data_heart, select = -c(1, 4,5,8))

#MCA - factors
data_heart <- as.data.frame(unclass(data_heart), stringsAsFactors=TRUE)

# Tables 
sjt.xtab(var.row = data_heart$HeartDisease,
         var.col = data_heart$Sex,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = data_heart$HeartDisease,
         var.col = data_heart$ChestPainType,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = data_heart$HeartDisease,
         var.col = data_heart$FastingBS,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = data_heart$HeartDisease,
         var.col = data_heart$RestingECG,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = data_heart$HeartDisease,
         var.col = data_heart$ExerciseAngina,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = data_heart$HeartDisease,
         var.col = data_heart$age_cat,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = data_heart$HeartDisease,
         var.col = data_heart$restingBP_cat,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = data_heart$HeartDisease,
         var.col = data_heart$cholesterol_cat,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = data_heart$HeartDisease,
         var.col = data_heart$MaxHR_cat,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")
#converting into FACTORS
data_heart[] <- lapply(data_heart, factor)

ACM <- dudi.acm(data_heart,scannf = FALSE)

expl_variance <-(ACM$eig / sum(ACM$eig)) * 100

n_categories <-  apply(data_heart,
                       MARGIN =  2,
                       FUN = function(x) nlevels(as.factor(x)))
#binary matrix

df_ACM <- data.frame(ACM$c1, Variable = rep(names(n_categories),
                                            n_categories))

#Perceptual map 
df_ACM %>%
  rownames_to_column() %>%
  rename(Category = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Category, color = Variable)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimension 1:", paste0(round(expl_variance[1], 2), "%")),
       y = paste("Dimension 2:", paste0(round(expl_variance[2], 2), "%"))) +
  theme_bw()

df_ACM_B <- data.frame(ACM$co, Variable = rep(names(n_categories),
                                              n_categories))

df_ACM_B %>%
  rownames_to_column() %>%
  rename(Category = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Category, color = Variable)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimension 1:", paste0(round(expl_variance[1], 2), "%")),
       y = paste("Dimension 2:", paste0(round(expl_variance[2], 2), "%"))) +
  theme_bw()


df_coord<- ACM$li


# 
df_coord %>%
  ggplot(aes(x = Axis1, y = Axis2, color = data_heart$HeartDisease)) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimension 1:", paste0(round(expl_variance[1], 2), "%")),
       y = paste("Dimension 2:", paste0(round(expl_variance[2], 2), "%")),
       color = "Heart Disease") +
  theme_bw()
