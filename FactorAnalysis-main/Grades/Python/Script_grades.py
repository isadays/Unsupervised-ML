# -*- coding: utf-8 -*-
"""Script_Grades.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1Tf0eAVjjur6wQBD6W3xuyxiXTtNQc42R
"""

#requirements
!pip install pandas
!pip install factor_analyzer
!pip install pingouin
!pip install seaborn
!pip install numpy
!pip install matplotlib
!pip install scipy
!pip install factor_analyzer
!pip install pingouin

#Libraries
import pandas as pd
from factor_analyzer import FactorAnalyzer
from factor_analyzer.factor_analyzer import calculate_bartlett_sphericity
import pingouin as pg
import seaborn as sns
import numpy as np
import matplotlib.pyplot as plt

# Dataset
grades = pd.read_excel("Factor_Grades.xlsx")

print(grades)

# Some basic informations
print(grades.info())
print(grades.describe())

#Quantitative variables (useful) to our analysis
grades_pca = grades[["finanças","custos", "marketing", "atuária"]]
print(grades_pca)

# Correlation matrix
matrix_corr = pg.rcorr(grades_pca,method="pearson",
                       upper = "pval", decimals=4, pval_stars={0.01:"***", 0.05:"**", 0.10: "*"})
print(matrix_corr)

#Correlation matrix
corr = grades_pca.corr()
f,ax = plt.subplots(figsize=(11,9))
mask = np.triu(np.ones_like(corr,dtype=bool))
cmap = sns.diverging_palette(230,20,n=256, as_cmap=True)
sns.heatmap(grades.corr(),mask=mask, vmax=1, vmin=-.25,center=0,
            square=True, linewidths=.5,annot=True, fmt=".3f",
            annot_kws={"size":16},
            cbar_kws = {"shrink":.75})
plt.title("Correlation Matrix")
plt.tight_layout()
ax.tick_params(axis="x",labelsize=14)
ax.tick_params(axis="y",labelsize=14)
ax.set_ylim(len(corr))
plt.show()

#Bartlett test of sphericity
bartlett, p_value = calculate_bartlett_sphericity(grades_pca)
print(f'Bartlett statistic: {bartlett}')

print(f'p-value : {p_value}')

#PCA
fa = FactorAnalyzer()
fa.fit(grades_pca)

#Eigenvalues
eigen, v = fa.get_eigenvalues()
print(eigen)

# Kaiser criterium
fa.set_params(n_factors=2, method="principal", rotation = None)
fa.fit(grades_pca)

#Eigenvalues, variances and cumulative variance
eigen_factors = fa.get_factor_variance()
eigen_factors

table_eigen = pd.DataFrame(eigen_factors)
table_eigen.columns = [f"Factor {i+1}" for i, v in enumerate(table_eigen.columns)]
table_eigen.index = ['Eigenvalue','Variance', 'Cum. Variance']
table_eigen = table_eigen.T
print(table_eigen)

# Loadings
loadings = fa.loadings_

table_loadings = pd.DataFrame(loadings)
table_loadings.columns = [f"Factor {i+1}" for i, v in enumerate(table_loadings.columns)]
table_loadings.index = grades_pca.columns
table_loadings
print(table_loadings)

communalities = fa.get_communalities()

table_communalities = pd.DataFrame(communalities)
table_communalities.columns = ['Communalities']
table_communalities.index = grades_pca.columns
table_communalities

print(table_communalities)

# Results
predict_factors= pd.DataFrame(fa.transform(grades_pca))
predict_factors.columns =  [f"Factor {i+1}" for i, v in enumerate(predict_factors.columns)]

print(predict_factors)

grades = pd.concat([grades.reset_index(drop=True), predict_factors], axis=1)
grades

scores = fa.weights_
table_scores = pd.DataFrame(scores)
table_scores.columns = [f"Factor {i+1}" for i, v in enumerate(table_scores.columns)]
table_scores.index = grades_pca.columns
table_scores

corr_factor = pg.rcorr(grades[['Factor 1','Factor 2']], method = 'pearson', upper = 'pval', decimals = 4, pval_stars = {0.01: '***', 0.05: '**', 0.10: '*'})
print(corr_factor)

grades['Ranking'] = 0
for index, item in enumerate(list(table_eigen.index)):
  variance = table_eigen.loc[item]["Variance"]

  grades["Ranking"] = grades["Ranking"] + grades[table_eigen.index[index]]*variance
print(grades)

# Graph of loadings and variances
import matplotlib.pyplot as plt

plt.figure(figsize=(12,8))

table_loadings_chart = table_loadings.reset_index()

plt.scatter(table_loadings_chart['Factor 1'], table_loadings_chart['Factor 2'], s=30)

def label_point(x, y, val, ax):
    a = pd.concat({'x': x, 'y': y, 'val': val}, axis=1)
    for i, point in a.iterrows():
        ax.text(point['x'] + 0.05, point['y'], point['val'])

label_point(x = table_loadings_chart['Factor 1'],
            y = table_loadings_chart['Factor 2'],
            val = table_loadings_chart['index'],
            ax = plt.gca())

plt.axhline(y=0, color='black', ls='--')
plt.axvline(x=0, color='black', ls='--')
plt.ylim([-1.5,1.5])
plt.xlim([-1.5,1.5])
plt.title(f"{table_eigen.shape[0]} principal components that explain {round(table_eigen['Variance'].sum()*100,2)}% the variance", fontsize=14)
plt.xlabel(f"PC 1: {round(table_eigen.iloc[0]['Variance']*100,2)}% of variance explained", fontsize=14)
plt.ylabel(f"PC 2: {round(table_eigen.iloc[1]['Variance']*100,2)}% of variance explained", fontsize=14)
plt.show()

# Cummulative variance
plt.figure(figsize=(12,8))

plt.title(f"{table_eigen.shape[0]} principal components {round(table_eigen['Variance'].sum()*100,2)}% of variance", fontsize=14)
sns.barplot(x=table_eigen.index, y=table_eigen['Variance'], data=table_eigen, color='green')
plt.xlabel("Principal components", fontsize=14)
plt.ylabel("% explained variance", fontsize=14)
plt.show()