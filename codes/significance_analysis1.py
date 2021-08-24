import pandas as pd
from scipy.stats import mannwhitneyu

#Loading Dataset
df=pd.read_csv('../dataset/covid19_online_survey_data.csv')

#Segregating positive and negative samples
dfpos=df[df['Diagnosed']=='COVID']
dfneg=df[df['Diagnosed']=='Non-COVID']


#Storing Smell and taste test Features names Category-wise
smell=['Scented_detergents', 'Spices_Herbs1','Spices_Herbs2', 'Spice_Mixtures', 'Fruits_Vegetables', 'Dairy', 'Other', 'Nasal_irritant']
taste=['Sweet', 'Salty', 'Sour', 'Bitter', 'Taste_irritant']

#Display Statistical Significance Results


for i in range(len(smell)):
    print(smell[i])
    print(mannwhitneyu(list(dfpos[smell[i]].values), list(dfneg[smell[i]].values)))
    
for i in range(len(taste)):
    print(taste[i])
    print(mannwhitneyu(list(dfpos[taste[i]].values), list(dfneg[taste[i]].values)))


print(mannwhitneyu(list(dfpos.age.values), list(dfneg.age.values)))    