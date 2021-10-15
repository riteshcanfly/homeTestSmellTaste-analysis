import pandas as pd
from matplotlib import pyplot as plt
import seaborn as sns
from sklearn.svm import SVC
from sklearn.naive_bayes import GaussianNB
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV, StratifiedKFold
from sklearn.preprocessing import StandardScaler
import numpy as np
np.random.seed(0)
import warnings
warnings.simplefilter('ignore')
from sklearn.metrics import accuracy_score,f1_score,make_scorer,precision_score,recall_score
from prettytable import PrettyTable

from sklearn.utils import resample


#Loading Dataset
df=pd.read_csv('../data/covid19_online_survey_data.csv')


#Storing Significant Features names Category-wise
smell=['Spices_Herbs1', 'Spices_Herbs2',
       'Spice_Mixtures', 'Other', 'Nasal_irritant']
taste=['Salty', 'Sour','Taste_irritant']
generic=['age',
       'smell_complete_loss', 'moderate_smell_change', 'No_change_smell',
       'slight_change', 'contact_11_20', 'contact_21_50', 'contact_5_10',
       'contact_less_5', 'contact_greater_50', 'contacts_none',
       'no_comorbity', 'Fever','Changes_in_food_flavor','Changes_in_smell', 'Changes_in_taste','No_symptoms',
       'Cough_with_mucus', 'Chest_tightness', 'difficult_breathing',
       'no_chnage_in_taste', 'change_in_bitter', 'change_in_sweet',
       'change_in_sour', 'change_in_salt']
label=['Diagnosed']

num_featue_dict={'gen':1,'smell':0,'taste':0, 'smell+taste':0, 'gen+smell+taste':9}

#Defining Functions

def standarize(X_train,X_test,num_featue_dict,feature_type):
    num_feature_count=num_featue_dict[feature_type]
    if num_feature_count>0:
        X_train_num=X_train[list(X_train.columns.values[0:num_feature_count])]
        X_train_cat=X_train[list(X_train.columns.values[num_feature_count:])]
        #labels=list(X_train['Diagnosed'])


        X_test_num=X_test[list(X_test.columns.values[0:num_feature_count])]
        X_test_cat=X_test[list(X_test.columns.values[num_feature_count:])]
        #labels=list(X_test['Diagnosed'])

        st_scale=StandardScaler().fit(X_train_num)
        X_train_num=pd.DataFrame(st_scale.transform(X_train_num), index=X_train_num.index, columns=X_train_num.columns)
        X_test_num=pd.DataFrame(st_scale.transform(X_test_num), index=X_test_num.index, columns=X_test_num.columns)

        X_train=pd.concat([X_train_num,X_train_cat],axis=1)
        X_test=pd.concat([X_test_num,X_test_cat],axis=1)
    else:
        st_scale=StandardScaler().fit(X_train)
        X_train=pd.DataFrame(st_scale.transform(X_train), index=X_train.index, columns=X_train.columns)
        X_test=pd.DataFrame(st_scale.transform(X_test), index=X_test.index, columns=X_test.columns)
        
    return X_train,X_test

def grid_searchcv(data,num_featue_dict,feature_type):
    
    cv_dict={}
    scoring={'Accuracy':make_scorer(accuracy_score),'Precision':make_scorer(precision_score,average='macro'),'Recall':make_scorer(recall_score,average='macro'),'F1 Score':make_scorer(f1_score,average='macro')}

    inner_cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=0)
    #outer_cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=0)

    mts=['svc','gnb','lr','dt','rf']
    # Set up possible values of parameters to optimize over
    p_grids=[[] for i in range(5)]
    p_grids[0] = {"C": [1, 10, 100, 1000, 10000],"gamma": [.0001, .001, .01, .1, .2, .5]}
    p_grids[1]={'var_smoothing': np.logspace(0,-9, num=100)}
    p_grids[2]={"C": [1, 10, 100, 1000, 10000],"fit_intercept":[True,False],"penalty":['l1', 'l2']}
    p_grids[3]={'criterion':['gini','entropy'],'max_depth':  [1, 5, 10, 50], "min_samples_split": [5, 10, 100, 500]}
    p_grids[4] = { 'n_estimators': [100,200],'max_features': ['auto', 'sqrt', 'log2'],'max_depth': [3, 5, 10],'min_samples_split': [2, 5, 10],'criterion' :['gini', 'entropy']}


    # ML model
    models=[[] for i in range(5)]
    models[0] = SVC(kernel="rbf",random_state=0,class_weight='balanced',probability=True)
    models[1]=GaussianNB()
    models[2]=LogisticRegression(random_state=0,solver='liblinear',max_iter=500,class_weight='balanced')
    models[3]=DecisionTreeClassifier(random_state=0,class_weight='balanced')
    models[4]=RandomForestClassifier(random_state=0,class_weight='balanced')

    #Extracting only significant features along with labels
    values=data.values
    X=data.iloc[:, 0 : -1]
    Y=list(values[:,-1])
    Y=[0 if val=='Non-COVID' else 1 for val in Y]

    #Splitting data into train and test for normalizing features
    #and finding best parameters based on training data only 
    #and preventing the model from seeing the entire data
    from sklearn.model_selection import train_test_split
    X_train, X_test, Y_train, Y_test = train_test_split(X, Y, stratify= Y,  random_state = 0,test_size = 0.3)
    X_train,X_test=standarize(X_train,X_test,num_featue_dict,feature_type)

    for i in range(5):
        mt=mts[i]
        print(mt)
        p_grid=p_grids[i]
        model=models[i]
        clf = GridSearchCV(estimator=model, param_grid=p_grid, cv=inner_cv,scoring=scoring,refit='F1 Score', error_score="raise")
        clf.fit(X_train, Y_train)
        model=clf.best_estimator_
        cv_dict[mt]=model

    return cv_dict


def bootstrap_resampling(data,clf,cv_dict,num_featue_dict,feature_type):
    # run bootstrap
    acc_stats = []
    pre_stats = []
    rec_stats = []
    f1_stats = []
    #auc_stats = []
    for i in range(n_iterations):
        print('i=',i,end='\r')
        # prepare train and test sets
        #data.drop_duplicates(inplace=True)
        values=data.values
        values=[[val[0],val[-1]] for val in values.tolist()]
        values=np.array(values)
        train=resample(values, stratify=values[:,-1],n_samples=n_size,random_state=i)
        ids=[val[0] for val in train.tolist()]
        train=data[data.id.isin(ids)].values
        train=train[:,1:]
        test=data[~data.id.isin(ids)].values
        test=test[:,1:]
        #values=data.values
        #train = resample(values, stratify=values[:,-1],n_samples=n_size,random_state=i)
        #test = np.array([x for x in values if x.tolist() not in train.tolist()])
        train[:,-1]=np.array([0 if val=='Non-COVID' else 1 for val in list(train[:,-1])])
        test[:,-1]=np.array([0 if val=='Non-COVID' else 1 for val in list(test[:,-1])])
        X_train=pd.DataFrame(train[:,:-1])
        #X_train.columns=colnames[:-1]
        X_test=pd.DataFrame(test[:,:-1])
        #X_test.columns=colnames[:-1]
        Y_train=list(train[:,-1])
        Y_test=list(test[:,-1])
        X_train,X_test=standarize(X_train,X_test,num_featue_dict,feature_type)
        # fit model
        model = cv_dict[clf]
        model.fit(X_train, Y_train)
        # evaluate model
        predictions = model.predict(X_test)
        acc_stats.append(accuracy_score(Y_test, predictions))
        pre_stats.append(precision_score(Y_test, predictions,average='macro'))
        rec_stats.append(recall_score(Y_test, predictions,average='macro'))
        f1_stats.append(f1_score(Y_test, predictions,average='macro'))
    alpha = 0.95
    p = ((1.0-alpha)/2.0) * 100
    acc_lower = max(0.0, np.percentile(acc_stats, p))
    pre_lower = max(0.0, np.percentile(pre_stats, p))
    rec_lower = max(0.0, np.percentile(rec_stats, p))
    f1_lower = max(0.0, np.percentile(f1_stats, p))
    p = (alpha+((1.0-alpha)/2.0)) * 100
    acc_upper = min(1.0, np.percentile(acc_stats, p))
    pre_upper = min(1.0, np.percentile(pre_stats, p))
    rec_upper = min(1.0, np.percentile(rec_stats, p))
    f1_upper = min(1.0, np.percentile(f1_stats, p))
    eval_values=[(round(acc_lower*100,2),round(acc_upper*100,2)),(round(pre_lower*100,2),round(pre_upper*100,2)),(round(rec_lower*100,2),round(rec_upper*100,2)),(round(f1_lower*100,2),round(f1_upper*100,2))]
    
    return eval_values




def form_table(stats_dict):
    x = PrettyTable()
    x.field_names = [ "Model", 'Accuracy', 'Precision', 'Recall', 'F1 Score']
    x.add_row(['Naive Bayes']+stats_dict['gnb'])
    x.add_row(['Decison Tree']+stats_dict['dt'])
    x.add_row(['Random Forest']+stats_dict['rf'])
    x.add_row(['Logistic Regression']+stats_dict['lr'])
    x.add_row(['SVM']+stats_dict['svc'])
    print(x)
    return x.get_string()

                            
#********************************************Model Development and Assessment********************************************#     
                       
#******************************************************Feature Set: GEN************************************************#
print('*****Generic******')
##Extracting feature subset
data=df[generic+label]

# Finding Optimal Parameters
cv_dict=grid_searchcv(data,num_featue_dict,'gen')   
    

    

##Model Evaluation Using Bootstrap ReSampling

values=data.values
# configure bootstrap
n_iterations = 1000
n_size = int(len(data) * 0.50)
clfs=['svc','gnb','lr','rf','dt']
stats_dict = {}

for j in range(5):
    clf=clfs[j]
    print(clf,'\n')
    stats_dict[clf]=bootstrap_resampling(data,clf,cv_dict,num_featue_dict,'gen')


data = form_table(stats_dict)

with open('../data/combined_ptable_all.txt', 'a+') as f:
    f.write('\nGEN\n\n\n\n')
    f.write(data)
    
    


#******************************************************Feature Set: OBSAT************************************************#
print('*****OBSAT******')
##Extracting feature subset
data=df[smell+label]

# Finding Optimal Parameters
cv_dict=grid_searchcv(data,num_featue_dict,'smell')   
    

    

##Model Evaluation Using Bootstrap ReSampling

values=data.values
# configure bootstrap
n_iterations = 1000
n_size = int(len(data) * 0.50)
clfs=['svc','gnb','lr','rf','dt']
stats_dict = {}

for j in range(5):
    clf=clfs[j]
    print(clf,'\n')
    stats_dict[clf]=bootstrap_resampling(data,clf,cv_dict,num_featue_dict,'smell')


data = form_table(stats_dict)

with open('../data/combined_ptable_all.txt', 'a+') as f:
    f.write('\nOBSAT\n\n\n\n')
    f.write(data)


#******************************************************Feature Set: GBSAT************************************************#
print('*****GBSAT******')
                            
##Extracting feature subset
data=df[taste+label]

# Finding Optimal Parameters
cv_dict=grid_searchcv(data,num_featue_dict,'taste')   
    

    

##Model Evaluation Using Bootstrap ReSampling

values=data.values
# configure bootstrap
n_iterations = 1000
n_size = int(len(data) * 0.50)
clfs=['svc','gnb','lr','rf','dt']
stats_dict = {}

for j in range(5):
    clf=clfs[j]
    print(clf,'\n')
    stats_dict[clf]=bootstrap_resampling(data,clf,cv_dict,num_featue_dict,'taste')


data = form_table(stats_dict)

with open('../data/combined_ptable_all.txt', 'a+') as f:
    f.write('\nGBSAT\n\n\n\n')
    f.write(data)




#******************************************************Feature Set: OBSAT+GBSAT************************************************#
print('*****OBSAT+GBSAT******')
            
##Extracting feature subset
data=df[smell+taste+label]

# Finding Optimal Parameters
cv_dict=grid_searchcv(data,num_featue_dict,'smell+taste')   
    

    

##Model Evaluation Using Bootstrap ReSampling

values=data.values
# configure bootstrap
n_iterations = 1000
n_size = int(len(data) * 0.50)
clfs=['svc','gnb','lr','rf','dt']
stats_dict = {}

for j in range(5):
    clf=clfs[j]
    print(clf,'\n')
    stats_dict[clf]=bootstrap_resampling(data,clf,cv_dict,num_featue_dict,'smell+taste')


data = form_table(stats_dict)

with open('../data/combined_ptable_all.txt', 'a+') as f:
    f.write('\nOBSAT+GBSAT\n\n\n\n')
    f.write(data)



#******************************************************Feature Set: GEN+ OBSAT+GBSAT************************************************#
print('*****Generic+OBSAT+GBSAT******')
                            
##Extracting feature subset
data=df[smell+taste+generic+label]

# Finding Optimal Parameters
cv_dict=grid_searchcv(data,num_featue_dict,'gen+smell+taste')   
    

    

##Model Evaluation Using Bootstrap ReSampling

values=data.values
# configure bootstrap
n_iterations = 1000
n_size = int(len(data) * 0.50)
clfs=['svc','gnb','lr','rf','dt']
stats_dict = {}

for j in range(5):
    clf=clfs[j]
    print(clf,'\n')
    stats_dict[clf]=bootstrap_resampling(data,clf,cv_dict,num_featue_dict,'gen+smell+taste')


data = form_table(stats_dict)

with open('../data/combined_ptable_all.txt', 'a+') as f:
    f.write('\nGEN+OBSAT+GBSAT\n\n\n\n')
    f.write(data)




