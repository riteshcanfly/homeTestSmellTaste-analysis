trial <- matrix(c(82,4,13,6,112,5,11,6), ncol=2)
colnames(trial) <- c('COVID', 'Non-COVID')
rownames(trial) <- c('No_smoke', 'Yes_curr','Yes_prev','Prefer not to say')
trial.table <- as.table(trial)
print(trial.table)
print('smoke')
print(chisq.test(trial.table))


trial <- matrix(c(53,15,11,26,114,9,6,5), ncol=2)
colnames(trial) <- c('COVID', 'Non-COVID')
rownames(trial) <- c('No_change_smell', 'slight_change','moderate_smell_change','smell_complete_loss')
trial.table <- as.table(trial)
print(trial.table)
print('smell')
print(chisq.test(trial.table))


print('contact')
# Simulated data
trial <- matrix(c(5,14,25,28,21,12,14,33,40,19,15,13), ncol=2)
colnames(trial) <- c('COVID', 'Non-COVID')
rownames(trial) <- c('contacts_none', 'contact_less_5','contact_5_10','contact_11_20','contact_21_50','contact_greater_50')
trial.table <- as.table(trial)
print(trial.table)
print(chisq.test(trial.table))

library(dplyr)
dft<-read.csv('../data/covid19_online_survey_data.csv')
dftp<-dft[dft$Diagnosed == "COVID", ]
dftn<-dft[dft$Diagnosed == "Non-COVID", ]
binary_attributes=colnames(dft)[c(30:67)]
for (att in binary_attributes){
    
    pyes<-sum(dftp[att] == 1)
    pno<-105-pyes
    nyes<-sum(dftn[att] == 1)
    nno<-134-nyes
    print(att)
    trial <- matrix(c(pyes,pno,nyes,nno), ncol=2)
    colnames(trial) <- c('COVID', 'Non-COVID')
    rownames(trial) <- c('YES','NO')
    trial.table <- as.table(trial)
    print(trial.table)
    print(chisq.test(trial.table))
}