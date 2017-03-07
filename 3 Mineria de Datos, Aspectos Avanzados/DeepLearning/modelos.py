####### FRANCISCO PÉREZ HERNÁNDEZ #######
#Cargamos todo lo necesario
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import random
from subprocess import check_output
#Marchine Learning Specific
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split


#Comprobamos que están los fichero test y train en la carpeta input
print(check_output(["ls", "./input"]).decode("utf8"))
#Leemos los ficheros de datos
dtrain = pd.read_csv('./input/train.csv')
dtest = pd.read_csv('./input/test.csv')

#Guardamos los dataset tanto completos como divididos en train y test
Xfull, yfull = dtrain.drop('label',axis =1), dtrain['label']
Xtrain, Xvalid, ytrain, yvalid = train_test_split(Xfull, yfull, test_size=0.1)

#Vamos a realizar un random forest de 100 árboles para comprobar la eficacia del modelo
rf = RandomForestClassifier(100)
rf.fit(Xtrain,ytrain)
print('Training score:',rf.score(Xtrain,ytrain))
print('Validation score:', rf.score(Xvalid,yvalid))

#Ahora vamos a hacerlo sobre todo el conjunto de datos para realizar una predicción en kaggle
rf_full = RandomForestClassifier(100)
rf_full.fit(Xfull,yfull)
rf_result = rf_full.predict(dtest)
#Guardamos esta predicción en un fichero de salida
pd.DataFrame({'ImageId': range(1, len(rf_result)+1), 'Label':rf_result}).to_csv('result/1rf.csv',index = None)


