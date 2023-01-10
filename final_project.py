# -*- coding: utf-8 -*-
"""Final Project.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1ZAH_MB7vU9F3jDfETRSFPObIOJKnsJY7
"""

# Final Project

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

df = pd.read_csv("Admission_Predict.csv")

df.head()

df.isna().sum()

data = df.drop(columns='Serial No.')
data.head()

"""Support Vector Model """

#Support Vector Regression Graph
#from sklearn.svm import SVR
#sns.scatterplot(data = data, x=data['CGPA'], y=data['GRE Score'], hue = data['ChanceOfAdmit'], palette= 'Accent')

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

#Train Test Split
X = data.drop(['Chance of Admit '], axis=1)
y = data['Chance of Admit ']

y

#X_train and X_test
X_train, X_test, y_train, y_test = train_test_split( X, y, test_size=0.3, random_state=101)

from sklearn.svm import SVR

#scale X_train and X_test
scaler = StandardScaler()
scaled_X_train = scaler.fit_transform(X_train)
scaled_X_test = scaler.transform(X_test)

#scaled_X_test

#scaled_X_train

#Define Support Vector Regression Model
svr_model = SVR()

svr_model.fit(scaled_X_train, y_train)

svr_model.score(scaled_X_train, y_train)

svr_model.score(scaled_X_test, y_test)

#predictions
y_pred = svr_model.predict(scaled_X_test)
y_pred

#Grid search -- grid1 for svr model
from sklearn.model_selection import GridSearchCV
param_grid = {'C':[0.01,0.1,1, 10, 50, 100, 1000],'kernel':['linear','rbf', 'poly', 'sigmoid']} #trying many C values
grid1 = GridSearchCV(svr_model,param_grid)
grid1.fit(scaled_X_train,y_train)

#Best parameters for SVR model
grid1.best_params_

grid1.score(scaled_X_test, y_test)

y_pred = grid1.predict(scaled_X_test)

from sklearn.metrics import mean_absolute_error
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import cross_validate,cross_val_score

MAE = mean_absolute_error(y_test,y_pred)
MSE = mean_squared_error(y_test,y_pred)
RMSE = np.sqrt(MSE)

scores = cross_validate(svr_model,scaled_X_train,y_train,
                         scoring=['neg_mean_absolute_error', 'neg_mean_squared_error','max_error'],cv=5)

scores

pd.DataFrame(scores).mean()

#SVM
MSE

#SVM
MAE

#SVM
RMSE

"""Linear Model"""

#linear model naomi
from sklearn.linear_model import LinearRegression
scaler = StandardScaler()
lmmodel=LinearRegression()
lmmodel.fit(scaled_X_train, y_train)

lmmodel.score(scaled_X_train, y_train)

lmmodel.score(scaled_X_test, y_test)

lmmodel.intercept_

print(pd.DataFrame({'Predictor':X.columns,'coefficient':lmmodel.coef_}))

from sklearn.linear_model import Lasso 
lasso_model=Lasso(alpha=50,max_iter=1000,tol=0.1)
lasso_model.fit(scaled_X_train,y_train)

lasso_model.score(scaled_X_train,y_train)

lasso_model.score(scaled_X_test,y_test)

print(pd.DataFrame({'Predictor':X.columns,'coefficient':lasso_model.coef_}))

scores = cross_validate(lmmodel,scaled_X_train,y_train,
                         scoring=['neg_mean_absolute_error','neg_mean_squared_error','max_error'],cv=5)

scores

pd.DataFrame(scores).mean()

y_pred1 = lmmodel.predict(scaled_X_test)

MAE = mean_absolute_error(y_test,y_pred1)
MSE = mean_squared_error(y_test,y_pred1)
RMSE = np.sqrt(MSE)

MAE

MSE

RMSE

"""Decision Tree Regression Model"""

from sklearn.tree import DecisionTreeRegressor

dtr_model = DecisionTreeRegressor()

dtr_model.fit(scaled_X_train, y_train)

dtr_model.score(scaled_X_train, y_train)

dtr_model.score(scaled_X_test, y_test)

y_pred = dtr_model.predict(scaled_X_test)
y_pred

from sklearn.metrics import mean_absolute_error
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import cross_validate,cross_val_score

MAE = mean_absolute_error(y_test,y_pred)
MSE = mean_squared_error(y_test,y_pred)
RMSE = np.sqrt(MSE)

scores = cross_validate(dtr_model,scaled_X_train,y_train,
                         scoring=['neg_mean_absolute_error','neg_mean_squared_error','max_error'],cv=5)

scores

pd.DataFrame(scores).mean()

MSE

MAE

RMSE

# plot of dtr_model
from sklearn.tree import plot_tree
plot_tree(dtr_model,filled=True,feature_names=X.columns);
