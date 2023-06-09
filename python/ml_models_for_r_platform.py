# -*- coding: utf-8 -*-
"""ML Models for R platform.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1rg7u6K1omoQVNY5md7_8mejuPuY-xwUv
"""

import numpy as np
import pandas as pd
import warnings
warnings.filterwarnings('ignore')

from sklearn.model_selection import GridSearchCV
from sklearn.preprocessing import LabelEncoder,StandardScaler,OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline

#Classifiers
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
#Regressor
from lightgbm import LGBMRegressor

filtered_data = pd.read_csv("C:/Users/emmet/OneDrive/Documents/GitHub/Cancer_Decision_Support_Tool/python/new_filtered data.csv")

def train_classification_model(model,X, y):
    numeric_col = X.select_dtypes(include=np.number).columns
    categorical_col = X.select_dtypes(exclude=np.number).columns

    le = LabelEncoder()
    y = le.fit_transform(y)

    preprocessor = ColumnTransformer(transformers=[
        ('num', StandardScaler(), numeric_col),
        ('cat', OneHotEncoder(handle_unknown='ignore'), categorical_col)
    ], sparse_threshold=0)

    pipeline = Pipeline(steps=[('preprocessor', preprocessor), ('model', model)])
    pipeline.fit(X, y)
    
    return pipeline, le

def train_regression_model(model, X, y):
    numeric_col = X.select_dtypes(include=np.number).columns
    categorical_col = X.select_dtypes(exclude=np.number).columns
    
    preprocessor = ColumnTransformer(transformers=[
        ('num', StandardScaler(), numeric_col),
        ('cat', OneHotEncoder(handle_unknown='ignore'), categorical_col)
    ], sparse_threshold=0)

    pipeline = Pipeline(steps=[('preprocessor', preprocessor), ('model', model)])
    pipeline.fit(X, y)
    return pipeline

def predict_classification(model, input_data, feature_names, le):
    input_df = pd.DataFrame([input_data], columns=feature_names)
    X = model.named_steps['preprocessor'].transform(input_df)
    y_pred = model.named_steps['model'].predict(X)
    return le.inverse_transform(y_pred)

def predict_regression(model, input_data, feature_names):
    input_df = pd.DataFrame([input_data], columns=feature_names)
    X = model.named_steps['preprocessor'].transform(input_df)
    y_pred = model.named_steps['model'].predict(X)
    return y_pred

def input_function():
    input_data = []

    input_data.append(input("Enter Sex (M or F): "))
    input_data.append(int(input("Enter Age: ")))
    input_data.append(float(input("Enter Height (m): ")))
    input_data.append(float(input("Enter BMI start treat (kg/m2): ")))
    input_data.append(input("Enter Diagnosis: "))
    input_data.append(input("Enter Grade: "))
    input_data.append(input("Enter HPV status: "))
    input_data.append(input("Enter Induction Chemotherapy (Yes or No): "))
    input_data.append(input("Enter Chemotherapy Regimen: "))
    input_data.append(input("Enter Platinum-based chemotherapy (Yes or No): "))
    input_data.append(input("Enter Received Concurrent Chemoradiotherapy? (Yes or No): "))
    input_data.append(input("Enter CCRT Chemotherapy Regimen: "))
    input_data.append(input("Enter Surgery Summary: "))
    input_data.append(int(input("Enter Smoking History: ")))
    input_data.append(int(input("Enter Current Smoker (1 for Yes, 0 for No): ")))
    input_data.append(input("Enter Stage: "))

    return input_data

# Load the data
df = filtered_data
X = df.iloc[:, 2:18]
Y = df.iloc[:, 18:]

# # Encode the target variable
# le = LabelEncoder()

# Train Logistic Regression Classifier for Response or Recurrence
y_response_reccurence = Y['Response or Recurrence']
lr_classifier = LogisticRegression()
lr_model, le_response_recurrence = train_classification_model(lr_classifier, X, y_response_reccurence)

# Train Decision Tree Classifier for 'Alive or Dead'
y_alive_dead = Y['Alive or Dead']
dt_classifier = DecisionTreeClassifier()
dt_model, le_alive_dead = train_classification_model(dt_classifier, X, y_alive_dead)

# Train LightGBM Regressor for 'Survival (months)'
y_survival = Y['Survival  (months)']
lgbm_regressor_survival = LGBMRegressor(learning_rate=0.0001, n_estimators=50)
lgbm_model_survival = train_regression_model(lgbm_regressor_survival, X, y_survival)

# Train LightGBM Regressor for 'Total RT treatment time (days)'
y_rt_treatment_time = Y['Total RT treatment time (days)']
lgbm_regressor_rt_treatment_time = LGBMRegressor(learning_rate=0.0001, n_estimators=50)
lgbm_model_rt_treatment_time = train_regression_model(lgbm_regressor_rt_treatment_time, X, y_rt_treatment_time)
