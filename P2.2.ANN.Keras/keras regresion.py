# -*- coding: utf-8 -*-
"""keras regresion.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/13viqsiNYavWuXZSwJCWVYscSXySGelj3
"""

#Saúl Mercado Pedroza#
#Librerías#
import tensorflow as tf
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
#scikit-learn
from sklearn.preprocessing import LabelEncoder, OneHotEncoder, StandardScaler
from sklearn.compose import ColumnTransformer
from sklearn.model_selection import train_test_split

from google.colab import drive

drive.mount('/content/drive')

df_bank = pd.read_csv('/content/drive/MyDrive/Codigod/Bank.csv')

X = df_bank.iloc[:,3:13].values
Y = df_bank.iloc[:,12].values

X[:,1] = LabelEncoder().fit_transform(X[:,1])
X[:,2] = LabelEncoder().fit_transform(X[:,2])

one = ColumnTransformer(
    [('one_hot_encoder', OneHotEncoder(categories = 'auto'),[2])],
    remainder='passthrough'
)

X = one.fit_transform(X)

#Escalado
X = StandardScaler().fit_transform(X)

X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state = 10)

print(X.shape)

#Modelo ANN
from keras.models import Sequential
from keras.layers import Dense

ann = Sequential()
#Capa de entrada
ann.add(
    Dense(units = 10,
          kernel_initializer = 'uniform',
          input_dim = 11)
)

#Capa oculta
ann.add(
    Dense(units = 10,
          kernel_initializer = 'uniform',
          activation = 'relu'
          )
    )
#Capa de salida

ann.add(
    Dense(units = 1,
          activation = 'linear',
          kernel_initializer = 'glorot_uniform')
)

#Entrenador
ann.compile(
    optimizer = 'adam',
    loss = 'mean_squared_error',
    metrics = ['mean_squared_error'])

#Training
ann.fit(X_train, Y_train,
        epochs = 50,
        batch_size = 50)

#Guardar modelo
from keras.models import load_model

ann.save('S22.ann.h5')

#Traer el modelo
modelo = load_model ('S22.ann.h5')

#Prediccion
Y_pred = modelo.predict(X_test)

#Visualizar la arquitectura de la ANN
from keras.utils import plot_model
plot_model(modelo, to_file='S22.ann.png',
        show_shapes = True,
        show_layer_activations= True,
        show_layer_names= True)

import matplotlib.pyplot as plt
import numpy as np

Y_test = Y_test.flatten()
Y_pred = Y_pred.flatten()

print("Y_test shape:", Y_test)
print("Y_pred shape:", Y_pred)

#Mostrar distrubuciones de los calores verdderos contra las predicciones#
plt.figure(figsize=(10, 6))
plt.scatter(Y_test, Y_pred, color='blue', label='Datos')
plt.title('Comparación de valores reales y predicciones')
plt.xlabel('Valores Reales')
plt.ylabel('Valores Predichos')

# Dibuja la línea de mejor ajuste
z = np.polyfit(Y_test, Y_pred, 1)
p = np.poly1d(z)
plt.plot(Y_test, p(Y_test), "r--", label='Línea de Mejor Ajuste')  # Dibuja la línea de ajuste

plt.legend()
plt.show()

#Hacerlas de una sola dimensión#
Y_test_oneDim = Y_test.flatten()
Y_pred_oneDim = Y_pred.flatten()

data_comp = pd.DataFrame({'True Values':Y_test_oneDim, 'Predictions':Y_pred_oneDim})

plt.figure(figsize=(10, 6))
sns.violinplot(data=data_comp)
plt.title('Comparision with true valuer and predictions')
plt.show()