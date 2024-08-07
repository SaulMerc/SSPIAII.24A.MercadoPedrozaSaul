# -*- coding: utf-8 -*-
"""
Saúl Mercado Pedroza
"""

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
Y = df_bank.iloc[:,13].values

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

"""Modelo"""

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
          activation = 'relu',
          kernel_initializer = 'uniform')
)

#Entrenador
ann.compile(
    optimizer = 'adam',
    loss = 'binary_crossentropy',
    metrics = ['accuracy'])

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
Y_pred = (Y_pred > 0.5)

#Visualizar la arquitectura de la ANN
from keras.utils import plot_model
plot_model(modelo, to_file='S22.ann.png',
           show_shapes = True,
           show_layer_activations= True,
           show_layer_names= True)

from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay
import matplotlib.pyplot as plt

matriz = confusion_matrix(Y_test,Y_pred)

print(matriz)

#Los cambios lo realicé en la cantidad de neuronas y la función de activación
#Dejando un total de units = 10 y la activation = relu
#Para realizar los cambios se tomó de ejemplo un blok publicado por keep coding y un libro
#digital por parte del MIT enlace: https://www.deeplearningbook.org/
#fuandamentan diversas cosas entre ellas el cambio de la arquitectura como afecta un solo dataset
#algunas cosas un tanto más como la optimización y límites teóricos
#aunque no dicen específicamente cuanto modificar en una parte en específico
#explican como la cantidad de neuronas y el algoritmo afecta al modelo de datos

#Mostrar distrubuciones de los calores verdderos contra las predicciones#
plt.figure(figsize=(8, 4))
plt.scatter(range(len(Y_test)), Y_test, color='blue', label='True Values', alpha= 1)
plt.scatter(range(len(Y_pred)), Y_pred, color='red', label='Predictions')
plt.title('Comparación de las predicciones con los valores reales')
plt.show()

#Hacerlas de una sola dimensión#
Y_test_oneDim = Y_test.flatten()
Y_pred_oneDim = Y_pred.flatten()

data_comp = pd.DataFrame({'True Values':Y_test_oneDim, 'Predictions':Y_pred_oneDim})

plt.figure(figsize=(10, 6))
sns.violinplot(data=data_comp)
plt.title('Comparision with true valuer and predictions')
plt.show()