"""
Saul Mercado Pedroza
"""

#Librerías#
import tensorflow as tf
import pandas as pd
#scikit-learn
from sklearn.preprocessing import LabelEncoder, OneHotEncoder, StandardScaler
from sklearn.compose import ColumnTransformer
from sklearn.model_selection import train_test_split

df_bank = pd.read_csv('../Datasets & Lib/Bank.csv')

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
    Dense(units = 6,
          kernel_initializer = 'uniform',
          input_dim = 11)
)

#Capa oculta
ann.add(
    Dense(units = 6,
          kernel_initializer = 'uniform',
          activation = 'sigmoid'
          )
    )
#Capa de salida

ann.add(
    Dense(units = 1,
          activation = 'sigmoid',
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