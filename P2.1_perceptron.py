"""
ANN Simple
Saul Mercado Pedroza
"""

# Librerias
import random as rd
import numpy as np
import math
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay
import matplotlib.pyplot as plt

dataset = [
    {"entrada":[0,0,1], "salida":0},
    {"entrada":[1,1,1], "salida":1},
    {"entrada":[1,0,1], "salida":1},
    {"entrada":[0,1,1], "salida":0}
]
ejemplos = [
    [1, 1, 0],
    [0, 0, 0],
    [1, 0, 0]
]

class ANN():
    def __init__(self):
        rd.seed(2001)
        self.__pesos = [
            rd.uniform(-1, 1),
            rd.uniform(-1, 1),
            rd.uniform(-1, 1)
        ]

    #Núcleo - Core
    def __nucleo(self, entradas):
        suma = 0
        for i, entradas in enumerate(entradas):
            suma += self.__pesos[i] * entradas
        return suma

    # Función de activación - sigmoide
    def __factSigmoid(self, score):
        return 1 / (1 + math.exp(-score))

    #Función de activacion - RELU
    def __factRelu(self, score):
        return np.maximum(0,score)

    #Función de activación - Binary Step
    def __factBinaryStep(self, score):
        return 1 if score >= 0 else 0

    # Predicción - Salida
    def predict(self, datos, factivacion):
        score = self.__nucleo(datos)
        method_name = f"_ANN__{factivacion}"
        method = getattr(self, method_name, None)
        y_pred = method(score)
        return y_pred

    # Función de coste
    def __coste(self, y_pred, y_actual):
        return((y_pred - y_actual)**2)/2

    # Obtener pesos
    def getPesos(self):
        return self.__pesos


    # Training
    def training(self, datos, epochs, factivacion):
        for epoch in range(epochs):
            loss_epoch = 0
            correct_predictions = 0
            for obs in datos:
                y_pred = self.predict(obs["entrada"], 'factSigmoid')
                coste = self.__coste(y_pred, obs["salida"])
                error = obs["salida"] - y_pred
                loss_epoch += coste
                correct_predictions += (1 if round(y_pred) == obs["salida"] else 0)

                # Ajustar pesos
                for index in range(len(self.__pesos)):
                    entrada = obs["entrada"][index]
                    adj = entrada * coste * error
                    print("Pesos adj",index,"-[",epoch,"]: ", adj)
                    self.__pesos[index] += adj

print(dir(ANN()))

# Modelo
neurona = ANN()
print("Pesos iniciales: ", neurona.getPesos())

# Entrenamiento
#factSigmoid
neurona.training(dataset, 30, 'factSigmoid')

#factRelu
neurona.training(dataset, 6, 'factRelu')

#factBinaryStep
neurona.training(dataset, 50, 'factBinaryStep')

# Predicción
resultados = []
for entrada in ejemplos:
    prediccion = neurona.predict(entrada, 'factSigmoid')
    resultados.append(round(prediccion))

# Imprimir los resultados de las predicciones
for i, resultado in enumerate(resultados):
    print(f"Predicción para nueva entrada {i+1}: {resultado}")

# Construir la matriz de confusión
etiquetas_verdaderas = [0, 0, 0]

matriz = confusion_matrix(etiquetas_verdaderas, resultados)
display = ConfusionMatrixDisplay(confusion_matrix=matriz, display_labels=[0, 1])

# Mostrar la matriz de confusión
display.plot()
plt.show()

'''
La función de activación que me pareció la más adecuada fue la sigmoide principalmente por los epochs, 
se sabe que la curva de aprendizaje debería de ser de manera contínua y fue algo que presentó esta función de activación
ya que en el caso de Relu con pocas epochs resultaba tener una efectividad buena siendo después de la epoca 8 un sobreajuste
y este aprendizaje tan rápidopodría afectar al mismo modelo. Por su parte la función de activación Binary Step por su
formula y que cualquier valor mayor o igual a 0 sería 1, imposibilita su uso en este caso en particular
'''