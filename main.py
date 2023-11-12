import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import PolynomialFeatures
import matplotlib.pyplot as plt

input_data = pd.read_csv('input.csv')
output_data = pd.read_csv('output.csv')

regressor = PolynomialFeatures(degree=19)
x_poly = regressor.fit_transform(input_data)
print(x_poly)