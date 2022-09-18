# ESTUDO DIRIGIDO 2
# RÔMULO DE OLIVEIRA CARNEIRO
# 19.1.4107

def lagrange_method(wanted_x:float|int, X:list, Y: list) -> float:
    coefficients = []

    for index in range(len(X)):
        L = 1
        for j in range(len(X)):
            if index != j: L *= (wanted_x - X[j]) / (X[index] - X[j])
        coefficients.append(L)
    
    pn = 0
    for i in range(len(coefficients)):
        pn += Y[i] * coefficients[i]
    return pn


def divided_diff(x: list, y: list, degree: int) -> list:
    coefficients = [y[0]] + [0] * (degree - 1)
    
    for i in range(degree - 1):
        for j in range(degree - i - 1):
            numerator = y[j + 1] - y[j]
            denominator = x[j + 1 + i] - x[j]
            y[j] = numerator / denominator
        coefficients[i + 1] = round(y[0], 4)
    return coefficients

def newton_polynomial(x: list, coefficients: list, value: float|int) -> float:
    n = len(coefficients)
    result = coefficients[0] \
        + (value - x[0]) * coefficients[1] \
        + (value - x[0]) * (value - x[1]) * coefficients[2] \
        + (value - x[0]) * (value - x[1]) * (value - x[2]) * coefficients[3]
    return round(result, 4)

def truncation_error(x: list, coefficients: list, value: float|int, degree: int) -> float:
    error = coefficients[-1]
    for i in range(degree):
        error *= (value - x[i])
    return abs(round(error, 4))


def finite_diff(p, x, y, n):
    def calculate_u(u, n):
        temp = u
        for i in range(1, n):
            temp *= (u - i)
        return temp
    
    def factorial(n):
        fact = 1
        for i in range(2, n + 1):
            fact *= i
        return fact
    
    for i in range(1, n):
        for j in range(n - i):
            y[j][i] = round(y[j + 1][i - 1] - y[j][i - 1], 4)
    
    sum = y[0][0]
    u = (p - x[0]) / (x[1] - x[0])
    for i in range(1,n):
        sum = sum + (calculate_u(u, i) * y[0][i]) / factorial(i)
    return sum


# QUESTAO 01 - LAGRANGE
wanted_x = 95
X = [70, 85, 100, 115, 130]
Y = [15.56, 14.28, 13.27, 11.30, 10.50]
question_1 = lagrange_method(wanted_x, X, Y)
print("QUESTION 01 | LAGRANGE")
print(f"p({str(wanted_x)}) = {round(question_1, 4)}\n")

# QUESTAO 02 - DIFERENÇAS DIVIDIDAS
value = 93
degree = 4
x = [60, 80, 100, 120, 140, 160, 180]
y = [76, 95, 112, 138, 151, 170, 192]
divided_diff = divided_diff(x, y, degree)
question_2 = newton_polynomial(x, divided_diff, value)
truncation_error = truncation_error(x, divided_diff, value, degree)
print("QUESTION 02 | DIVIDED DIFFERENCES")
print(f"y({value}) = {question_2}")
print(f"Et({value}) = {truncation_error}\n")

# QUESTAO 03 - DIFERENÇAS FINITAS
p = 11
x = [0, 5, 10, 15, 20, 25, 30, 35, 40]
n = len(x)
y = [[0 for i in range(n)] for j in range(n)]
y[0][0] = 0.9999
y[1][0] = 0.9998
y[2][0] = 0.9997
y[3][0] = 0.9991
y[4][0] = 0.9982
y[5][0] = 0.9971
y[6][0] = 0.9957
y[7][0] = 0.9941
y[8][0] = 0.9902
print("QUESTION 03 | FINITE DIFFERENCES")
result = finite_diff(p, x, y, n)
print(f"p({p}): {round(result, 4)}")