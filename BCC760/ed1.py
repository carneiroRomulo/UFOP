# Nome: Rômulo de Oliveira Carneiro
# Matrícula: 19.1.4107

DECIMAL_PLACES = 4

def gaussian_eliminitation_without_pivoting(A: list[list], b: list, R: list) -> list:
    max_depth = len(A)
    for depth in range(0, max_depth-1):
        pivo = A[depth][depth]
        for row in range(depth+1, max_depth):
            multiplier = - (A[row][depth] /  pivo)
            for column in range(0, max_depth):
                A[row][column] += multiplier * A[depth][column]
            b[row] += multiplier * b[depth]
    
    C3 = round(b[2] / A[2][2], DECIMAL_PLACES)
    print(b, A)
    C2 = round((b[1] - (C3 * A[1][2])) / A[1][1], DECIMAL_PLACES)
    C1 = round((b[0] - (C3 * A[0][2]) - (C2 * A[0][1])) / A[0][0], DECIMAL_PLACES)
    C = [C1, C2, C3]
    
    for i in range(3):
        for j in range(3):
            A[i][j] = A[i][j] * C[j]
        A[i] = A[i][0] + A[i][1] + A[i][2]
        R[i] = round(b[i] - A[i], DECIMAL_PLACES)
    return C

def gaussian_eliminitation_with_pivoting(A, b):
    for i in range(0, 3):
        if i == 0:
            if (A[0][0] > A[1][0]) & (A[0][0] > A[2][0]):
                pivo = A[i][i]
            elif (A[1][0] > A[0][0]) & (A[1][0] > A[2][0]):
                A[0], A[1] = A[1], A[0]
                b[0], b[1] = b[1], b[0]
                pivo = A[i][i]
            else:
                A[0], A[2] = A[2], A[0]
                b[0], b[2] = b[2], b[0]
                pivo = A[i][i]
        if i == 1:
            if A[1][1] > A[2][1]:
                pivo = A[i][i]
            else:
                A[1], A[2] = A[2], A[1]
                b[1], b[2] = b[2], b[1]
                pivo = A[i][i]
        for j in range(i+1, 3):
            multiplier = -(A[j][i] / pivo)
            for k in range(0, 3):
                A[j][k] += multiplier * A[i][k]
                A[j][k] = round(A[j][k], 4)
            b[j] += multiplier * b[i]

    C3 = round((b[2] / A[2][2]), DECIMAL_PLACES)
    C2 = round(((b[1] - (C3 * A[1][2])) / A[1][1]), DECIMAL_PLACES)
    C1 = round(((b[0] - (C3 * A[0][2]) - (C2 * A[0][1])) / A[0][0]), DECIMAL_PLACES)
    C = [C1, C2, C3]
    return C

def LU_decomposition(L: list[list], U: list[list], b: list) -> list:
    for i in range(0, 2):
        pivo = U[i][i]
        for j in range(i+1, 3):
            multiplier = -(U[j][i] / pivo)
            for k in range(0, 3):
                U[j][k] += multiplier * U[i][k]
                U[j][k] = U[j][k]
                L[j][i] = -multiplier
    Y1 = b[0]
    Y2 = b[1] - (Y1 * L[1][0])
    Y3 = b[2] - (Y1 * L[2][0]) - (Y2 * L[2][1])
    Y = [Y1, Y2, Y3]

    C3 = round(Y[2] / U[2][2], DECIMAL_PLACES)
    C2 = round((Y[1] - (C3 * U[1][2])) / U[1][1], DECIMAL_PLACES)
    C1 = round((Y[0] - (C3 * U[0][2]) - (C2 * U[0][1])) / U[0][0], DECIMAL_PLACES)
    X = [C1, C2, C3]
    return X


def gauss_seidel(A: list[list], b: list, x1: list, x2: list, x3: list, max_iterations: int, max_precision: float):
    for k in range(1, max_iterations):
        x1[k] = (1 / A[0][0]) * (b[0] + (10 * x2[k - 1]) + (4 * x3[k - 1]))
        x2[k] = (1 / A[1][1]) * (b[1] + (10 * x1[k])     + (5 * x3[k - 1]))
        x3[k] = (1 / A[2][2]) * (b[2] + (4  * x1[k])     + (5 * x2[k]))
        precision = max(abs(x1[k - 1] - x1[k]), abs(x2[k- 1 ] - x2[k]), abs(x3[k - 1] - x3[k]))
        iteration = k
        if (precision < max_precision) | (k == max_iterations): break
    X = [round(x1[iteration], DECIMAL_PLACES), round(x2[iteration],DECIMAL_PLACES), round(x3[iteration], DECIMAL_PLACES)]
    return X, iteration, round(precision, DECIMAL_PLACES)


def jacobi(A: list[list], b: list, x1: list, x2: list, x3: list, max_iterations: int, max_precision: float):
    for k in range(1, max_iterations):
        x1[k] = (1 / A[0][0]) * (b[0] + (10 * x2[k - 1]) + (4 * x3[k - 1]))
        x2[k] = (1 / A[1][1]) * (b[1] + (10 * x1[k - 1]) + (5 * x3[k - 1]))
        x3[k] = (1 / A[2][2]) * (b[2] + (4  * x1[k - 1]) + (5 * x2[k - 1]))
        precision = max(abs(x1[k-1] - x1[k]), abs(x2[k-1] - x2[k]), abs(x3[k-1] - x3[k]))
        iteration = k
        if (precision < max_precision) | (k == max_iterations): break
    X = [round(x1[iteration], 4), round(x2[iteration], 4), round(x3[iteration], 4)]
    return X, iteration, round(precision, DECIMAL_PLACES)


# QUESTAO 01
print('QUESTAO 01 - ELIMINAÇÃO DE GAUSS')
A = [[-130, 30, 0], [90, -90, 0], [40, 60, -120]]
b = [-200, 0, -500]
R = [0, 0, 0]
C = gaussian_eliminitation_without_pivoting(A, b, R)
print(f'C = {C}')
print(f'Resíduo = {R}')

# QUESTAO 02
A = [[0.18, 0.3, 0.55], [0.52, 0.2, 0.25], [0.3, 0.5, 0.2]]
b = [5700, 4800, 5800]
C = gaussian_eliminitation_with_pivoting(A, b)
print('\nQUESTAO 02 - ELIMINAÇÃO DE GAUSS COM PIVOTAÇÃO')
print(f"X: {C}")

# QUESTAO 03
A = [[10, 20, 20], [50, 40, 10], [30, 10, 40]]
b = [100, 300, 200]
L = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
U = A
X = LU_decomposition(L, U, b)
print('\nQUESTAO 03 - DECOMPOSIÇÃO LU')
print(f"X: {X}")

# QUESTAO 04
max_iterations = 6
max_precision = 0.2
A = [[20, -10, -4], [-10, 25, -1], [-4, -5, 20]]
b = [26, 0, 7]
x1, x2, x3 = [0]*max_iterations, [0]*max_iterations, [0]*max_iterations

# QUESTAO 04b - GAUSS-SEIDEL
X, iteration, precision = gauss_seidel(A, b, x1, x2, x3, max_iterations, max_precision)
print("\nQUESTAO 4b - GAUSS-SEIDEL")
print(f"X: {X}")
print(f"Precisão: {precision}")
print(f"Nº de Iterações: {iteration}")

# QUESTAO 04b - JACOBI
X, iteration, precision = jacobi(A, b, x1, x2, x3, max_iterations, max_precision)
print("\nQUESTAO 4c - JACOBI")
print(f"X: {X}")
print(f"Precisão: {precision}")
print(f"Nº de Iterações: {iteration}")