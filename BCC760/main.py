import pdb
def substituicao_retroativa(A,b):
    n = len(A)
    x = n * [0]

    for k in range(1, n-1):
        result = 0
        for i in range(k+1, n):
            result = (A[i][k] / A[k][k])
            for j in range(k, n):
                A[i][j] = A[i][j] + (result * A[k][j])
            b[i] = b[i] + (result * b[k])
        x.append(b[i])

    pdb.set_trace()

A = [[5, -2, 6, 1],
     [0, 3, 7, -4],
     [0, 0, 4, 5],
     [0, 0, 0, 2],
    ]
b = [1, -2, 28, 8]

print(substituicao_retroativa(A, b))

def eliminacao_de_gauss(A, b, *args, **kwargs):
    equations = kwargs.pop("equations")


def decomposicao_lu(*args, **kwargs):
    equations = kwargs.pop("equations")


def metodo_jacob(*args, **kwargs):
    equations = kwargs.pop("equations")


def metodo_gauss_seidel(*args, **kwargs):
    equations = kwargs.pop("equations")