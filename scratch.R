A <- BinNumeric(lower=0, upper=20)
B <- BinNumeric(lower=10, upper=30)
C <- BinNumeric(lower=25, upper=50)

combinable(A, B)
combine(A, B) ## Combines and creates a single Bin with the new range

combinable(A, C)
combine(A, C) ## returns a list of the inputs

Reduce(combine, list(A, B, C))
Reduce(combine, list(A, C, B))
