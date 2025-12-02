rm(list = ls())
cat("\014")

calculaEficiencia <- function(A, B) {
  if (B < A) {
    resultado <- (A - B) / B
    cat("O algoritmo B e", resultado, "vezes mais eficiente que A\n")
    
  } else if (B > A) {
    resultado <- (B - A) / A
    cat("O algoritmo B e", resultado, "vezes menos eficiente que A\n")
    
  } else {
    cat("Eles são iguais\n")
  }
}
#----------------------------------------------------------------------
cat(" Execução 1 \n")
n1 <- 1000
A1 <- n1^3
B1 <- n1^2
cat("A:", A1, "| B:", B1, "\n")
calculaEficiencia(A1, B1)
#----------------------------------------------------------------------
cat("\n Execução 2 \n")
n2 <- 1000
A2 <- n2 * 2 * log2(n2)
B2 <- log2(n2)
cat("A:", A2, "| B:", B2, "\n")
calculaEficiencia(A2, B2)
#----------------------------------------------------------------------
cat("\n Execução 3 \n")
n3 <- 5
A3 <- log2(n3)
B3 <- factorial(n3)
cat("A:", A3, "| B:", B3, "\n")
calculaEficiencia(A3, B3)
#----------------------------------------------------------------------
cat("\n Execução 4 \n")
n4 <- 1000
A4 <- n4 * log2(n4)
B4 <- 1
cat("A:", A4, "| B:", B4, "\n")
calculaEficiencia(A4, B4)
#----------------------------------------------------------------------
cat("\n Execução 5 \n")
n5 <- 5
A5 <- factorial(n5)
B5 <- factorial(n5)
cat("A:", A5, "| B:", B5, "\n")
calculaEficiencia(A5, B5)
#----------------------------------------------------------------------
cat("\n Execução 6 \n")
n6 <- 1000
A6 <- 1 / n6
B6 <- n6
cat("A:", A6, "| B:", B6, "\n")
calculaEficiencia(A6, B6)
#----------------------------------------------------------------------
cat("\n Execução 7 \n")
n7 <- 5
A7 <- 1
B7 <- factorial(n7)
cat("A:", A7, "| B:", B7, "\n")
calculaEficiencia(A7, B7)