rm(list = ls())
cat("\014")

# Algoritmo Original
encontrar_par_original <- function(vetor, k) {
  n <- length(vetor)
  if (n < 2)
    return(c(-1, -1))
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (vetor[i] + vetor[j] == k) {
        return(c(i, j))
      }
    }
  }
  return(c(-1, -1))
}

#-------------------------------------------------------------

# Algoritmo Melhorado 
encontrar_par_melhorado <- function(vetor, k) {
  n <- length(vetor)
  if (n < 2)
    return(c(-1, -1))
  
  vetor_ordenado <- sort(vetor)
  esq <- 1
  dir <- n
  
  while (esq < dir) {
    soma <- vetor_ordenado[esq] + vetor_ordenado[dir]
    if (soma == k) {
      return(c(esq, dir))
    } else if (soma < k) {
      esq <- esq + 1
    } else {
      dir <- dir - 1
    }
  }
  
  return(c(-1, -1))
}

#-------------------------------------------------------------

# Parâmetros
n_elementos <- 100000
n_elementos_teste_rapido <- 10000  
k_alvo <- 10
n_rodadas <- 30
limite_superior_sample <- 1000

tempos_original <- numeric(n_rodadas)
tempos_melhorado <- numeric(n_rodadas)

cat("Iniciando a simulação...\n")

for (i in 1:n_rodadas) {
  vetor_teste <- sample(1:limite_superior_sample, n_elementos, replace = TRUE)
  vetor_teste_rapido <- vetor_teste[1:n_elementos_teste_rapido]
  
  tempo_ini <- system.time({
    resultado_original <- encontrar_par_original(vetor_teste_rapido, k_alvo)
  })
  tempos_original[i] <- tempo_ini["elapsed"]
  
  tempo_mel <- system.time({
    resultado_melhorado <- encontrar_par_melhorado(vetor_teste, k_alvo)
  })
  tempos_melhorado[i] <- tempo_mel["elapsed"]
}

#Gráfico 
media_tempo_original <- mean(tempos_original)
media_tempo_melhorado <- mean(tempos_melhorado)

cat("\n--- Análise Final ---\n")
cat(
  "Tempo médio (Original, n =",
  n_elementos_teste_rapido,
  "): ",
  round(media_tempo_original, 5),
  "s\n"
)
cat(
  "Tempo médio (Melhorado, n =",
  n_elementos,
  "): ",
  round(media_tempo_melhorado, 5),
  "s\n"
)
