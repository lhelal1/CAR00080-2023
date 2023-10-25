# importar dataset Infant

pacman::p_load(tidyverse,
               dplyr,
               ggplot2)

### Infant Dataset
unique(infant$IDNO)
unique(infant$MONTH) # min = 1, max = 9, factor
unique(infant$OUTCOME) # dummy no = 0, yes = 1
unique(infant$BIRTHWGT, na.rm = TRUE) # float min = 1000g, max = 4700g
unique(infant$GENDER) # factor 1 2
unique(infant$DIARRHEA) # dummy no = 0, yes = 1

### Isolando variáveis

id <- as.numeric(infant$IDNO)
month <- as.factor(infant$MONTH)
outcome <- as.factor(infant$OUTCOME)
bwg <- as.numeric(infant$BIRTHWGT)
diarr <- as.factor(infant$DIARRHEA)

### Distribuições de Frequências

# numéricas
hist(bwg)

# categóricas
table(outcome)
table(diarr)
table(month[outcome == 0])

### Distribuições de Probabilidades

hist(bwg/1458)

library(ggplot2)

# Dados simulados (neste exemplo, substitua por seus próprios dados)
successes <- c(30, 50, 70, 90, 110, 130, 150, 170, 190, 210)  # Número de sucessos
attempts <- 365  # Número de tentativas

# Função de verossimilhança (distribuição binomial)
likelihood_function <- function(params) {
  p_success <- params
  log_likelihood <- sum(dbinom(successes, size = attempts, prob = p_success, log = TRUE))
  return(-log_likelihood)  # Maximizar a verossimilhança é o mesmo que minimizar o negativo da log-verossimilhança
}

# Valores de probabilidade de sucesso para o gráfico
p_values <- seq(0, 1, by = 0.01)
likelihood_values <- sapply(p_values, likelihood_function)

# Crie um dataframe com os valores
likelihood_df <- data.frame(p_success = p_values, likelihood = likelihood_values)

# Crie o gráfico da função de verossimilhança
ggplot(likelihood_df, aes(x = p_success, y = likelihood)) +
  geom_line(color = "blue", size = 1) +
  labs(x = "Probabilidade de Sucesso", y = "Log-Verossimilhança") +
  ggtitle("Função de Verossimilhança") +
  theme_minimal()

