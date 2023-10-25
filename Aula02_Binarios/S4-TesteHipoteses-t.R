# Exemplo da Teoria da NHST e Teste t em R

# Carregando bibliotecas
library(ggplot2)
library(dplyr)

# Definindo dados de exemplo
set.seed(123)  # Definindo semente aleatória para reprodutibilidade
amostra_controle <- rnorm(30, mean = 100, sd = 15)  # Amostra do grupo de controle
amostra_tratamento <- rnorm(30, mean = 110, sd = 15)  # Amostra do grupo de tratamento

# Visualização dos dados com um gráfico de densidade
densidade_controle <- as.data.frame(density(amostra_controle))
densidade_controle <- as.data.frame(densidade_controle)
densidade_tratamento <- as.data.frame(density(amostra_tratamento))

## Visualização dos dados com um gráfico de densidade
ggplot() +
    geom_line(data = data.frame(x = densidade_controle$x, y = densidade_controle$y), aes(x = x, y = y), color = "blue", linetype = "solid") +
    geom_line(data = data.frame(x = densidade_tratamento$x, y = densidade_tratamento$y), aes(x = x, y = y), color = "red", linetype = "dashed") +
    labs(title = "Distribuição de Dados - Grupo de Controle vs. Grupo de Tratamento",
         x = "Valores", y = "Densidade") +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_minimal()


# Teste de hipóteses (t-test) para comparar os grupos
resultado_teste_t <- t.test(amostra_controle, amostra_tratamento)
resultado_teste_t

# Visualização dos grupos com um gráfico de barras
dados <- data.frame(Grupo = c("Controle", "Tratamento"),
                    Média = c(mean(amostra_controle), mean(amostra_tratamento)),
                    Erro_Padrão = c(sd(amostra_controle) / sqrt(length(amostra_controle)),
                                    sd(amostra_tratamento) / sqrt(length(amostra_tratamento))))

# Gráfico de barras
ggplot(data = dados, aes(x = Grupo, y = Média, fill = Grupo)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = Média - Erro_Padrão, ymax = Média + Erro_Padrão), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "Comparação das Médias entre os Grupos",
         x = "Grupo", y = "Média") +
    theme_minimal()

# Resultado do teste t
cat("\nResultado do Teste t:\n")
cat("Estatística t: ", resultado_teste_t$statistic, "\n")
cat("Graus de liberdade: ", resultado_teste_t$parameter, "\n")
cat("Valor-p: ", resultado_teste_t$p.value, "\n")

# Interpretação do teste t
if (resultado_teste_t$p.value < 0.05) {
    cat("\nConclusão: Rejeitamos a hipótese nula (H0) em favor da hipótese alternativa (H1).\n")
} else {
    cat("\nConclusão: Não temos evidência suficiente para rejeitar a hipótese nula (H0).\n")
}
