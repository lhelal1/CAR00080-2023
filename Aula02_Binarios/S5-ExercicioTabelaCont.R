###### EXERCICIO 1



# Carregue a biblioteca necessária
library(ggplot2)

# Crie uma tabela de contingência de exemplo
dados <- data.frame(
    Sexo = c("Masculino", "Masculino", "Feminino", "Feminino"),
    Compra = c("Sim", "Não", "Sim", "Não")
)

# Exiba a tabela de contingência
table(dados$Sexo, dados$Compra)

# Realize o teste qui-quadrado de independência
resultado_teste <- chisq.test(table(dados$Sexo, dados$Compra))

# Imprima o resultado do teste
cat("Resultado do Teste Qui-Quadrado:\n")
print(resultado_teste)

# Exiba a tabela de contingência observada
tab_contingencia <- table(dados$Sexo, dados$Compra)
cat("Tabela de Contingência Observada:\n")
print(tab_contingencia)

# Exiba a tabela de contingência esperada sob a hipótese nula de independência
cat("Tabela de Contingência Esperada (Hipótese Nula):\n")
print(resultado_teste$expected)

# Exiba o valor-p
cat("Valor-p:\n")
print(resultado_teste$p.value)

# Interpretação do teste
if (resultado_teste$p.value < 0.05) {
    cat("\nConclusão: Rejeitamos a hipótese nula (H0). Há evidências de uma associação entre Sexo e Compra.\n")
} else {
    cat("\nConclusão: Não temos evidências suficientes para rejeitar a hipótese nula (H0). Não há associação significativa entre Sexo e Compra.\n")
}

# Visualização dos resultados com um gráfico de barras
ggplot(data = dados, aes(x = Sexo, fill = Compra)) +
    geom_bar(position = "fill") +
    labs(title = "Gráfico de Barras: Sexo vs. Compra",
         x = "Sexo", y = "Proporção") +
    theme_minimal()


