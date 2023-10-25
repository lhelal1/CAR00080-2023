# MLE

library(ggplot2)

p <- 0.6 # probabilidade inicial
amostragem <- rbinom(n = 10, size = 1, prob = p) # amostragem aleatoria
table(amostragem)

mean(amostragem)

p_verossimilhanca <- function(obs, p){
    verossimilhanca <- 1
    # loopando por todas as observacoes
    for (i in 1:length(obs)){
        y <- obs[i]
        verossimilhanca <- verossimilhanca*(p^y*(1-p)^(1-y)) # funcao de distribuicao
    }
    return(verossimilhanca) # retorna o produto final
}

options(scipen=999)

# simulacoes
p_verossimilhanca(amostragem, 0.5)

p_verossimilhanca(amostragem, 0.6)

p_verossimilhanca(amostragem, 0.7)

p_vetor <- seq(0, 1, by = 0.001) # probabilidades possiveis

#plot
plot(x = p.seq, y = p_verossimilhanca(amostragem, p_vetor), type = "n",
     xlab = "p", ylab = "Likelihood")
lines(x = p_vetor, y = p_verossimilhanca(amostragem, p_vetor))


otimizacao <- optimize(f = p_verossimilhanca, # funcao de L a ser maximizada
                obs = amostragem,    # observacoes
                intervalo = c(0,1), # possiveis valores
                maximo = T)       # maximiza a funcao
otimizacao
