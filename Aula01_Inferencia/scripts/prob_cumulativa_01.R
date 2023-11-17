### Probabilidade Cumulativa Plot 1

# libraries
pacman::p_load(tidyverse,
               ggplot2,
               dplyr,
               visualize,
               pander)


# data cumulative plot
set.seed(19104)
dias <- seq(1,160,1)
volume_chuva <- seq(0,200,5)
prob_sim <- rep(NA, length(volume_chuva))
prob_math <- rep(NA, length(volume_chuva))

for(j in 1:length(volume_chuva)){
    result <- rep(NA, 10000)
    for(i in 1:10000){
        s <- sample(dias, volume_chuva[j], replace=T)
        result[i] <- anyDuplicated(s)
    }
    prob_sim[j] <- 1-(choose(365, volume_chuva[j])*factorial(volume_chuva[j]))/365^volume_chuva[j]
    prob_math[j] <- 1-(choose(160, volume_chuva[j])*factorial(volume_chuva[j]))/160^volume_chuva[j]
}

# plot com R base
{
    plot(volume_chuva, prob_math, col="dodgerblue", type="b", xlab="Dias acumulados",
         ylab="Probabilidade de Preciptação", pch=20, range(0,126))
    legend("topleft", c("Teórica"), pch=c(20,20), col=c("dodgerblue"))
    title("Função de Probabilidade Teórica vs. Observada - em R Base")
}

{
    points(volume_chuva, prob_sim, col="firebrick", type = "b", pch = 20, range(0,126))
    lines(volume_chuva, prob_sim, col="firebrick", type = "b", pch=20)
    legend("topleft", c("Teórica","Observada"), pch=c(20,20), col=c("dodgerblue", "firebrick"))
}


{
    abline(v = 20, lty = "dashed")
    abline(h = 0.41, lty = "dashed")
    abline(h = 0.71, lty = "dashed")
}
