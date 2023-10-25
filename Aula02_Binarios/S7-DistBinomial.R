### Dist Binomial

# Seed
set.seed(10)

n <- 4     # Number of trials (population size)
s <- 2000  # Number of simulations
m <- c(20, 100, 500, 1000)
EX <- n*p
VarX <- n*p*(1-p)
Z_score <- matrix(NA, nrow = s, ncol = length(m))
for (i in 1:s){
    for (j in 1:length(m)){
        samp <- rbinom(n = m[j], size = n, prob = 0.05)
        sample_mean <- mean(samp)

        Z_score[i,j] <- (sample_mean-EX)/sqrt(VarX/m[j])
    }
}

# Distribuicao de Medias
par(mfrow=c(4,1))
for (j in 1:4){
    hist(Z_score[,j], xlim=c(-5,5),
         freq=FALSE, ylim=c(0, 0.5),
         ylab="Probability", xlab="",
         main=paste("Sample Size =", m[j]))

    x <- (seq(-4, 4, by=0.01))
    y <- dnorm(x)
    z <- as.numeric(x)
    lines(z, y, col="blue")
}
