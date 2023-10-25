##### Poisson

pacman::p_load(
    tidyverse,
    dplyr,
    ggplot2,
    sandwich,
    msm,
    epitools
)

# Subindo banco

p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
    prog <- factor(prog, levels=1:3, labels=c("General", "Academic",
                                              "Vocational"))
    id <- factor(id)
})

# Estatistica
summary(p)

with(p, tapply(num_awards, prog, function(x) {
    sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

# Plot
ggplot(p, aes(num_awards, fill = prog)) +
    geom_histogram(binwidth=.5, position="dodge")


### GLM Poisson
summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))

### Covariancia e outros
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est
with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


## Update modelo
m2 <- update(m1, . ~ . - prog)
## ANOVA
anova(m2, m1, test="Chisq")

# Define S
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)),
                 coef(m1), cov.m1)

## Exponencia B
rexp.est <- exp(r.est[, -3])

## Variancia Robusta
rexp.est[, "Robust SE"] <- s
rexp.est
(s1 <- data.frame(math = mean(p$math),
                  prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))

# Predicao
predict(m1, s1, type="response", se.fit=TRUE)

p$phat <- predict(m1, type="response")
p <- p[with(p, order(prog, math)), ]

## Plot
ggplot(p, aes(x = math, y = phat, colour = prog)) +
    geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
    geom_line(size = 1) +
    labs(x = "Math Score", y = "Expected number of awards")
