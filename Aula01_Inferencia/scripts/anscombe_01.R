#### Quarteto de Anscombe ####

library(datasets)
data = datasets::anscombe
head(data)
glimpse(data)


library(ggplot2)
library(gridExtra)

sapply(data, mean)
sapply(data, var)
cor(data[,1:4],data[,5:8])

anova(lm1, lm2, lm3, lm4)
anova(lm2)
anova(lm3)
anova(lm4)

######
# dataset

library(readr)

dtAns <- read_csv("anscombe.csv")
head(dtAns)
select(dtAns, -c(...1))

# table 1
lm(formula = y ~ x, data = dtAns)


# summary stats
mean(dtAns$y[dtAns$set==1])
mean(dtAns$y[dtAns$set==2])
mean(dtAns$y[dtAns$set==3])
mean(dtAns$y[dtAns$set==4])
mean(dtAns$y[dtAns$set==5])
mean(dtAns$y[dtAns$set==6])
mean(dtAns$y[dtAns$set==7])


var(dtAns$y[dtAns$set==1])
var(dtAns$y[dtAns$set==2])
var(dtAns$y[dtAns$set==3])
var(dtAns$y[dtAns$set==4])
var(dtAns$y[dtAns$set==5])
var(dtAns$y[dtAns$set==6])
var(dtAns$y[dtAns$set==7])


min(dtAns$y[dtAns$set==1])
min(dtAns$y[dtAns$set==2])
min(dtAns$y[dtAns$set==3])
min(dtAns$y[dtAns$set==4])
min(dtAns$y[dtAns$set==5])
min(dtAns$y[dtAns$set==6])
min(dtAns$y[dtAns$set==7])


unique(dtAns$y[dtAns$set==1])
unique(dtAns$y[dtAns$set==2])
unique(dtAns$y[dtAns$set==3])
unique(dtAns$y[dtAns$set==4])
unique(dtAns$y[dtAns$set==5])
unique(dtAns$y[dtAns$set==6])
unique(dtAns$y[dtAns$set==7])

isTRUE(dtAns$y[dtAns$set==1] == dtAns$y[dtAns$set==2])


ggplot(get(paste0("xy",i)), aes(x=x,y=y)) +
    xlab(NULL) + ylab(NULL) + geom_point(size=2) + ggtitle(i)+
    geom_smooth(method='lm', se=FALSE)  + theme_void() + theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.5))
)


####

################################################################

install.packages("datagenerator")

datagenerator <- function(n = 50, meanx = 9, varX = 11, b0 = 3, b1 = .5, corXY = .8,
                          xFUN = function(n) rnorm(n),
                          uFUN = function(x) rnorm(length(x))) {

    # Mean y and variance y have to be set with some care as they are dependent upon x to some extent
    # since population mean(y) = b0 + b1 * mean(x) + mean(u) = b0 + b1*meanx
    # variance(y) = b1^2 * variance(x) + variance(u) + 2*cov(x,u)

    # So we know that
    # meany     = b0+b1*meanx

    SSE <- (b1^2 * varX)*n
    # SST <- SSE + SSU

    # SSU = SSE/corXY - SSE
    # varianceU = SSU/n
    # varianceU = (SST - SSE)/n
    # Since corXY^2 = (SSE/SST)
    # SST =  SSE/corXY^2
    varianceu = (SSE/corXY^2 - SSE)/n

    # variancey = b1^2 * varX + varianceu

    # First off let us try to generate our data
    x <- xFUN(n)

    # Adjust x to have meanx and varX
    a <- (varX/var(x))^.5
    x <- a*(x-mean(x)) + meanx
    mean(x) ; var(x)

    # if (is.na(set2)) set2 <- set

    u <- uFUN(x)

    # Adjust u to be uncorrelated with x
    cfux <- coef(lm(u~x))
    u <- u - cfux[1] - cfux[2]*x

    # Adjust u to have varianceu
    a <- (varianceu/var(u))^.5
    u <- a*(u-mean(u))

    y = b0 + b1*x + u

    xy <- data.frame(x,y)
}

################################################################

library(ggplot2)

# Anscombe Quartet
xy1 <- datagenerator()
xy2 <- datagenerator(xFUN = function(n) 1:n,
                     uFUN = function(x) {n <- length(x); sinpi(pi/4*(0:(n-1))/(n-1))})
xy3 <- datagenerator(xFUN = function(n) c(rep(0,n-1),1))
xy4 <- datagenerator(xFUN = function(n) sort(rnorm(n), decreasing=TRUE),
                     uFUN = function(x) c(x[1]*(-5), x[2] , x[-(1:2)]*(-5)))


# Generate a situation were one outlier can completely misrepresent the relationships in the data
xy5 <- datagenerator(xFUN = function(n) c(2*n,(n-1):1),
                     uFUN = function(x) c(x[1] * 50, x[-1] * 0))

# An alternative function that looks like a quardratic but is not
xy6 <- datagenerator(uFUN = function(x) -log(x))

# Heteroskedastic data
xy7 <- datagenerator(xFUN = function(n) 1:n, uFUN = function(x) (rnorm(length(x)))*rank(x))

# Heteroskedastic data
xy8 <- datagenerator(xFUN = function(n) rbinom(n,1,.5))


xy <- cbind(xy1,xy2,xy3,xy4,xy5,xy6, xy7, xy8)

apply(xy, 2,  mean); apply(xy, 2,  var)

summary(lm(y~x, data=xy1))
summary(lm(y~x, data=xy2))
summary(lm(y~x, data=xy3))
summary(lm(y~x, data=xy4))
summary(lm(y~x, data=xy5))
summary(lm(y~x, data=xy6))
summary(lm(y~x, data=xy7))
summary(lm(y~x, data=xy8))

for (i in 1:8)
    assign(paste0("s",i),
           ggplot(get(paste0("xy",i)), aes(x=x,y=y)) +
               xlab(NULL) + ylab(NULL) + geom_point(size=2) + ggtitle(i)+
               geom_smooth(method='lm', se=FALSE)  + theme_void() + theme(legend.position="none") +
               theme(plot.title = element_text(hjust = 0.5))
    )


setwd("Z:/Dropbox/Econometrics by Simulation/2019_03_March")
xylong <- cbind(set = rep(1:8,each=30), rbind(xy1,xy2,xy3,xy4,xy5,xy6,xy7)

write_csv(xylong, "Anscome.csv")

library(gridExtra)

grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8, ncol=2)

png("Anscome.png", width=500, height=600)
grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8, ncol=2)
dev.off()
################################################################


# Anscombe Quartet
xy1 <- datagenerator(b1 = -.5)
xy2 <- datagenerator(b1 = -.5, xFUN = function(n) 1:n,
                     uFUN = function(x) {n <- length(x); sinpi(pi/4*(0:(n-1))/(n-1))})
xy3 <- datagenerator(b1 = -.5, xFUN = function(n) c(rep(0,n-1),1))
xy4 <- datagenerator(b1 = -.5, xFUN = function(n) sort(rnorm(n), decreasing=TRUE),
                     uFUN = function(x) c(x[1]*(-5), x[2] , x[-(1:2)]*(-5)))


# Generate a situation were one outlier can completely misrepresent the relationships in the data
xy5 <- datagenerator(b1 = -.5, xFUN = function(n) c(2*n,(n-1):1),
                     uFUN = function(x) c(x[1] * 50, x[-1] * 0))

# An alternative function that looks like a quardratic but is not
xy6 <- datagenerator(b1 = -.5, uFUN = function(x) -log(x))

# Heteroskedastic data
xy7 <- datagenerator(b1 = -.5, xFUN = function(n) 1:n, uFUN = function(x) (rnorm(length(x)))*rank(x))

# Heteroskedastic data
xy8 <- datagenerator(b1 = -.5, xFUN = function(n) rbinom(n,1,.5))

xy <- cbind(xy1,xy2,xy3,xy4,xy5,xy6, xy7, xy8)

apply(xy, 2,  mean); apply(xy, 2,  var)

summary(lm(y~x, data=xy1))
summary(lm(y~x, data=xy2))
summary(lm(y~x, data=xy3))
summary(lm(y~x, data=xy4))
summary(lm(y~x, data=xy5))
summary(lm(y~x, data=xy6))
summary(lm(y~x, data=xy7))
summary(lm(y~x, data=xy8))

for (i in 1:8)
    assign(paste0("s",i),
           ggplot(get(paste0("xy",i)), aes(x=x,y=y)) +
               xlab(NULL) + ylab(NULL) + geom_point(size=2) + ggtitle(i)+
               geom_smooth(method='lm', se=FALSE)  + theme_void() + theme(legend.position="none") +
               theme(plot.title = element_text(hjust = 0.5))
    )


setwd("Z:/Dropbox/Econometrics by Simulation/2019_03_March")
xylong <- cbind(set = rep(1:7,each=30), rbind(xy1,xy2,xy3,xy4,xy5,xy6,xy7))

write.csv(xylong, "Anscome2.csv")

library(gridExtra)

xy1 |>
    ggplot(aes(x=x, y=y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

xy2 |>
    ggplot(aes(x=x, y=y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

xy3 |>
    ggplot(aes(x=x, y=y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

xy4 |>
    ggplot(aes(x=x, y=y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

xy5 |>
    ggplot(aes(x=x, y=y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

xy6 |>
    ggplot(aes(x=x, y=y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

xy7 |>
    ggplot(aes(x=x, y=y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)



grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8, ncol=2)

