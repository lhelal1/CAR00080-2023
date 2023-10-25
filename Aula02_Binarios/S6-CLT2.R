### CLT

data <-read.csv("dataset_clt.csv")

dim(data)
head(data,10)

# Media Pop
mean(data$Wall.Thickness)

# Plot Histograma
hist(data$Wall.Thickness, col = "pink",main = "Histogram for Wall Thickness", xlab = "wall thickness")
abline(v=12.8,col="red",lty=1)

# Ajuste
pdf("histogram.pdf", width = 6, height = 4)  # Define o tamanho do gráfico
hist(data$Wall.Thickness, col = "pink", main = "Histogram for Wall Thickness", xlab = "Wall Thickness")
dev.off()


