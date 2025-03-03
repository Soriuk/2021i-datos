# datos hidrol�gicos ejercicio explorativo

inp <- read.csv("FDC.csv", na.strings="")

head(inp)
dim(inp)

inp[!complete.cases(inp),]

# Gr�fico de caudales de los rios en estudio.
plot(inp[,2],
     main= "Caudales en el tiempo de los Rios Banano y Estrella",
     type = "l",
     col="blue",
     xlab = "Fecha",
     ylab = "Caudal en ml/d�a"
     )
lines(inp[,3],
      col= "green")

summary(inp[,2:3])

## Resultado de la comparativa estad�stica de ambos rios

#  Pandora.mm..637km2. Banano.mm..90km2.
# Min.   :  0.180     Min.   :  2.43   
# 1st Qu.:  2.390     1st Qu.:  6.88   
# Median :  3.590     Median : 10.18   
# Mean   :  5.459     Mean   : 14.28   
# 3rd Qu.:  5.900     3rd Qu.: 15.46   
# Max.   :140.650     Max.   :384.00

# Histograma representativo del rio Estrella
hist(inp[,2],
     main = "Histograma del rio Estrella",
     xlab= "Rango absoluto (ml/d�a)",
     ylab = "Frecuencia",
     col= "blue"
     )
# Los datos del rio Estrella representan un caudal bajo en su mayor�a.

# Histograma representativo del rio Banano
hist(inp[,3],
     main = "Histograma del rio Banano",
     xlab = "Rango absoluto (ml/d�a)",
     ylab = "Frecuencia",
     col= "green"
     )
# Los datos del rio Banano representan un mayor caudal en comparaci�n al rio Estrella

# Mejor visualizaci�n de los datos
# Adjudicando nombres:

names(inp) <- c("fecha", "Estrella", "Banano")
attach(inp)

Tempdate <- strptime(inp[,1], format = "%d/%m/%Y")

MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN=sum)
write.csv(MAQ_Estrella, file = "MAQ.csv")

MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%Y"), FUN=sum)
write.csv(rbind(MAQ_Banano, MAQ_Estrella), file = "MAQrbind.csv")

# Gr�fico de acumulados anuales
plot(MAQ_Banano,
     main = "Gr�fico comparativo de acumulados anuales entre el Rio Banano (puntos) y Rio Estrella (L�nea)",
     ylim=c(100,3000),
     ylab = "Comparativa Rio Banano y Rio Estrella",
     xlab = "Periodo de 10 años (1973-1983)",
     col= "green"
     )
lines(MAQ_Estrella, 
      col=4,
      )
# Con dicho gr�fico se pueden identificar los "picos" en los caudales de cada r�o,
# en el a�o de 1975 para el Rio Banano y 1976 para el Rio Estrella, siendo estos a�os de fenomeno del ni�o en CR. 


# Gr�fico de acumulados mensuales

MMQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN=sum)
MMQ_Banano <- tapply(Banano, format(Tempdate, format = "%m"), FUN=sum)

plot(MMQ_Banano,
     main = "Gr�fico comparativo de acumulados Mensuales entre el Rio Banano (puntos) y Rio Estrella (L�nea)",
     ylim=c(100,3000),
     ylab = "Comparativa Rio Banano y Rio Estrella",
     xlab = "Periodo de 12 meses",
     col= "green"
)
lines(MMQ_Estrella, 
      col=4,
)
# An�lisis de correlaci�n

corinp <- cor(inp[,2:3], method = "spearman")

# LLuvia de datos entre ambos rios
plot(Estrella, Banano)

inp.lm <- lm(Estrella ~ Banano, data=inp)
summary(inp.lm)
plot(inp.lm)
 # Con el �ltimo plot se pueden observar los datos del summary en diferentes gr�ficos y realizar un analisis explorativo de cada uno.













