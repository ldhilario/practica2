setwd("/home/luis/Escritorio/jovito")

# Cargamos los ficheros

SELLIN <- read.csv2("sell-in.csv")
SELLOUT <- read.csv2("sell-out.csv")

# Añadimos a sellout los campos conocidos presentes en sellin
DATOS_UNIDOS <- merge(SELLIN,SELLOUT)

# Visualizamos algunos datos para ver la estructura del fichero
head(DATOS_UNIDOS)

# Ejecutamos el test de valores ausentes para verificar que los 
# datos están completos

sapply(DATOS_UNIDOS, function(x)(sum(is.na(x)))) 

# Ejecutamos algunos test básicos de estadística descriptiva
summary(DATOS_UNIDOS)

# Ejecutamos un resumen agrupado por provincias

library(abind, pos=17)
library(e1071, pos=18)

numSummary(DATOS_UNIDOS[,c("Resto_Farmacos", "TOTAL_VOL", 
                            "Uds_AH", "Uds_DNT", "Uds_VH"), drop=FALSE], 
            groups=DATOS_UNIDOS$PROVINCIA, statistics=c("mean", "sd", "IQR", 
                                                        "quantiles"), quantiles=c(0,.25,.5,.75,1))
with(DATOS_UNIDOS, Barplot(PROVINCIA, xlab="PROVINCIA", ylab="Frequency"))
# Representamos gráficamente cada observación
plot(DATOS_UNIDOS$Uds_AH)
plot(DATOS_UNIDOS$Uds_DNT)
plot(DATOS_UNIDOS$Uds_VH)

# Comprobación de normalidad
qqnorm(DATOS_UNIDOS$Uds_AH)
qqline(DATOS_UNIDOS$Uds_AH)
shapiro.test(DATOS_UNIDOS$Uds_AH)

with(DATOS_UNIDOS, hist(Uds_AH, scale="frequency", breaks="Sturges",
                        col="darkgray"))
with(DATOS_UNIDOS, hist(Uds_DNT, scale="frequency", 
                        breaks="Sturges", col="darkgray"))
with(DATOS_UNIDOS, hist(Uds_VH, scale="frequency", breaks="Sturges",
                        col="darkgray"))

cor(DATOS_UNIDOS[,c("Resto_Farmacos","TOTAL_VOL","Uds_AH","Uds_DNT",
                    "Uds_VH")], use="complete")

MODELO <- lm(TOTAL_VOL~Uds_AH + Uds_DNT + Uds_VH + Resto_Farmacos, data = DATOS_UNIDOS) 


coefficients(MODELO)
summary(MODELO)

MODELO2 <- lm(TOTAL_VOL~Uds_DNT + Uds_VH + Resto_Farmacos, data = DATOS_UNIDOS) 

coefficients(MODELO2)
summary(MODELO2)

local({
  .x <- seq(0.005, 6.025, length.out=1000)  
  plotDistr(.x, df(.x, df1=3, df2=434), cdf=FALSE, xlab="x", ylab="Density",
            main=paste("F Distribution:  Numerator df = 3, Denominator df = 434"))
})

plot(MODELO2$residuals) 

crPlots(MODELO2, span=0.5)

avPlots(MODELO2, id.method="mahal", id.n=1)

