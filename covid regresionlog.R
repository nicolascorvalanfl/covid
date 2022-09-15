install.packages("knitr")
install.packages("tinytex")
install.packages("ipak")
install.packages("readlx")
install.packages("readxl")
install.packages("datos")
library(datos)
library(tidyverse)
library(readxl)
install.packages("rapportools")
library(rapportools)

covid <- BASE_COVID

#Demográficos

t.test(covid$edad~covid$dx)

t.test(covid$sexo~covid$dx)

t.test(covid$escolaridad~covid$dx)

with(covid,tapply(edad,dx,sd))

with(covid,tapply(sexo,dx,mean))

with(covid,tapply(escolaridad,dx,mean))


#REGRESIÓN LOGÍSTICA

modelo <- glm(dx_cog~severidad + 
                enfermedad_cardiaca + 
                hipertension + 
                pulmonar + 
                diabetes + 
                tuberculosis + 
                cancer + 
                fumador + 
                obesidad + 
                hads_depresion + 
                caide_total + 
                hads_ansiedad, data=covid, family="binomial")
summary(modelo)

modelo1 <- modelo <- glm(dx_cog~severidad + 
                           enfermedad_cardiaca + 
                           hipertension + 
                           pulmonar + 
                           diabetes + 
                           tuberculosis + 
                           cancer + 
                           fumador + 
                           obesidad + 
                           hads_depresion + 
                           caide_total + 
                           hads_ansiedad, data=covid, family="binomial")

install.packages("gtsummary")
library(gtsummary)

tbl_regression(modelo1, exponentiate=TRUE)




install.packages("OddsPlotty")
library(OddsPlotty)

OR <- odds_plot(modelo1,
                title = "Odds Ratios",
                subtitle = "Factores")

OR$odds_plot

library(forestplot)

foresplot(modelo1, mean, )