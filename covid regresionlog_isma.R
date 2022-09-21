library(tidyverse)
library(readxl)
library(gtsummary)

covid <-read_excel("BASE COVID.xlsx")



#REGRESIÓN LOGÍSTICA


modelo1 <- glm(dx_cog~severidad + 
                           hads_depresion + 
                           caide_total + 
                           hads_ansiedad, data=covid, family="binomial")

library(gtsummary)

summary(modelo1)

tbl_regression(modelo1, exponentiate=TRUE)

library(easystats)
report(modelo1)




library(sjPlot)
plot_model(modelo1, 
           colors = c("#32374c","#fba31b"),
           axis.lim=c(0.3,3), 
           show.p = TRUE,
           show.data = T,
           p.val = "profile",
           title = "Factores de riesgo de sintomas cognitivos post-covid (OR)")

check_model(modelo1)
check_collinearity(modelo1)
print(a)
