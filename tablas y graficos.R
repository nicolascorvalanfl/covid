library(readxl)
library(tidyverse)
covid <-read_excel("BASE COVID.xlsx")

variables<-c("covid$ravlt_total_z",
             "covid$ravlt_diferido_z",
             "covid$benson_diferido_z",
             "")

#convertir as.numeric()

covid$memoria<-(as.numeric(covid$ravlt_total_z)+
                  as.numeric(covid$ravlt_diferido_z)+
                  as.numeric(covid$benson_diferido_z))/3

covid$memoria_i<-covid$memoria<(-1)

covid$atencion<-(as.numeric(covid$tmt_a_z)+
               as.numeric(covid$digitos_directos_z))/2

covid$atencion_i<-covid$atencion<(-1)

covid$ejecutivo<-(as.numeric(covid$tmt_b_z)+
                    as.numeric(covid$digitos_inversos_z))/2

covid$ejecutivo_i<-covid$ejecutivo<(-1)


covid$lenguaje<-as.numeric(covid$mint_z)

covid$lenguaje_i<-covid$lenguaje<(-1)

covid$hads_depresion_dx<-covid$hads_depresion>7


a<-covid %>% select(hc,
                    memoria, 
                    atencion, 
                    ejecutivo, 
                    lenguaje, 
                    hads_depresion_dx)

str(a)
library(reshape2)

a<-melt(a, id=c("hc", "hads_depresion_dx"))

a$hads_depresion_dx<-as.factor(a$hads_depresion_dx)
ggplot(a, aes(x=variable, y=value, group=hc, color=hads_depresion_dx))+
  geom_line(alpha=0.5)+
  geom_hline(yintercept = -1)+
  scale_color_manual(values = c("#32374c","#fba31b"))+
  theme_minimal()




b<-covid %>% select(memoria_i, atencion_i, ejecutivo_i, lenguaje_i,hc)

base2 <- melt(b, id.var = "hc")



b<-as.data.frame(table(base2$variable, base2$value))


b<-melt(b, id="hc")


ggplot(b, aes(fill = Var2, y = Freq, x = Var1 )) +
  geom_col(position = "fill")+
  theme_minimal()
