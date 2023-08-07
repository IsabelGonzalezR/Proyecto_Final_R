#Proyecto final R
#González Reyes Rocío Isabel

install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(ggplot2)

balances_2014 <- read_excel("Data/balances_2014.xlsx")
str(balances_2014)
file.choose()
sum(is.na(balances_2014))

#Limpiando la data

n_columnas <- ncol(balances_2014)
for (i in 1:n_columnas) {
Data_Balances_2014 <- balances_2014[!is.na(balances_2014[, i]), ]
}
dim(Data_Balances_2014)

str(Data_Balances_2014)
View(Data_Balances_2014)

#Parte 1 – Data:

#Columnas de interes ----
empresas<- as_tibble(Data_Balances_2014)

#Columnas requeridas
empresas<-mutate(empresas,Liquidez_corriente=v345/v539,
                 Endeudamiento_del_activo=v599/v499,
                 Endeudamiento_patrimonial=v599/v698,
                 Endeudamiento_del_Activo_Fijo=v698/v498,
                 Apalancamiento=v539/v499)

empresas <- select(empresas,nombre_cia , situacion, tipo, pais,
                   provincia, canton, ciudad, ciiu4_nivel1, ciiu4_nivel6,
                   Liquidez_corriente,Endeudamiento_del_activo, Endeudamiento_patrimonial,
                   Endeudamiento_del_Activo_Fijo,Apalancamiento)
#Renombrando columnas
empresas<- rename(empresas, Empresas = nombre_cia,
                  Status = situacion,
                  Tipo_de_empresa= tipo,
                  País= pais,
                  Provincia= provincia,
                  Cantón= canton,
                  Ciudad= ciudad,
                  Actividad_económica= ciiu4_nivel1,
                  Subactividad= ciiu4_nivel6)
str(empresas)
View(empresas)

#Tabla resumen del número total de empresas por actividad económica. 
empresas_por_actividad_economica <- read_xlsx("data/ciiu.xlsx")

empresas_por_actividad_economica %>% filter(CODIGO=="A" | CODIGO=="B" | CODIGO=="C"| CODIGO=="D"|
                             CODIGO=="E"|CODIGO=="F"|CODIGO=="G"|CODIGO=="H"|
                             CODIGO=="I"|CODIGO=="J"|CODIGO=="K"|CODIGO=="L"|
                             CODIGO=="M"|CODIGO=="N"|CODIGO=="O"|CODIGO=="P"|
                             CODIGO=="Q"|CODIGO=="R"|CODIGO=="S"|CODIGO=="T"|
                             CODIGO=="U"|CODIGO=="Z")

empresas_por_actividad_economica<-
    empresas_por_actividad_economica %>%select(CODIGO,DESCRIPCION)

empresas_act_econ<-empresas %>% select(Actividad_económica)

tabla1<-empresas_act_econ %>% group_by(Actividad_económica) %>%
  summarise(Ntotal_emp_Actividad_eco=n()) %>%
  left_join(empresas_por_actividad_economica ,by=c("Actividad_económica"="CODIGO"))
tabla1<-select(tabla1,Actividad_económica,DESCRIPCION,Ntotal_emp_Actividad_eco)

View(tabla1)

#Tabla resumen del número total de empresas por actividad económica por cada cantón.

tabla2<-empresas%>% group_by(Actividad_económica,Cantón) %>%
  summarise(Ntotal_empresas_ecoycanton=n()) %>%
  left_join(empresas_por_actividad_economica,by=c("Actividad_económica"="CODIGO"))

tabla2<-select(tabla2,Actividad_económica,DESCRIPCION,
               Cantón,Ntotal_empresas_ecoycanton)

view(tabla2)

#Indicadores financieros de liquidez y solvencia por Status y provincia.
# Liquidez_corriente
ggplot(empresas, aes(x =Provincia, y = Liquidez_corriente,fill=Status)) +
  geom_bar(stat = "summary", position = "stack")+
  labs(title = "Comparativo liquidez corriente según Estado y Provincia.",
       x = "Provincia", y = "Liquidez corriente") +
  theme(legend.title = element_text(4),
        legend.text=element_text(size = 4),
        legend.position = "bottom", legend.key.size = unit(0.3, "cm"),
        axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 4)
  )

#Endeudamiento del activo
ggplot(empresas, aes(x =Provincia, y = Endeudamiento_del_activo,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Comparativo Endeudamiento del activo por Status y Provincia",
       x = "Provincia", y = "Endeudamiento del activo") +
  theme(legend.title = element_text(size = 4),
        legend.text=element_text(size = 4),legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 4)
  )

#Endeudamiento patrimonial 
ggplot(empresas, aes(x =Provincia, y = Endeudamiento_patrimonial,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(
    title = "Comparativo Endeudamiento patrimonial por Status y Provincia",
    x = "Provincia", y = "Endeudamiento patrimonial") +
  theme(legend.title = element_text(size = 4),
        legend.text=element_text(size = 4),
        legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 4) 
  )

#Endeudamiento_del_Activo_Fijo
ggplot(empresas, aes(x =Provincia, y = Endeudamiento_del_Activo_Fijo,
                     fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Comparativo  Endeudamiento del Activo Fijo por Status y Provincia",
       x = "Provincia", y = "Endeudamiento del Activo Fijo") +
  theme(legend.title = element_text(size = 4),
        legend.text=element_text(size = 4),
        legend.position = "bottom", legend.key.size = unit(0.3, "cm"),
        axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 4) 
  )

#Apalancamiento
ggplot(empresas, aes(x =Provincia, y = Apalancamiento,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Comparativo Apalancamiento por Status y Provincia",
       x = "Provincia", y = "Apalancamiento") +
  theme(legend.title = element_text(size = 4),
        legend.text=element_text(size = 4),legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3) 
  )

#Comparativo de los indicadores financieros de liquidez y solvencia por tipo de
#   empresa.

ggplot(empresas, aes(x = Tipo_de_empresa)) +
  geom_line(aes(y = Liquidez_corriente, group = 1,
                color = "Liquidez Corriente"),
            stat = "summary", fun = "mean", position = "dodge", size = 1) +
  geom_point(aes(y = Liquidez_corriente, group = 1,
                 color = "Liquidez Corriente"),
             stat = "summary", fun = "mean", position = "dodge", size = 3) +
  geom_line(aes(y = Endeudamiento_del_activo, group = 1,
                color = "Endeudamiento del Activo"),
            stat = "summary", fun = "mean", position = "dodge", size = 1) +
  geom_point(aes(y = Endeudamiento_del_activo, group = 1,
                 color = "Endeudamiento del Activo"),
             stat = "summary", fun = "mean", position = "dodge", size = 3)+
  geom_line(aes(y = Endeudamiento_patrimonial, group = 1,
                color = "Endeudamiento Patrimonial"),
            stat = "summary", fun = "mean", position = "dodge", size = 1) +
  geom_line(aes(y = Endeudamiento_del_Activo_Fijo, group = 1,
                color = "Endeudamiento del Activo Fijo"),
            stat = "summary", fun = "mean", position = "dodge", size = 1) +
  geom_line(aes(y = Apalancamiento, group = 1,
                color = "Apalancamiento"),
            stat = "summary", fun = "mean", position = "dodge", size = 1) +
  geom_point(aes(y = Endeudamiento_patrimonial, group = 1,
                 color = "Endeudamiento Patrimonial"),
             stat = "summary", fun = "mean", position = "dodge", size = 3) +
  geom_point(aes(y = Endeudamiento_del_Activo_Fijo, group = 1,
                 color = "Endeudamiento del Activo Fijo"),
             stat = "summary", fun = "mean", position = "dodge", size = 3) +
  geom_point(aes(y = Apalancamiento, group = 1, color = "Apalancamiento"),
             stat = "summary", fun = "mean", position = "dodge", size = 3)+
  scale_color_manual(values = c("Liquidez Corriente" = "purple",
                                "Endeudamiento del Activo" = "red",
                                "Endeudamiento Patrimonial" = "orange",
                                "Endeudamiento del Activo Fijo" = "pink",
                                "Apalancamiento" = "magenta")) +
  theme_minimal() +
  labs(title = "Comparativo de los indicadores financieros de
liquidez y solvencia por tipo de empresa",
       x = "Tipo de Empresa", y = "Valor en Dólares",
       color = "Indicadores Financieros") + # Cambiar el título de la leyenda
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size=7,angle = 45, hjust = 1))


#Parte 2 – Preguntas de investigación:

#1.¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?


empresas$trab_direc<-Data_Balances_2014$trab_direc
empresas$tamanio<-Data_Balances_2014$tamanio
empresas$trab_admin<-Data_Balances_2014$trab_admin

Micro_pequeña<-empresas %>% select(tamanio,Endeudamiento_del_activo) %>%
  filter(tamanio=="PEQUEÑA" | tamanio=="MICRO")

Micro_pequeña_<-Micro_pequeña [ is.finite(Micro_pequeña$Endeudamiento_del_activo), ]
Endactivo_Micro_pequeña<-sum(Micro_pequeña_$Endeudamiento_del_activo, na.rm = TRUE)
Grande<-empresas %>% select(tamanio,Endeudamiento_del_activo) %>%
  filter(tamanio=="GRANDE")
Endactivo_Grande<-sum(Grande$Endeudamiento_del_activo, na.rm = TRUE)
Pregunta1<-data.frame(
  Tipo_empresa = c("Micro + Pequeñas", "Grandes"),
  Endeudamiento=c(Endactivo_Micro_pequeña,Endactivo_Grande)
)

print(Pregunta1)
       
#Respuesta: Esto indica que las micro y pequeñas empresas tienen el mayor ratio de 
#             endeudamiento en comparación con las empresa grandes.


#2.¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más de
#   60 trabajadores directos y que cuenta con 100 a 800 trabajadores administrativos?

Liquidez_tipodecompañia<-empresas %>% 
      select(Tipo_de_empresa,Liquidez_corriente,trab_direc) %>%
        group_by(Tipo_de_empresa) %>% filter(trab_direc>=60)

Liquidez_tipodecompañia_datos<-
  Liquidez_tipodecompañia[ is.finite(Liquidez_tipodecompañia$Liquidez_corriente), ]

Liquidez_tipodecompañia_datos_<-sum(Liquidez_tipodecompañia_datos$Liquidez_corriente)

Liquidez_tipodecompañia_100<-empresas %>% 
  select(Liquidez_corriente,trab_admin,Tipo_de_empresa) %>%
  group_by(Tipo_de_empresa)%>% filter(trab_admin >=100 & trab_admin <=800)

Liquidez_tipodecompañia_100_datos<- 
  Liquidez_tipodecompañia_100[ is.finite(Liquidez_tipodecompañia_100$Liquidez_corriente), ]
Pregunta2<-sum(Liquidez_tipodecompañia_100_datos$Liquidez_corriente)
Pregunta2_tab<-data.frame(
  Filtros=c("Mayores o igual a 60 Trabajadores directos",
            " De 100 a 800 trabajadores administrativos "),
  liquidez_x_compañía= c(Liquidez_tipodecompañia_datos_, Pregunta2)
)

print(Pregunta2_tab)



#3. Describe el top 10 de empresas con mayor apalancamiento.

Top_10_Apalancamiento<-empresas %>% select(Empresas,Apalancamiento)
Top_10_Apalancamiento_p<-
  Top_10_Apalancamiento[ is.finite(Top_10_Apalancamiento$Apalancamiento), ]


Top_en_orden<-Top_10_Apalancamiento_p %>% arrange(desc(Apalancamiento))
Top_10_Apalancamiento_p<-head(Top_en_orden,10)
print(Top_10_Apalancamiento_p)

ggplot(Top_10_Apalancamiento_p, aes(x = reorder(Empresas,Apalancamiento), y = Apalancamiento)) +
  geom_bar(stat = "identity", fill= "purple") +
  labs(title = "Top 10 de empresas con mayor apalancamiento",
       x = "Empresas", y = "Apalancamiento") +
  theme(axis.text.x = element_text(size=6,angle = 45, hjust = 1))


install.packages("knitr")
library (knitr)
install.packages("tinytex")
tinytex::install_tinytex()

