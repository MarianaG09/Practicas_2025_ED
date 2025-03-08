
# Librerías ---------------------------------------------------------------
library(dplyr)
library(data.table)
library(readxl)
library(ggplot2)
library(agricolae)
# Carga de datos ----------------------------------------------------------
datos<-fread(input="Data/CNA2014_ENCABEZADO_15.csv",
             sep=",") %>% 
select(COD_VEREDA,TIPO_UC,S05_TENENCIA,P_S5PAUTOS,P_S7P82,P_S7P84F,P_S7P85B) %>% 
filter(TIPO_UC==1) %>% 
mutate(S05_TENENCIA=as.character(S05_TENENCIA)) #Para cambiar el tipo de variable
str(datos)
glimpse(datos)
# Limpieza de datos ------------------------------------------------------------
t_homologacion_7<-readxl::read_excel(
  path = "Data/Tablasdehomologacion.xlsx",
  sheet = "Hoja2") %>% 
  mutate(S05_TENENCIA=as.character(S05_TENENCIA))
#Es importante tambien cambiar el tipo de variable en la tabla de homologación
# Para evitar problemas de compatibilidad "MUTATE".
str(t_homologacion_7)
datos_dep <- datos %>% 
  left_join(t_homologacion_7,by=c("S05_TENENCIA"="S05_TENENCIA")) %>% 
  select(Predominancia,P_S7P85B) %>% 
  na.omit()
str(datos_dep)

# TDF Cualitativa ---------------------------------------------------------

tdf_505_tenencia <- datos_dep %>% 
  group_by(Predominancia) %>% 
  summarise(n_i=n()) %>% #Frecuencia absoluta.
  arrange(desc(n_i)) %>%  #Ordenar los valores de forma descendente
  mutate(N_i=cumsum(n_i)) %>% #Frecuencia absoluta acumulada
  mutate(f_i=n_i/sum(n_i)) %>%  #Freccuencia relativa
  mutate(F_i=cumsum(f_i))
  
# FORMA RESUMIDA A TENER EN CUENTA******
tdf_505_tenencia <- datos_dep %>% 
  group_by(Predominancia) %>% 
  summarise(n_i=n()) %>% 
  arrange(desc(n_i)) %>%  
  mutate(N_i=cumsum(n_i),
         f_i=n_i/sum(n_i),
         F_i=cumsum(f_i)) 

# Gráfico
barplot(table(datos_dep$Predominancia))

# Usando esquisser PARA GRAFICOS GGPLOT
#install.packages("esquisse")
# library(esquisse)
# library(plotly)
# esquisse::esquisser(viewer = "browser")  

#Grafico de barras
ggplot(datos_dep) +
  aes(x = Predominancia) +
  geom_bar(fill = "#3D9FEB") +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(face = "bold"))

# Para mejorar las tablas revisar paquete DT, datatable investigar 
# pauqetes de R para mostrar tablas.
# Tener en cuenta que se evaluará todo el tema de las 

# TDF-VARIABLE CUANTI -----------------------------------------------------

#Número de clases
k=round(1+3.3*log10(nrow(datos_dep)))
k
#Rango
rango=max(datos_dep$P_S7P85B,na.rm=T)-
  min(datos_dep$P_S7P85B,na.rm=T)
rango
#Longitud
longitud=rango/k
longitud

#Cortes
cortes<-min(datos_dep$P_S7P85B,na.rm=T)+c(seq(0,k,1))*longitud
cortes

#TDF- Leche
tdf_P_S7P85B<-datos_dep %>% 
  mutate(P_S7P85B_c=as.factor(cut(P_S7P85B,
                       breaks=cortes,
                       levels=cortes,
                       include.lowest=T,
                       dig.lab=6))) %>% 
  group_by(P_S7P85B_c, .drop =F ,add=F) %>% 
  summarise(n_i=n()) %>% 
  mutate(N_i=cumsum(n_i),
         f_i=n_i/sum(n_i),
         F_i=cumsum(f_i),
         x_i=cortes[1:k]+longitud/2,
         c_i=abs(cortes[1:k]-cortes[2:(k+1)]),
         d_i=n_i/c_i)
