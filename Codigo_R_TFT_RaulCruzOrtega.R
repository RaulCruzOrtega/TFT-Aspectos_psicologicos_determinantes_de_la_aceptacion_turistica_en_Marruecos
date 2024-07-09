library(tidyverse)
library(cluster) #Clustring
library(fclust) #Fuzzy Clustering
library(factoextra) #extraer y visualizar resultados de análisis de datos multivariados (fviz_nbclust)
library(cSEM) #c-sem 
library(ggplot2)
library(readxl) #Lectura de xlsx
library(corrplot) #Presentar la matriz de correlación
library(clustMixType) #Kproto
library(NbClust)
library(kableExtra) #kable
library(plotly) #Graficas 3d
library(GGally) #GGPAIRS
library(FactoMineR) #HCPC y PCA
library(clValid) #Dunn Kmodes
library(klaR) #Kmodes
library(geocmeans) #Fukuyama and Sugeno


#Realizamos la carga de datos
BD <- read_excel("C:/Users/RaulCruz/Desktop/ULPGC/TFT/BD_TFT.xlsx")
View(BD)

#Eliminamos las filas las cuales contienen datos NA 
df <- na.omit(BD)

#Seleccionamos los datos que será utilizados en el estudio
datos <- data.frame(
  NRC1 = df$v030,
  NRC2 = df$v032,
  NRC3 = df$v033,
  TF1 = df$v035,
  TF2 = df$v036i,
  TF3 = df$v037,
  CTL1 = df$v063,
  CTL2 = df$v064,
  CTL3 = df$v065,
  CTL4 = df$v066,
  CTL5 = df$v068,
  CTL6 = df$v069,
  Desapego_1 = df$v114,
  Desapego_2 = df$v115,
  Desapego_3 = df$v117,
  Insatisfaccion_1 = df$v119,
  Insatisfaccion_2 = df$v120,
  Insatisfaccion_3 = df$v121,
  Disimilitud_1 = df$v122,
  Disimilitud_2 = df$v123,
  Disimilitud_3 = df$v124,
  Edad = df$v176,
  Genero = df$v177
)
View(datos)


################################## PCA #########################################

#Obtenemos los datos a los que tenemos que aplicar el Análisis de Componentes Principales (PCA)
Desapego <- data.frame(datos[13:15])
Insatisfaccion <- data.frame(datos[16:18])
Disimilitud <- data.frame(datos[19:21])

#-----------------------------Desapego Social-----------------------------------
#Realizamos el Análisis de Componentes Principales obligando a obtener una única dimesión
PCA_Desapego <- PCA(X = Desapego, ncp=1, scale.unit = TRUE)

summary(PCA_Desapego)

PCA_Desapego <- PCA_Desapego$ind$coord
PCA_Desapego <- round((PCA_Desapego - min(PCA_Desapego))/(max(PCA_Desapego)-min(PCA_Desapego)),4)
Desapego$PCA_Desapego <- PCA_Desapego
corrplot(cor(Desapego), method = "shade", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black",
)

#-----------------------------Insatisfaccion Social-----------------------------
#Realizamos el Análisis de Componentes Principales obligando a obtener una única dimesión
PCA_Insatisfaccion <- PCA(X = Insatisfaccion, ncp = 1, scale.unit = TRUE)

summary(PCA_Insatisfaccion)

PCA_Insatisfaccion <- PCA_Insatisfaccion$ind$coord
PCA_Insatisfaccion <- round((PCA_Insatisfaccion - min(PCA_Insatisfaccion))/(max(PCA_Insatisfaccion)-min(PCA_Insatisfaccion)),4)
Insatisfaccion$PCA_Insatisfac <- PCA_Insatisfaccion
corrplot(cor(Insatisfaccion), method = "shade", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black",
)

#-----------------------------Disimilitud Social--------------------------------
#Realizamos el Análisis de Componentes Principales obligando a obtener una única dimesión
PCA_Disimilitud <- PCA(X = Disimilitud, ncp = 1, scale.unit = TRUE)

summary(PCA_Disimilitud)

PCA_Disimilitud <- PCA_Disimilitud$ind$coord
PCA_Disimilitud <- round((PCA_Disimilitud - min(PCA_Disimilitud))/(max(PCA_Disimilitud)-min(PCA_Disimilitud)),4)
Disimilitud$PCA_Disimil <- PCA_Disimilitud
corrplot(cor(Disimilitud), method = "shade", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black",
)

################################### FIN PCA ####################################

################################## Clustering ##################################
#-------------------------------Datos a clusterizar-----------------------------

#----Variables Continuas----
dt_clusterizacion <- data.frame(
  Desapego = PCA_Desapego,
  Insatisfaccion  = PCA_Insatisfaccion,
  Disimilitud = PCA_Disimilitud
)
names(dt_clusterizacion) <- c('Desapego','Insatisfaccion', 'Disimilitud')
View(dt_clusterizacion)

#----Variables Categóricas----
procesar_edades <- function(dta) {
  result_array <- numeric(length(dta))  # Use character instead of numeric
  for (i in 1:length(dta)) {
    if (dta[i] < 30) {
      result_array[i] <- 1
    }
    else if (dta[i] >= 30 & dta[i] <= 44) {
      result_array[i] <- 2
    }
    else if (dta[i] >= 45) {
      result_array[i] <- 3
    }
  }
  return(result_array)
}

dt_clusterizacion_categorica <- data.frame(
  Edad = datos$Edad,
  Genero = datos$Genero
)
names(dt_clusterizacion_categorica) <- c('Edad','Genero')
#Procesamos las edades
dt_clusterizacion_categorica$Edad <- procesar_edades(dt_clusterizacion_categorica$Edad)
View(dt_clusterizacion_categorica)

#-------------------------------Número de clusters------------------------------

#Clusterización Jerárquica
HCPC(dt_clusterizacion, kk=Inf, min=2, max=6, consol=TRUE)

################### Kmeans ###################
fviz_nbclust(dt_clusterizacion, kmeans, method = "silhouette") +
  ggtitle("Evaluación del Número Óptimo de Clústeres - Silhouette") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold")) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "black")+ 
  geom_point(color = "black")

fviz_nbclust(dt_clusterizacion, kmeans, method = "wss") +
  ggtitle("Evaluación del Número Óptimo de Clústeres - Codo") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold")) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "black")+ 
  geom_point(color = "black")

fviz_nbclust(dt_clusterizacion, kmeans, method = "gap_stat") +
  ggtitle("Evaluación del Número Óptimo de Clústeres - Gap") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))+
  geom_vline(xintercept = 3, linetype = "dashed", color = "black")+
  geom_point(color = "black")

################### Kmedoids ###################
fviz_nbclust(dt_clusterizacion, pam, method = "silhouette") +
  ggtitle("Evaluación del Número Óptimo de Clústeres - Silhouette") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold")) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "black")+ 
  geom_point(color = "black")

fviz_nbclust(dt_clusterizacion, pam, method = "wss") +
  ggtitle("Evaluación del Número Óptimo de Clústeres - Codo") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold")) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "black")+ 
  geom_point(color = "black")

fviz_nbclust(dt_clusterizacion, pam, method = "gap_stat") +
  ggtitle("Evaluación del Número Óptimo de Clústeres - Gap") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold")) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "black")+ 
  geom_point(color = "black")

#----------------------------------Clusterización-------------------------------

#################### Kmeans ####################
#!!!!!Ejecutar Kmeans junto con la semilla!!!!!
set.seed("2345")
km <- kmeans(dt_clusterizacion, 2)
km
plot_ly(x=~dt_clusterizacion$Desapego, y=~dt_clusterizacion$Insatisfaccion, 
        z=~dt_clusterizacion$Disimilitud, type = 'scatter3d',
        color = ~factor(km$cluster) ) %>% 
        layout(
        title = 'Kmeans 2 Clusters',
        scene = list(
        xaxis = list(title = 'Desapego'),
        yaxis = list(title = 'Insatisfacción'),
        zaxis = list(title = 'Disimilitud')),
        legend = list(
          title = list(text = 'Cluster'),
          font = list(size = 20),
          x = 1,   # Posiciona la leyenda al extremo derecho
          y = 0.5  # Centra la leyenda verticalmente
          )
        )
ggpairs(dt_clusterizacion, aes(color = as.factor(km$cluster), alpha = 0.5), title = "Kmeans 2 Clusters")
#El panel superior mostrará la relación entre las variables continuas, el panel inferior 
#mostrará sus diagramas de dispersión, la diagonal mostrará sus diagramas de densidad
datos_y_resultados_kmeans <- cbind(dt_clusterizacion, Cluster = km$cluster)
datos_y_resultados_kmeans

#Representación distribución entre Clusters
distribucion <- as.data.frame(prop.table(table(datos_y_resultados_kmeans$Cluster))* 100)
names(distribucion) <- c("Cluster", "Porcentaje")
ggplot(distribucion, aes(x = "", y = Porcentaje, fill = Cluster)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + 
  theme_void() +
  geom_text(aes(label = sprintf("%.2f%%", Porcentaje)), position = position_stack(vjust = 0.5)) +
  labs(fill = "Cluster", title = "Distribución Porcentual entre Clusters")+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 12, face = "bold")
    )

#Representación de caja y violín - cluster 1
data_long <- reshape2::melt(datos_y_resultados_kmeans[datos_y_resultados_kmeans$Cluster == 1,  1:3])

ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(alpha = 0.5, adjust = 1.5) + # Ajustar la transparencia y anchura de los violines
  geom_boxplot(outlier.color = "red", outlier.shape = 1, width = 0.2, alpha = 0.5, position = position_dodge(0.9)) + # Ajustar la transparencia y anchura de los boxplots
  scale_fill_brewer(palette = "Pastel1") + # Paleta de colores para los violines y boxplots
  labs(x = "", y = "", fill = "Variable", title = "Perfil del Cluster 1 - Kmeans: Desapego, Insatisfacción y Disimilitud") +
  theme_minimal() +
  ylim(0, 1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), # Centrar y poner en negrita el título
    axis.title = element_text(face = "bold", size = 12), # Negrita en títulos de ejes
    legend.title = element_text(face = "bold", size = 10), # Negrita en título de leyenda
    legend.position = "right", # Posición de la leyenda
    panel.grid.major = element_blank(), # Eliminar las líneas de cuadrícula principales
    panel.grid.minor = element_blank(), # Eliminar las líneas de cuadrícula menores
    axis.text = element_text(color = "black") # Color de texto de los ejes
  )

#Representación de caja y violín - cluster 2
data_long <- reshape2::melt(datos_y_resultados_kmeans[datos_y_resultados_kmeans$Cluster == 2,  1:3])

ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(alpha = 0.5, adjust = 1.5) + # Ajustar la transparencia y anchura de los violines
  geom_boxplot(outlier.color = "red", outlier.shape = 1, width = 0.2, alpha = 0.5, position = position_dodge(0.9)) + # Ajustar la transparencia y anchura de los boxplots
  scale_fill_brewer(palette = "Pastel1") + # Paleta de colores para los violines y boxplots
  labs(x = "", y = "", fill = "Variable", title = "Perfil del Cluster 2 - Kmeans: Desapego, Insatisfacción y Disimilitud") +
  theme_minimal() +
  ylim(0, 1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), # Centrar y poner en negrita el título
    axis.title = element_text(face = "bold", size = 12), # Negrita en títulos de ejes
    legend.title = element_text(face = "bold", size = 10), # Negrita en título de leyenda
    legend.position = "right", # Posición de la leyenda
    panel.grid.major = element_blank(), # Eliminar las líneas de cuadrícula principales
    panel.grid.minor = element_blank(), # Eliminar las líneas de cuadrícula menores
    axis.text = element_text(color = "black") # Color de texto de los ejes
  )

#################### Kmedoids ####################
#!!!!!Ejecutar pam junto con la semilla!!!!!
set.seed("2345")
pam_res <- pam(dt_clusterizacion, 2)
pam_res
plot_ly(x=~dt_clusterizacion$Desapego, y=~dt_clusterizacion$Insatisfaccion, 
        z=~dt_clusterizacion$Disimilitud, type = 'scatter3d',
        color = ~factor(pam_res$cluster)) %>% 
        layout(
        title = 'Kmedoids 2 Clusters',
        scene = list(
        xaxis = list(title = 'Desapego'),
        yaxis = list(title = 'Insatisfacción'),
        zaxis = list(title = 'Disimilitud')),
        legend = list(
          title = list(text = 'Cluster'),
          font = list(size = 20),
          x = 1,   # Posiciona la leyenda al extremo derecho
          y = 0.5  # Centra la leyenda verticalmente
          )
        )
ggpairs(dt_clusterizacion, aes(color = as.factor(pam_res$cluster), alpha = 0.5), title = "Kmedoids 2 Clusters")
datos_y_resultados_kmedoids <- cbind(dt_clusterizacion, Cluster = pam_res$cluster)
datos_y_resultados_kmedoids

#Representación distribución entre Clusters
distribucion <- as.data.frame(prop.table(table(datos_y_resultados_kmedoids$Cluster))* 100)
names(distribucion) <- c("Cluster", "Porcentaje")
ggplot(distribucion, aes(x = "", y = Porcentaje, fill = Cluster)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + 
  theme_void() +
  geom_text(aes(label = sprintf("%.2f%%", Porcentaje)), position = position_stack(vjust = 0.5)) +
  labs(fill = "Cluster", title = "Distribución Porcentual entre Clusters")+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 12, face = "bold")
  )

#Representación de caja y violín - cluster 1
data_long <- reshape2::melt(datos_y_resultados_kmedoids[datos_y_resultados_kmedoids$Cluster == 1,  1:3])

ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(alpha = 0.5, adjust = 1.5) + # Ajustar la transparencia y anchura de los violines
  geom_boxplot(outlier.color = "red", outlier.shape = 1, width = 0.2, alpha = 0.5, position = position_dodge(0.9)) + # Ajustar la transparencia y anchura de los boxplots
  scale_fill_brewer(palette = "Pastel1") + # Paleta de colores para los violines y boxplots
  labs(x = "", y = "", fill = "Variable", title = "Perfil del Cluster 1 - Kmedoids: Desapego, Insatisfacción y Disimilitud") +
  theme_minimal() +
  ylim(0, 1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), # Centrar y poner en negrita el título
    axis.title = element_text(face = "bold", size = 12), # Negrita en títulos de ejes
    legend.title = element_text(face = "bold", size = 10), # Negrita en título de leyenda
    legend.position = "right", # Posición de la leyenda
    panel.grid.major = element_blank(), # Eliminar las líneas de cuadrícula principales
    panel.grid.minor = element_blank(), # Eliminar las líneas de cuadrícula menores
    axis.text = element_text(color = "black") # Color de texto de los ejes
  )

#Representación de caja y violín - cluster 2
data_long <- reshape2::melt(datos_y_resultados_kmedoids[datos_y_resultados_kmedoids$Cluster == 2,  1:3])

ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(alpha = 0.5, adjust = 1.5) + # Ajustar la transparencia y anchura de los violines
  geom_boxplot(outlier.color = "red", outlier.shape = 1, width = 0.2, alpha = 0.5, position = position_dodge(0.9)) + # Ajustar la transparencia y anchura de los boxplots
  scale_fill_brewer(palette = "Pastel1") + # Paleta de colores para los violines y boxplots
  labs(x = "", y = "", fill = "Variable", title = "Perfil del Cluster 2 - Kmedoids: Desapego, Insatisfacción y Disimilitud") +
  theme_minimal() +
  ylim(0, 1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), # Centrar y poner en negrita el título
    axis.title = element_text(face = "bold", size = 12), # Negrita en títulos de ejes
    legend.title = element_text(face = "bold", size = 10), # Negrita en título de leyenda
    legend.position = "right", # Posición de la leyenda
    panel.grid.major = element_blank(), # Eliminar las líneas de cuadrícula principales
    panel.grid.minor = element_blank(), # Eliminar las líneas de cuadrícula menores
    axis.text = element_text(color = "black") # Color de texto de los ejes
  )

#-------------------Número de clusters difusos y borrosidad---------------------

selection_table_index_and_cluster <- function(data, method = "cmeans"){
  set.seed("2345")
  fuzzy_index <- c(1.1,1.2,1.4,1.6,1.8,2) # Vector con índices de fuzzificación 
  fuzzy_cluster <- c(2,3,4,5,6) # Vector con número de clusters 
  ## Vectores para almacenar los valores de los índices 
  xb <- c() 
  fs <- c() 
  pc <- c() 
  pe <- c() 
  trownames <- c() 
  for (i in 1:length(fuzzy_cluster)) {
    for (j in 1:length(fuzzy_index)) {
      if(method == "cmeans") {
        model <- FKM(data, fuzzy_cluster[i], fuzzy_index[j], 1)
      } else if(method == "cmedoids") {
        model <- FKM.med(data, fuzzy_cluster[i], fuzzy_index[j], 1)
      } else {
        stop("El método especificado debe ser 'cmeans' o 'cmedoids'")
      }
      xb <- c(xb,Fclust.index(model,"XB"))
      pc <- c(pc,Fclust.index(model,"PC"))
      pe <- c(pe,Fclust.index(model,"PE"))
      fs <- c(fs,calcFukuyamaSugeno(as.matrix(data), model$U, model$H, fuzzy_index[j]))
      trownames <- c(trownames,paste0(fuzzy_cluster[i],"-",fuzzy_index[j]))
    }
  }
  ## Creo el dataframe con los valores de los índices y genero la lista de nombres de las columnas
  Ktable <- as.data.frame(cbind(xb,fs,pc,pe))
  tcolnames <- c('XB','FS','PC','PE')
  ## Cambio nombres de columnas y de filas para poder identificar los valores
  # de la tabla
  colnames(Ktable) <- tcolnames
  rownames(Ktable) <- trownames
  # Devuelvo la tabla
  return(Ktable)
}

sustituir_infinitos <- function(tabla_resultado){
  #Infinitos en XB
  max_sin_inf <- max(tabla_resultado$XB[is.finite(tabla_resultado$XB)])
  tabla_resultado$XB <- replace(tabla_resultado$XB, !is.finite(tabla_resultado$XB), max_sin_inf)
  #Infinitos en FS
  max_sin_inf <- max(tabla_resultado$FS[is.finite(tabla_resultado$FS)])
  tabla_resultado$FS <- replace(tabla_resultado$FS, !is.finite(tabla_resultado$FS), max_sin_inf)
  #Infinitos en PC
  max_sin_inf <- max(tabla_resultado$PC[is.finite(tabla_resultado$PC)])
  tabla_resultado$PC <- replace(tabla_resultado$PC, !is.finite(tabla_resultado$PC), max_sin_inf+1)
  #Infinitos en PE
  max_sin_inf <- max(tabla_resultado$PE[is.finite(tabla_resultado$PE)])
  tabla_resultado$PE <- replace(tabla_resultado$PE, !is.finite(tabla_resultado$PE), max_sin_inf)
  return(tabla_resultado)
}

fuzzy_index <- c(1.1,1.2,1.4,1.6,1.8,2) # Vector con índices de fuzzificación
fuzzy_cluster <- c(2,3,4,5,6) # Vector con número de clusters

#################### Cmeans ####################
tabla_resultado_cmeans <- selection_table_index_and_cluster(dt_clusterizacion)
kable(tabla_resultado_cmeans)
tabla_resultado_cmeans <- sustituir_infinitos(tabla_resultado_cmeans)
kable(tabla_resultado_cmeans)

#Sacamos las graficas
#XB
Valores <- matrix(tabla_resultado_cmeans$XB, nrow = 6)
plot_ly(x=~fuzzy_cluster, y=~fuzzy_index ,z =~Valores, type = "surface") %>% 
  layout(
    title = 'Xie-Beni Cmeans',
    scene = list(
      xaxis = list(title = 'Nº Clusters', tickvals = 2:6),
      yaxis = list(title = 'Borrosidad')))

#FS
Valores <- matrix(tabla_resultado_cmeans$FS, nrow = 6)
plot_ly(x=~fuzzy_cluster, y=~fuzzy_index ,z =~Valores, type = "surface") %>% 
  layout(
    title = 'Fukuyama-Sugeno Cmeans',
    scene = list(
      xaxis = list(title = 'Nº Clusters', tickvals = 2:6),
      yaxis = list(title = 'Borrosidad')))

#PC
Valores <- matrix(tabla_resultado_cmeans$PC, nrow = 6)
plot_ly(x=~fuzzy_cluster, y=~fuzzy_index ,z =~Valores, type = "surface") %>% 
  layout(
    title = 'Partition Coefficient Cmeans',
    scene = list(
      xaxis = list(title = 'Nº Clusters', tickvals = 2:6),
      yaxis = list(title = 'Borrosidad')))

#PE
Valores <- matrix(tabla_resultado_cmeans$PE, nrow = 6)
plot_ly(x=~fuzzy_cluster, y=~fuzzy_index ,z =~Valores, type = "surface") %>% 
  layout(
    title = 'Partition Entropy Cmeans',
    scene = list(
      xaxis = list(title = 'Nº Clusters', tickvals = 2:6),
      yaxis = list(title = 'Borrosidad')))

#################### Cmedoids ####################
tabla_resultado_cmedoids <- selection_table_index_and_cluster(dt_clusterizacion, "cmedoids")
kable(tabla_resultado_cmedoids)
tabla_resultado_cmedoids <- sustituir_infinitos(tabla_resultado_cmedoids)
kable(tabla_resultado_cmedoids)

#Sacamos las graficas
#XB
Valores <- matrix(tabla_resultado_cmedoids$XB, nrow = 6)
plot_ly(x=~fuzzy_cluster, y=~fuzzy_index ,z =~Valores, type = "surface") %>% 
  layout(
    title = 'Xie-Beni Cmedoids',
    scene = list(
      xaxis = list(title = 'Nº Clusters', tickvals = 2:6),
      yaxis = list(title = 'Borrosidad')))

#FS
Valores <- matrix(tabla_resultado_cmedoids$FS, nrow = 6)
plot_ly(x=~fuzzy_cluster, y=~fuzzy_index ,z =~Valores, type = "surface") %>% 
  layout(
    title = 'Fukuyama-Sugeno Cmedoids',
    scene = list(
      xaxis = list(title = 'Nº Clusters', tickvals = 2:6),
      yaxis = list(title = 'Borrosidad')))

#PC
Valores <- matrix(tabla_resultado_cmedoids$PC, nrow = 6)
plot_ly(x=~fuzzy_cluster, y=~fuzzy_index ,z =~Valores, type = "surface") %>% 
  layout(
    title = 'Partition Coefficient Cmedoids',
    scene = list(
      xaxis = list(title = 'Nº Clusters', tickvals = 2:6),
      yaxis = list(title = 'Borrosidad')))

#PE
Valores <- matrix(tabla_resultado_cmedoids$PE, nrow = 6)
plot_ly(x=~fuzzy_cluster, y=~fuzzy_index ,z =~Valores, type = "surface") %>% 
  layout(
    title = 'Partition Entropy Cmedoids',
    scene = list(
      xaxis = list(title = 'Nº Clusters', tickvals = 2:6),
      yaxis = list(title = 'Borrosidad')))

#------------------------------Clusterización difusa----------------------------
#Obtener el array para colorear el cluster difuso
procesar_clusters <- function(dta) {
  result_array <- numeric(nrow(dta))
  for (i in 1:nrow(dta)) {
    if (dta[i, 1] > 1) {
      result_array[i] <- dta[i, 2] + (dta[i, 1] - 1)
    } else {
      if (dta[i, 2] == 1){
        result_array[i] <- dta[i, 2]
      }
      else{
        result_array[i] <- (1 - dta[i, 2]) + 1
      }
    }
  }
  return(result_array)
}

#################### Cmeans ####################
#!!!!!Ejecutar Cmeans junto con la semilla!!!!!
set.seed("2345")
Cmeans <- FKM(dt_clusterizacion, 2, 1.1)
Cmeans
plot_ly(x=~dt_clusterizacion$Desapego, y=~dt_clusterizacion$Insatisfaccion, 
        z=~dt_clusterizacion$Disimilitud, color = procesar_clusters(Cmeans$clus), 
        type = 'scatter3d', mode = 'markers',
        text = ~paste("Cluster:", round(procesar_clusters(Cmeans$clus),2)), hoverinfo = 'text') %>% 
        layout(
        title = 'Cmeans 2 Clusters',
        scene = list(
        xaxis = list(title = 'Desapego'),
        yaxis = list(title = 'Insatisfacción'),
        zaxis = list(title = 'Disimilitud')))
ggpairs(dt_clusterizacion, aes(color = as.factor(Cmeans$clus[,1]), alpha = 0.5), title = "Cmeans 2 Clusters")
datos_y_resultados_cmeans <- cbind(dt_clusterizacion, Cluster = Cmeans$clus[,1])
datos_y_resultados_cmeans


#Representación distribución entre Clusters
distribucion <- as.data.frame(prop.table(table(datos_y_resultados_cmeans$Cluster))* 100)
names(distribucion) <- c("Cluster", "Porcentaje")
ggplot(distribucion, aes(x = "", y = Porcentaje, fill = Cluster)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + 
  theme_void() +
  geom_text(aes(label = sprintf("%.2f%%", Porcentaje)), position = position_stack(vjust = 0.5)) +
  labs(fill = "Cluster", title = "Distribución Porcentual entre Clusters")+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 12, face = "bold")
  )

#Representación de caja y violín - cluster 1
data_long <- reshape2::melt(datos_y_resultados_cmeans[datos_y_resultados_cmeans$Cluster == 1,  1:3])

ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(alpha = 0.5, adjust = 1.5) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1, width = 0.2, alpha = 0.5, position = position_dodge(0.9)) + 
  scale_fill_brewer(palette = "Pastel1") + 
  labs(x = "", y = "", fill = "Variable", title = "Perfil del Cluster 1 - Cmeans: Desapego, Insatisfacción y Disimilitud") +
  theme_minimal() +
  ylim(0, 1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(face = "bold", size = 12), 
    legend.title = element_text(face = "bold", size = 10), 
    legend.position = "right", 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text = element_text(color = "black") 
  )

#Representación de caja y violín - cluster 2
data_long <- reshape2::melt(datos_y_resultados_cmeans[datos_y_resultados_cmeans$Cluster == 2,  1:3])

ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(alpha = 0.5, adjust = 1.5) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1, width = 0.2, alpha = 0.5, position = position_dodge(0.9)) + 
  scale_fill_brewer(palette = "Pastel1") + 
  labs(x = "", y = "", fill = "Variable", title = "Perfil del Cluster 2 - Cmeans: Desapego, Insatisfacción y Disimilitud") +
  theme_minimal() +
  ylim(0, 1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 10), 
    legend.position = "right", 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text = element_text(color = "black") 
  )

#################### Cmedoids ####################
#!!!!!Ejecutar Cmeans junto con la semilla!!!!!
set.seed("12345")
Cmedoids <- FKM.med(dt_clusterizacion, 2, 1.1)
Cmedoids
plot_ly(x=~dt_clusterizacion$Desapego, y=~dt_clusterizacion$Insatisfaccion, 
        z=~dt_clusterizacion$Disimilitud, color = procesar_clusters(Cmedoids$clus), 
        type = 'scatter3d', mode = 'markers',
        text = ~paste("Cluster:", round(procesar_clusters(Cmedoids$clus),2)), hoverinfo = 'text') %>% 
        layout(
        title = 'Cmedoids 2 Clusters',
        scene = list(
        xaxis = list(title = 'Desapego'),
        yaxis = list(title = 'Insatisfacción'),
        zaxis = list(title = 'Disimilitud')))
ggpairs(dt_clusterizacion, aes(color = as.factor(Cmedoids$clus[,1]), alpha = 0.5), title = "Cmedoids 2 Clusters")
datos_y_resultados_cmedoids <- cbind(dt_clusterizacion, Cluster = Cmedoids$clus[,1])
datos_y_resultados_cmedoids

#Representación distribución entre Clusters
distribucion <- as.data.frame(prop.table(table(datos_y_resultados_cmedoids$Cluster))* 100)
names(distribucion) <- c("Cluster", "Porcentaje")
ggplot(distribucion, aes(x = "", y = Porcentaje, fill = Cluster)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + 
  theme_void() +
  geom_text(aes(label = sprintf("%.2f%%", Porcentaje)), position = position_stack(vjust = 0.5)) +
  labs(fill = "Cluster", title = "Distribución Porcentual entre Clusters")+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 12, face = "bold")
  )

#Representación de caja y violín - cluster 1
data_long <- reshape2::melt(datos_y_resultados_cmedoids[datos_y_resultados_cmedoids$Cluster == 1,  1:3])

ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(alpha = 0.5, adjust = 1.5) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1, width = 0.2, alpha = 0.5, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "", y = "", fill = "Variable", title = "Perfil del Cluster 1 - Cmedoids: Desapego, Insatisfacción y Disimilitud") +
  theme_minimal() +
  ylim(0, 1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12), 
    legend.title = element_text(face = "bold", size = 10), 
    legend.position = "right", 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text = element_text(color = "black") 
  )

#Representación de caja y violín - cluster 2
data_long <- reshape2::melt(datos_y_resultados_cmedoids[datos_y_resultados_cmedoids$Cluster == 2,  1:3])

ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(alpha = 0.5, adjust = 1.5) + # Ajustar la transparencia y anchura de los violines
  geom_boxplot(outlier.color = "red", outlier.shape = 1, width = 0.2, alpha = 0.5, position = position_dodge(0.9)) + # Ajustar la transparencia y anchura de los boxplots
  scale_fill_brewer(palette = "Pastel1") + # Paleta de colores para los violines y boxplots
  labs(x = "", y = "", fill = "Variable", title = "Perfil del Cluster 2 - Cmedoids: Desapego, Insatisfacción y Disimilitud") +
  theme_minimal() +
  ylim(0, 1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(face = "bold", size = 12), 
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right", 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text = element_text(color = "black")
  )

#------------------------------Clusterización Categórica------------------------
calcularDunn <- function(data, distance_matrix, range) {
  set.seed(2345) # Reproducibilidad
  dunn_indices <- numeric(length(range))
  for (i in seq_along(range)) {
    kmodes_result <- kmodes(data, modes = range[i])
    dunn_indices[i] <- dunn(distance_matrix, kmodes_result$cluster)
  }
  return(dunn_indices)
}

#---- Obtención del número óptimo de conglomerados ----
#Cluster Jerárquico
HCPC(dt_clusterizacion_categorica, kk=Inf, min=2, max=6, consol=TRUE)

#Silhouette
fviz_nbclust(dt_clusterizacion_categorica,kmodes,method = "silhouette", k.max=6)+
  ggtitle("Evaluación del Número Óptimo de Clústeres - Silhouette") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))+
  geom_vline(xintercept = 6, linetype = "dashed", color = "black")+
  geom_point(color = "black")

#DUNN
gower_dist <- daisy(dt_clusterizacion_categorica, metric = "gower")
dunn <- calcularDunn(dt_clusterizacion_categorica, gower_dist, 2:6)

indexResults <- data.frame(
  k = 2:6,
  Dunn = dunn
)
ggplot(data = indexResults, aes(x = k, y = Dunn)) +
  ggtitle("Evaluación del Número Óptimo de Clústeres - Silhouette") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))+
  geom_line(color = "blue") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "black")+
  geom_point(color = "black")+
  labs(x = "Número de clusters", y = "Valor Índice Dunn", title = "Índice de Dunn")

################# Kmodes #################
#!!!!!Ejecutar Kmodes junto con la semilla!!!!!
set.seed("2345")
kmodes_result<- kmodes(dt_clusterizacion_categorica, 6)
kmodes_result

#Representación distribución entre Clusters
distribucion <- as.data.frame(prop.table(table(kmodes_result$cluster))* 100)
names(distribucion) <- c("Cluster", "Porcentaje")
ggplot(distribucion, aes(x = "", y = Porcentaje, fill = Cluster)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + 
  theme_void() +
  geom_text(aes(label = sprintf("%.2f%%", Porcentaje)), position = position_stack(vjust = 0.5)) +
  labs(fill = "Cluster", title = "Distribución Porcentual entre Clusters")+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 12, face = "bold")
  )

#Identificamos las características de los individuos del clúster
hist2d <- plot_ly(
  x = ~factor(dt_clusterizacion_categorica$Edad, levels = c(1, 2, 3), labels = c("18-29", "30-44", "45+")), 
  y = ~factor(dt_clusterizacion_categorica$Genero, levels = c(1, 2), labels = c("Hombre", "Mujer")),
  type = 'histogram2d',
) %>%
  add_text(
    text = ~paste("Clúster", kmodes_result$cluster, "\n"),
    textposition = "middle center",
    textfont = list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
  )
subplot(hist2d) %>%
  layout(
    title = 'Kmodes 6 Clusters',
    xaxis = list(title = 'Edad'),
    yaxis = list(title = 'Género'),
    legend = list(title = list(text = '<b>Clusters</b>'))
  )
kable(kmodes_result$size)

#------------------------------Clusterización Mixta-----------------------------
#Preparamos los datos a clusterizar
dt_clusterizacion_mixta <- cbind(dt_clusterizacion, dt_clusterizacion_categorica)
dt_clusterizacion_mixta$Edad <- as.factor(dt_clusterizacion_mixta$Edad)
dt_clusterizacion_mixta$Genero <- as.factor(dt_clusterizacion_mixta$Genero)

#---- Obtención del número óptimo de conglomerados ----
#Silhouette
#!!!!!!!!!!Ejecutar todo junto a la semilla!!!!!!!!!!!!
set.seed("112233")
Essil <- numeric(5)
for(i in 2:6){
  kpres <- kproto(dt_clusterizacion_mixta, k = i)
  Essil[i-1] <- validation_kproto("silhouette",kpres)
}
ggplot(data = data.frame(2:6, Essil), aes(x = 2:6, y = Essil)) +
  labs(x = "Número de clusters", y = "Índice Silhouette", title = "Silhouette") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))+
  geom_line(color = "blue") +
  geom_vline(xintercept = 2, linetype = "dashed", color = "black")+
  geom_point(color = "black")

#Mcclain
#!!!!!!!!!!Ejecutar todo junto a la semilla!!!!!!!!!!!!
set.seed("112233")
Esmcclain <- numeric(5)
for(i in 2:6){
  kpres <- kproto(dt_clusterizacion_mixta, k = i)
  Esmcclain[i-1] <- validation_kproto("mcclain",kpres)
}
ggplot(data = data.frame(2:6, Esmcclain), aes(x = 2:6, y = Esmcclain)) +
  labs(x = "Número de clusters", y = "Índice McClain-Rao", title = "McClain-Rao") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))+
  geom_line(color = "blue") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "black")+
  geom_point(color = "black")

#Dunn
#!!!!!!!!!!Ejecutar todo junto a la semilla!!!!!!!!!!!!
set.seed("112233")
Esdunn <- numeric(5)
for(i in 2:6){
  kpres <- kproto(dt_clusterizacion_mixta, k = i)
  Esdunn[i-1] <- validation_kproto("dunn",kpres)
}
ggplot(data = data.frame(2:6, Esdunn), aes(x = 2:6, y = Esdunn)) +
  labs(x = "Número de clusters", y = "Índice Dunn", title = "Dunn") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))+
  geom_line(color = "blue") +
  geom_vline(xintercept = 3, linetype = "dashed", color = "black")+
  geom_point(color = "black")

#Cindex
#!!!!!!!!!!Ejecutar todo junto a la semilla!!!!!!!!!!!!
set.seed("112233")
Escindex <- numeric(5)
for(i in 2:6){
  kpres <- kproto(dt_clusterizacion_mixta, k = i)
  Escindex[i-1] <- validation_kproto("cindex",kpres)
}
ggplot(data = data.frame(2:6, Escindex), aes(x = 2:6, y = Escindex)) +
  labs(x = "Número de clusters", y = "Índice Cindex", title = "Cindex") +
  theme_minimal()+
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))+
  geom_line(color = "blue") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "black")+
  geom_point(color = "black")

################# Kprototypes #################
#!!!!!!!!!!Ejecutar Kproto junto a la semilla!!!!!!!!!!!!
set.seed("112233")
kproto_result <- kproto(dt_clusterizacion_mixta, k = 5)
kproto_result
datos_y_resultados_kproto <- cbind(kproto_result$data, Cluster = kproto_result$cluster)
datos_y_resultados_kproto

#Representación distribución entre Clusters
distribucion <- as.data.frame(prop.table(table(datos_y_resultados_kproto$Cluster))* 100)
names(distribucion) <- c("Cluster", "Porcentaje")
ggplot(distribucion, aes(x = "", y = Porcentaje, fill = Cluster)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + 
  theme_void() +
  geom_text(aes(label = sprintf("%.2f%%", Porcentaje)), position = position_stack(vjust = 0.5)) +
  labs(fill = "Cluster", title = "Distribución Porcentual entre Clusters")+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 12, face = "bold")
  )

par(mfrow=c(3, 2))
for (i in 1:5) {
  boxplot(datos_y_resultados_kproto[,i] ~ datos_y_resultados_kproto$Cluster,
          main = names(datos_y_resultados_kproto)[i],
          xlab = "Clúster",
          ylab = "Valor",
          col = c("#f8766d", "#a3a500", "#00bf7d", "#00b0f6", "#e76bf3"),
          border = "black")
}
par(mfrow=c(1, 1))

kable(kproto_result$size)

summary(kproto_result)
################################ FIN Clustering ################################

################################### PLS-Sem ####################################

#----------------- Datos y definición del modelo estructural -------------------
datos_csem <- datos
datos_csem$Kmeans_result <- datos_y_resultados_kmeans$Cluster
datos_csem$Kmedoids_result <- datos_y_resultados_kmedoids$Cluster
datos_csem$Cmeans_result <- Cmeans$clus[,1]
datos_csem$Cmedoids_result <- Cmedoids$clus[,1]
datos_csem$Kmodes_result <- kmodes_result$cluster
datos_csem$kproto_result <- kproto_result$cluster
datos_csem

modeloDef <- "
#Structural model
Conflicto ~ NoRespCultura
Turismofobia ~ Conflicto + NoRespCultura

#Measurement models
NoRespCultura =~ NRC1 + NRC2 + NRC3 
Turismofobia =~ TF1 + TF2 + TF3
Conflicto =~ CTL1 + CTL2 + CTL3 + CTL4 + CTL5 + CTL6
"

#---------------------- Evaluación del modelo ----------------------------------
Csem_result <- csem(.data=datos, .model=modeloDef,
                .disattenuate = FALSE,
                .resample_method = 'bootstrap',
                .seed = 112233,
                .R = 5000)

verify(Csem_result)
summarize(Csem_result) #loadings, path estimates 
assess(Csem_result) #HTMT2, AVE, Cronbachs_alpha, Joereskogs_rho, VIF value

#------------------------- Análisis multigrupo - Kmeans ------------------------
Csem_Kmeans <- csem(.data = datos_csem, .model = modeloDef, .disattenuate = FALSE, .resample_method = "bootstrap", .seed = 112233, .R=5000, .id="Kmeans_result")
verify(Csem_Kmeans)

# Prueba MICOM
outMICOM=testMICOM(Csem_Kmeans, .seed = 112233)
outMICOM

# Análisis Multigrupo
outoverall=testMGD(.object = Csem_Kmeans, .type_vcv='construct', .seed = 112233, .approach_mgd = 'Chin'); outoverall
# No existen diferencias significativas

#------------------------ Análisis multigrupo - Kmedoids -----------------------
Csem_Kmedoids<- csem(.data = datos_csem, .model = modeloDef, .disattenuate = FALSE, .resample_method = "bootstrap", .seed = 112233, .R=5000, .id="Kmedoids_result")
verify(Csem_Kmedoids)

# Prueba MICOM
outMICOM=testMICOM(Csem_Kmedoids, .seed = 112233)
outMICOM

# Análisis Multigrupo
outoverall=testMGD(.object = Csem_Kmedoids, .type_vcv='construct', .seed = 112233, .approach_mgd = 'Chin'); outoverall
# No existen diferencias significativas

#---------------------- Análisis multigrupo - Cmeans ---------------------------
Csem_Cmeans<- csem(.data = datos_csem, .model = modeloDef, .disattenuate = FALSE, .resample_method = "bootstrap", .seed = 112233, .R=5000, .id="Cmeans_result")
verify(Csem_Cmeans)

# Prueba MICOM
outMICOM=testMICOM(Csem_Cmeans, .seed = 112233)
outMICOM

# Análisis Multigrupo
outoverall=testMGD(.object = Csem_Cmeans, .type_vcv='construct', .seed = 112233, .approach_mgd = 'Chin'); outoverall
# No existen diferencias significativas

#---------------------- Análisis multigrupo - Cmedoids -------------------------
Csem_Cmedoids<- csem(.data = datos_csem, .model = modeloDef, .disattenuate = FALSE, .resample_method = "bootstrap", .seed = 112233, .R=5000, .id="Cmedoids_result")
verify(Csem_Cmedoids)

# Prueba MICOM
outMICOM=testMICOM(Csem_Cmedoids, .seed = 112233)
outMICOM

# Análisis Multigrupo
outoverall=testMGD(.object = Csem_Cmedoids, .type_vcv='construct', .seed = 112233, .approach_mgd = 'Chin'); outoverall
# No existen diferencias significativas

#------------------------ Análisis multigrupo - Kmodes -------------------------
Csem_Kmodes<- csem(.data = datos_csem, .model = modeloDef, .disattenuate = FALSE, .resample_method = "bootstrap", .seed = 112233, .R=5000, .id="Kmodes_result")
verify(Csem_Kmodes)

# Prueba MICOM
outMICOM=testMICOM(Csem_Kmodes, .seed = 112233)
outMICOM

# Análisis Multigrupo
outoverall=testMGD(.object = Csem_Kmodes, .type_vcv='construct', .seed = 112233, .approach_mgd = 'Chin'); outoverall
# Existen diferencias significativas entre los grupos 2_3, 2_5, 2_6, 3_4, 4_5 y 4_6

summarize(Csem_Kmodes)

#------------------------ Análisis multigrupo - Kproto -------------------------
Csem_Kproto<- csem(.data = datos_csem, .model = modeloDef, .disattenuate = FALSE, .resample_method = "bootstrap", .seed = 112233, .R=5000, .id="kproto_result")
verify(Csem_Kproto)

# Prueba MICOM
outMICOM=testMICOM(Csem_Kproto, .seed = 112233)
outMICOM

# Análisis Multigrupo
outoverall=testMGD(.object = Csem_Kproto, .type_vcv='construct', .seed = 112233, .approach_mgd = 'Chin'); outoverall
# Existen diferencias significativas entre los grupos 2_3 y 2_5

summarize(Csem_Kproto)

