
# 1) Cargamos librerias ---------------------------------------------------
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)

set.seed(213521)



# 2) Creamos muestra distribuidas normalmente -----------------------------


# crear 5000 puntos al azar y muestrear de ahi
sim <- lapply(1:100, function(x){
Clase1 <- data.frame(Clase = "1",
                     x1 = rnorm(50, mean = -5, 2.5),
                     y1 = rnorm(50, mean = -8, 2))

Clase2 <- data.frame(Clase = "2",
                     x1 = rnorm(50, mean = -3, 2.5),
                     y1 = rnorm(50, mean = -4, 2))

clases <- rbind(Clase1, Clase2)

# Vector de medias

centros <- clases %>% group_by(Clase) %>%
  dplyr::summarise(x1 = mean(x1), 
                   y1 = mean(y1)) %>% 
  data.frame()

centro <- clases %>% 
  dplyr::summarise(x1 = mean(x1), 
                   y1 = mean(y1)) 

# Matriz de dispersión interna
# clase 1

centro.c1 <- c(centros$x1[1], centros$y1[1])
c1 <- clases %>% filter(Clase == "1") %>% 
  dplyr::select(x1, y1) %>% 
  mutate(x1 = x1 - centro.c1[1],
         y1 = y1 - centro.c1[2])


# clase 2
centro.c2 <- c(centros$x1[2], centros$y1[2])
c2 <- clases %>% filter(Clase == "2") %>% 
  dplyr::select(x1, y1) %>% 
  mutate(x1 = x1 - centro.c1[1],
         y1 = y1 -  centro.c1[1])

S_I <- t(as.matrix(c1))%*%as.matrix(c1) + 
       t(as.matrix(c2))%*%as.matrix(c2)


# Matriz de dispersión entre clases
centros.c <-data.frame(Clase = centros$Clase,
                       x1 = centros$x1 - centro$x1,
                       y1 = centros$y1 - centro$y1)
#  centros.c$x1 <- centros$x1 - centro$x1
#  centros.c$y1 <- centros$y1 - centro$y1

S_E <- t(as.matrix(centros.c %>% 
  filter(Clase == "1") %>% 
  dplyr::select(-Clase))) %*%
as.matrix(centros.c %>% 
  filter(Clase == "1") %>% 
  dplyr::select(-Clase))  

# Ecuación de la linea

B0 <- .5*  as.matrix(centros %>% 
                    filter(Clase == "2") %>% 
                    dplyr::select(-Clase)) %*%
  solve(cov(as.matrix(clases %>% 
                        dplyr::select(-Clase)))) %*%
  t(as.matrix(centros %>% 
                filter(Clase == "2") %>% 
                dplyr::select(-Clase)) )-
  as.matrix(centros %>% 
            filter(Clase == "1") %>% 
            dplyr::select(-Clase)) %*%
  solve(cov(as.matrix(clases %>% 
                  dplyr::select(-Clase)))) %*%
  t(as.matrix(centros %>% 
              filter(Clase == "1") %>% 
              dplyr::select(-Clase)))


B1 <- solve(cov(as.matrix(clases %>% 
            dplyr::select(-Clase)))) %*%
t(as.matrix(centros %>% 
            filter(Clase == "1") %>% 
            dplyr::select(-Clase)))-
t(as.matrix(centros %>% 
            filter(Clase == "2") %>% 
            dplyr::select(-Clase)))

x.plot <- seq(-10,1, .11)
y.plot = -10 - x.plot * B1[1]/B1[2]
linea <- data.frame(x.plot, y.plot)

list(x.plot, y.plot)
})


  

y.intervalos <- sapply(1:100, function(y){
  ldply(sim[[y]][[2]])
}) %>% data.frame() %>% 
  mutate(ID = 1:101) %>% 
    gather(variable, value, V1:V1.99) %>%
    group_by(ID) %>% 
    dplyr::summarise(q20 = quantile(value, probs = .2),
                     q50 = quantile(value, probs = .5),
                     q80 = quantile(value, probs = .8))
  
y.intervalos <- y.intervalos %>% 
  mutate(xplot = seq(-10,1, .11)) %>% 
  data.frame() 
  

ggplot()+
  geom_point(aes(x = clases$x1, 
                 y = clases$y1, 
                 color = clases$Clase)) +
  geom_point(aes(x = centros$x1, 
                 y = centros$y1, 
                 color = centros$Clase), size = 5)+
  geom_line(aes(x = y.intervalos$xplot, y = y.intervalos$q50)) +
#   geom_linerange(aes(x = y.intervalos$xplot, 
#                      ymin = y.intervalos$q20, 
#                      ymax = y.intervalos$q80))



