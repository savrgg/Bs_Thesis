# Conjunto sintético de datos
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)

set.seed(213521)
# crear 5000 puntos al azar y muestrear de ahi
  Clase1 <- data.frame(Clase = "1",
                       x1 = rnorm(10, mean = -5, 2.5),
                       y1 = rnorm(10, mean = -8, 2))
  Clase2 <- data.frame(Clase = "2",
                       x1 = rnorm(10, mean = -3, 2.5),
                       y1 = rnorm(10, mean = -4, 2))
  Clase3 <- data.frame(Clase = "3",
                       x1 = rnorm(10, mean = -7, 2.5),
                       y1 = rnorm(10, mean = -1, 2))
  
  
  clases <- rbind(Clase1, Clase2, Clase3)
  
  
# Centros
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
  
  centro.c2 <- c(centros$x1[2], centros$y1[2])
  c2 <- clases %>% filter(Clase == "2") %>% 
    dplyr::select(x1, y1) %>% 
    mutate(x1 = x1 - centro.c2[1],
           y1 = y1 -  centro.c2[2])
  
  centro.c3 <- c(centros$x1[3], centros$y1[3])
  c3 <- clases %>% filter(Clase == "3") %>% 
    dplyr::select(x1, y1) %>% 
    mutate(x1 = x1 - centro.c3[1],
           y1 = y1 -  centro.c3[2])
  
  S_I <- t(as.matrix(c1))%*%as.matrix(c1) + 
    t(as.matrix(c2))%*%as.matrix(c2)+
    t(as.matrix(c3))%*%as.matrix(c3)
  
  
  # Matriz de dispersión entre clases
  centros.c <-data.frame(Clase = centros$Clase,
                         x1 = centros$x1 - centro$x1,
                         y1 = centros$y1 - centro$y1)
  S_E <- 10*t(as.matrix(centros.c %>% 
                       filter(Clase == "1") %>% 
                       dplyr::select(-Clase))) %*%
    as.matrix(centros.c %>% 
                filter(Clase == "1") %>% 
                dplyr::select(-Clase))  +
    10*t(as.matrix(centros.c %>% 
                     filter(Clase == "2") %>% 
                     dplyr::select(-Clase))) %*%
    as.matrix(centros.c %>% 
                filter(Clase == "2") %>% 
                dplyr::select(-Clase))  +
    10*t(as.matrix(centros.c %>% 
                     filter(Clase == "3") %>% 
                     dplyr::select(-Clase))) %*%
    as.matrix(centros.c %>% 
                filter(Clase == "3") %>% 
                dplyr::select(-Clase))  
  
  
  
# Matriz de dispersión total
S_I + S_E  
29*(as.matrix(clases %>% dplyr::select(x1, y1)) %>% cov)

# ggplot 
clases <- clases %>% 
  left_join(centros %>% 
              dplyr::rename(x.c = x1,
                            y.c = y1))
ggplot()+
  geom_point(aes(x = clases$x1, 
                 y = clases$y1, 
                 color = clases$Clase)) +
  geom_point(aes(x = centros$x1, 
                 y = centros$y1, 
                 color = centros$Clase), size = 5)+
  geom_segment(aes(x = clases$x1, xend = clases$x.c, 
                   y = clases$y1, yend = clases$y.c,
                   color = clases$Clase)) + 
  geom_point()+
  labs(x = "x1", y = "x2", title = "Dispersión interna de clases")+
  xlim(c(-12,3))+ ylim(c(-12,3))


centros <- centros %>% 
  mutate(x.c = centro$x1, 
         y.c = centro$y1)

ggplot()+
  geom_point(aes(x = centros$x1, 
                 y = centros$y1, 
                 color = centros$Clase), size = 5)+
  geom_segment(aes(x = centros$x1, xend = centros$x.c, 
                   y = centros$y1, yend = centros$y.c,
                   color = centros$Clase)) +
  geom_point(aes(x = centro$x1, y = centro$y1), size = 5 ) +
  labs(x = "x1", y = "x2", title = "Dispersión entre clases")+
  xlim(c(-12,3))+ ylim(c(-12,3))

ggplot()+
  geom_point(aes(x = clases$x1, 
                 y = clases$y1, 
                 color = clases$Clase)) +
  geom_segment(aes(x = clases$x1, xend = centro$x1, 
                   y = clases$y1, yend = centro$y1,
                   color = clases$Clase)) + 
  geom_point(aes(x = centro$x1, y = centro$y1), size = 5 ) +
  labs(x = "x1", y = "x2", title = "Dispersión de todas las clases")+
  xlim(c(-12,3))+ ylim(c(-12,3))


  
  
  