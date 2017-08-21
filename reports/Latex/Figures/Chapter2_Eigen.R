library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)


# 1) Forma de la funcion --------------------------------------------------
x <- seq(-10,10, by = .01)+10
y <- -exp(x-10)/(1+exp(x-10)) +.5
puntos <- data.frame(x, y)

plot <- ggplot() +
  geom_line(data = puntos, aes(x =x, y = y)) + xlim(c(0,20)) +
  geom_point(aes(x = 10, y = 0), color = "dodgerblue", size = 5) +
  geom_text(aes(10, 0, label="p*",vjust = -1.3, hjust = -.5), size = 7) +
  geom_text(aes(0, 0, label="0",vjust = 0, hjust = 0), size = 5) +
  geom_vline(xintercept = 0, color = "red", alpha= .5)+
  geom_hline(yintercept = 0, color = "red", alpha= .5) +
  labs(title = "Comportamiento de la funcion f(p)", 
       x = "p", y = "f(p)")+
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) 


# png("/Users/salvadorgarcia/Repositorios/tesis/Figures/Chapter2_fp.png", width = 600, height = 400)
# plot
# dev.off()



# 2) Ejemplo 1--------------------------------------------------------------
x <- seq(0,20, by = .01)
y <- 4-(1.5*x)
puntos1 <- data.frame(x, y)

x <- seq(0,20, by = .01)
y <- 6-(2.5*x)
puntos2 <- data.frame(x, y)

x <- seq(0,20, by = .01)
y <- 8-(5*x)
puntos3 <- data.frame(x, y)

# Grafica de los eigenvalores
plot <- ggplot() +
  geom_line(data = puntos1, aes(x =x, y = y), color = "firebrick4") +
  geom_line(data = puntos2, aes(x =x, y = y), color = "dodgerblue4")+
  geom_line(data = puntos3, aes(x =x, y = y), color = "forestgreen") + 
  geom_vline(xintercept = 0, color = "red", alpha= .5)+
  geom_hline(yintercept = 0, color = "red", alpha= .5) +
  xlim(c(-0.8,10)) + ylim(c(-11,11)) +
  labs(title = "Variación de eigenvalores conforme p cambia", 
       x = "p", y = "f(p)")+
  geom_text(aes(-0.7, 3.5, label="4-1.5p",vjust = -1.3, hjust = -.5), size = 5) +
  geom_text(aes(-0.7, 5.5, label="6-2.5p",vjust = -1.3, hjust = -.5), size = 5) +
  geom_text(aes(-0.7, 7.5, label="8-5p",vjust = -1.3, hjust = -.5), size = 5) +
  theme(plot.title = element_text(size=23))+
  theme(axis.title.y = element_text(size = 23 ))+
  theme(axis.title.x = element_text(size = 23 ))+
  theme(axis.text = element_text(size = 20))
#png("/Users/salvadorgarcia/Repositorios/tesis/Figures/Chapter2_fp.png", width = 600, height = 400)
# png("/Users/salvadorgarcia/Repositories/Escrito/tesis/Figures/Chapter2_3eigen.png", width = 600, height = 400)
# plot
# dev.off()

# f(p) con un valor máximo de un eigenvalor
plot1 <- ggplot() +
  geom_line(data = puntos1[201:2001, ], aes(x =x, y = y), color = "firebrick4") +
  geom_line(data = puntos2[81:201,], aes(x =x, y = y), color = "dodgerblue4")+
  geom_line(data = puntos3[0:81,], aes(x = x, y = y), color = "forestgreen") + 
  geom_vline(xintercept = 0, color = "red", alpha= .5)+
  geom_hline(yintercept = 0, color = "red", alpha= .5) +
  xlim(c(-0.8,10)) + ylim(c(-11,11)) +
  labs(title = "Función f(p) con p = 1", 
       x = "p", y = "f(p)")+
  theme(plot.title = element_text(size=23))+
  theme(axis.title.y = element_text(size = 23 ))+
  theme(axis.title.x = element_text(size = 23 ))+
  theme(axis.text = element_text(size = 20))

# f(p) con un valor máximo de dos eigenvalores
temp1 <- puntos3[0:116, ] +puntos2[0:116, ]
temp1 <- temp1 %>% mutate(x = x/2)
temp2 <- puntos2[116:2001, ] +puntos1[116:2001, ]
temp2 <- temp2 %>% mutate(x = x/2)

plot2 <- ggplot() +
  geom_line(data = temp1, aes(x =x, y = y), color = "cyan4") +
  geom_line(data = temp2, aes(x =x, y = y), color = "chocolate3")+
  geom_vline(xintercept = 0, color = "red", alpha= .5)+
  geom_hline(yintercept = 0, color = "red", alpha= .5) +
  xlim(c(-0.8,10)) + ylim(c(-11,15)) +
  labs(title = "Función f(p) con p = 2", 
       x = "p", y = "f(p)")+
  theme(plot.title = element_text(size=23))+
  theme(axis.title.y = element_text(size = 23 ))+
  theme(axis.title.x = element_text(size = 23 ))+
  theme(axis.text = element_text(size = 20))

# f(p) con un valor máximo de tres eigenvalores

temp3 <- puntos1+puntos2+puntos3
temp3 <- temp3 %>% mutate(x = x/3)

plot3 <- ggplot() +
  geom_line(data = temp3, aes(x =x, y = y), color = "black") +
  geom_vline(xintercept = 0, color = "red", alpha= .5)+
  geom_hline(yintercept = 0, color = "red", alpha= .5) +
  xlim(c(-0.8,10)) + ylim(c(-11,20)) +
  labs(title = "Función f(p) con p = 3", 
       x = "p", y = "f(p)")+
  theme(plot.title = element_text(size=23))+
  theme(axis.title.y = element_text(size = 23 ))+
  theme(axis.title.x = element_text(size = 23 ))+
  theme(axis.text = element_text(size = 20))


png("/Users/salvadorgarcia/Repositories/Escrito/tesis/Figures/Chapter2_grid3eigen.png", width = 600, height = 850)
grid.arrange(plot1,plot2,plot3,ncol = 1)
dev.off()




# Primeras cotas para la localización del óptimo


# f(p) con un valor máximo de un eigenvalor
plot1 <- ggplot() +
  geom_rect(aes(xmin = 3.9/1.5, xmax = 4.1/1.5,   ymin = -Inf, ymax = Inf),  
            fill = "red", alpha = .3)+
  geom_line(data = puntos1[201:2001, ], aes(x =x, y = y), color = "firebrick4") +
  geom_line(data = puntos2[81:201,], aes(x =x, y = y), color = "dodgerblue4")+
  geom_line(data = puntos3[0:81,], aes(x = x, y = y), color = "forestgreen") + 
  geom_vline(xintercept = 0, color = "red", alpha= .5)+
  geom_hline(yintercept = 0, color = "red", alpha= .5) +
  xlim(c(-0.8,10)) + ylim(c(-11,11)) +
  labs(title = "Intervalos para p*, con p = 1", 
       x = "p", y = "f(p)")+
  theme(plot.title = element_text(size=23))+
  theme(axis.title.y = element_text(size = 23 ))+
  theme(axis.title.x = element_text(size = 23 ))+
  theme(axis.text = element_text(size = 20)) 
  

# f(p) con un valor máximo de dos eigenvalores
temp1 <- puntos3[0:116, ] +puntos2[0:116, ]
temp1 <- temp1 %>% mutate(x = x/2)
temp2 <- puntos2[116:2001, ] +puntos1[116:2001, ]
temp2 <- temp2 %>% mutate(x = x/2)

plot2 <- ggplot() +
  geom_rect(aes(xmin = 6/2.5, xmax = 4/1.5,   ymin = -Inf, ymax = Inf),  
            fill = "red", alpha = .3)+
  geom_line(data = temp1, aes(x =x, y = y), color = "cyan4") +
  geom_line(data = temp2, aes(x =x, y = y), color = "chocolate3")+
  geom_vline(xintercept = 0, color = "red", alpha= .5)+
  geom_hline(yintercept = 0, color = "red", alpha= .5) +
  xlim(c(-0.8,10)) + ylim(c(-11,15)) +
  labs(title = "Intervalos para p*, con p = 2", 
       x = "p", y = "f(p)")+
  theme(plot.title = element_text(size=23))+
  theme(axis.title.y = element_text(size = 23 ))+
  theme(axis.title.x = element_text(size = 23 ))+
  theme(axis.text = element_text(size = 20))

# f(p) con un valor máximo de tres eigenvalores

temp3 <- puntos1+puntos2+puntos3
temp3 <- temp3 %>% mutate(x = x/3)

plot3 <- ggplot() +
  geom_rect(aes(xmin = 8/5, xmax = 4/1.5,   ymin = -Inf, ymax = Inf),  
            fill = "red", alpha = .3)+
  geom_line(data = temp3, aes(x =x, y = y), color = "black") +
  geom_vline(xintercept = 0, color = "red", alpha= .5)+
  geom_hline(yintercept = 0, color = "red", alpha= .5) +
  xlim(c(-0.8,10)) + ylim(c(-11,20)) +
  labs(title = "Intervalos para p*, con p = 3", 
       x = "p", y = "f(p)")+
  theme(plot.title = element_text(size=23))+
  theme(axis.title.y = element_text(size = 23 ))+
  theme(axis.title.x = element_text(size = 23 ))+
  theme(axis.text = element_text(size = 20))


png("/Users/salvadorgarcia/Repositories/Escrito/tesis/Figures/Chapter2_grid3eigen_interv.png", width = 600, height = 850)
grid.arrange(plot1,plot2,plot3,ncol = 1)
dev.off()



# 3) Segunda cota ---------------------------------------------------------

# f(p) con un valor máximo de un eigenvalor
plot1 <- ggplot() +
  geom_rect(aes(xmin = 8/5, xmax = 8/1.5,   ymin = -Inf, ymax = Inf),  
            fill = "red", alpha = .3)+
  geom_line(data = puntos1[201:2001, ], aes(x =x, y = y), color = "firebrick4") +
  geom_line(data = puntos2[81:201,], aes(x =x, y = y), color = "dodgerblue4")+
  geom_line(data = puntos3[0:81,], aes(x = x, y = y), color = "forestgreen") + 
  geom_vline(xintercept = 0, color = "red", alpha= .5)+
  geom_hline(yintercept = 0, color = "red", alpha= .5) +
  xlim(c(-0.8,10)) + ylim(c(-11,11)) +
  labs(title = "Intervalos para p*, con p = 1", 
       x = "p", y = "f(p)")+
  theme(plot.title = element_text(size=23))+
  theme(axis.title.y = element_text(size = 23 ))+
  theme(axis.title.x = element_text(size = 23 ))+
  theme(axis.text = element_text(size = 20)) 


# f(p) con un valor máximo de dos eigenvalores
temp1 <- puntos3[0:116, ] +puntos2[0:116, ]
temp1 <- temp1 %>% mutate(x = x/2)
temp2 <- puntos2[116:2001, ] +puntos1[116:2001, ]
temp2 <- temp2 %>% mutate(x = x/2)

plot2 <- ggplot() +
  geom_rect(aes(xmin = 14/7.5, xmax = 14/4,   ymin = -Inf, ymax = Inf),  
            fill = "red", alpha = .3)+
  geom_line(data = temp1, aes(x =x, y = y), color = "cyan4") +
  geom_line(data = temp2, aes(x =x, y = y), color = "chocolate3")+
  geom_vline(xintercept = 0, color = "red", alpha= .5)+
  geom_hline(yintercept = 0, color = "red", alpha= .5) +
  xlim(c(-0.8,10)) + ylim(c(-11,15)) +
  labs(title = "Intervalos para p*, con p = 2", 
       x = "p", y = "f(p)")+
  theme(plot.title = element_text(size=23))+
  theme(axis.title.y = element_text(size = 23 ))+
  theme(axis.title.x = element_text(size = 23 ))+
  theme(axis.text = element_text(size = 20))

# f(p) con un valor máximo de tres eigenvalores

temp3 <- puntos1+puntos2+puntos3
temp3 <- temp3 %>% mutate(x = x/3)

plot3 <- ggplot() +
  geom_rect(aes(xmin = 1.95, xmax = 2.05,   ymin = -Inf, ymax = Inf),  
            fill = "red", alpha = .3)+
  geom_line(data = temp3, aes(x =x, y = y), color = "black") +
  geom_vline(xintercept = 0, color = "red", alpha= .5)+
  geom_hline(yintercept = 0, color = "red", alpha= .5) +
  xlim(c(-0.8,10)) + ylim(c(-11,20)) +
  labs(title = "Intervalos para p*, con p = 3", 
       x = "p", y = "f(p)")+
  theme(plot.title = element_text(size=23))+
  theme(axis.title.y = element_text(size = 23 ))+
  theme(axis.title.x = element_text(size = 23 ))+
  theme(axis.text = element_text(size = 20))


png("/Users/salvadorgarcia/Repositories/Escrito/tesis/Figures/Chapter2_grid3eigen_interv2.png", width = 600, height = 850)
grid.arrange(plot1,plot2,plot3,ncol = 1)
dev.off()












