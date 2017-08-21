# funciones prueba 20 componentes

plot.comp <- function(V1, label, train.p, test.p){
  train.proyectados <- data.frame(as.matrix(train.p %>% 
                          dplyr::select(-grupo)) %*% V1) %>%
    mutate(grupo = train.p$grupo)
  
  test.proyectados <- data.frame(as.matrix(test.p %>% 
                          dplyr::select(-grupo)) %*% V1) %>%
    mutate(grupo = test.p$grupo)
  
  a1 = ggplot(train.proyectados) + 
    geom_point(aes(x=X1, y=X2, 
                   group = grupo, color = grupo), 
               size=5, alpha = .2) + 
    geom_point(aes(x=X1, y=X2, 
                   group = grupo, color = grupo))+ 
    labs(title = label)
  a2 = ggplot(train.proyectados) + 
    geom_point(aes(x=X3, y=X4, 
                   group = grupo, color = grupo), 
               size=5, alpha = .2) + 
    geom_point(aes(x=X3, y=X4, 
                   group = grupo, color = grupo))+ 
    labs(title = label)
  list(a1,a2)
}
plot.ult <- function(V1, label, train.p, test.p){
  train.proyectados <- data.frame(as.matrix(train.p %>% 
                          dplyr::select(-grupo)) %*% V1) %>%
    mutate(grupo = train.p$grupo)
  
  test.proyectados <- data.frame(as.matrix(test.p %>% 
                          dplyr::select(-grupo)) %*% V1) %>%
    mutate(grupo = test.p$grupo)
  a7 =  ggplot(train.proyectados) + 
    geom_point(aes(x=X17, y=X18, 
                   group = grupo, color = grupo), 
               size=5, alpha = .2) + 
    geom_point(aes(x=X17, y=X18, 
                   group = grupo, color = grupo))+ 
    labs(title = label)
  
  a8 =  ggplot(train.proyectados) + 
    geom_point(aes(x=X19, y=X20, 
                   group = grupo, color = grupo), 
               size=5, alpha = .2) + 
    geom_point(aes(x=X19, y=X20, 
                   group = grupo, color = grupo))+ 
    labs(title = label)
  list(a7,a8)
}
prueba.20comp <- function(train.p, test.p, dataset, dim=10){
  # Iterative LDA, projected to a 20-dimentional space
  modelo.iterativo <- lda.iter(train.p = train.p, 
                               test.p = test.p,
                               espacio.proy = 20,
                               dim = dim)
  # Data.frame with information of (\rho and f(\rho))
  iter <- sapply(1:length(modelo.iterativo$iteraciones), 
                 function(x){
          data.frame(modelo.iterativo$iteraciones[[x]]$rho,
                modelo.iterativo$iteraciones[[x]]$frho)  %>% 
                     setNames(c("rho", 
                            "frho")) %>% data.frame()}) %>% 
    t() %>% data.frame() %>% 
    mutate(iter = 1:nrow(.), 
           rho = unlist(rho),
           frho = unlist(frho))
  iter <- iter %>% 
    dplyr::mutate(frho2 = c(iter$frho[2:nrow(.)],NA),
                  rho2 = c(iter$rho[2:nrow(.)], NA))
  
  # f(\rho) graph. It contains the iterations of the algorit
  plot <- iter %>% ggplot() +
    geom_point(aes(x = rho, y = frho))  + 
    geom_segment(aes(x = rho, y = frho, 
                     xend = rho, yend = frho2), 
                 color = "gray50") +
    geom_segment(aes(x = rho, y = frho2, 
                     xend = rho2, yend = frho2), 
                 arrow = arrow(length = unit(.3, "cm")), 
                 size = 1, 
                 color = "gray50") +
    labs(title = paste("Valor de la función f(rho) con 
      respecto al numero de iteraciones ", dataset,sep =""), 
         ylab = "f(rho)",
         xlab = "rho")
  # Save the image into a *.png file
  png(paste(getwd(), "/graphs/Chapter4_iteraciones_", 
      dataset, ".png", sep =""),width = 500, height = 500)
  grid.arrange(plot, ncol = 1)
  dev.off()
  
  # Proyected data into a 20-dimensional space
  V <- modelo.iterativo$iteraciones[[
    length(modelo.iterativo$iteraciones)]]$V
  
  r1 <- 1
  r2 <- round(length(modelo.iterativo$iteraciones)/3*1)
  r3 <- round(length(modelo.iterativo$iteraciones))
  
  # iteration1
  V1 <- modelo.iterativo$iteraciones[[r1]]$V
  plot1 <- plot.comp(V1, paste("Iteracion ",r1, sep=""),
                     train.p, test.p)
  
  # iteracion3
  V2 <- modelo.iterativo$iteraciones[[r2]]$V
  plot2 <- plot.comp(V2, paste("Iteracion ",r2, sep=""),
                     train.p, test.p)
  
  # iteracion9
  V3 <- modelo.iterativo$iteraciones[[r3]]$V
  plot3 <- plot.comp(V3, paste("Iteracion ",r3, sep=""),
                     train.p, test.p)
  
  png(paste(getwd(),"/graphs/Chapter4_ejemplo20componentes_",
        dataset, ".png", sep =""),width = 700, height = 800)
  grid.arrange(plot1[[1]],plot1[[2]],plot2[[1]],
               plot2[[2]],plot3[[1]],plot3[[2]], 
               ncol = 2)
  dev.off()
  
  # last components
  V4 <- modelo.iterativo$iteraciones[[r3]]$V
  plot4 <- plot.ult(V4, "Ultimas componentes", 
                    train.p, test.p)
  png(paste(getwd(),"/graphs/Chapter4_ultimasComponentes_",
        dataset, ".png", sep =""),height = 400, width = 700)
  grid.arrange(plot4[[1]],plot4[[2]], ncol = 2)
  dev.off()
  # knn model ---------------------------------------------
  V <- modelo.iterativo$iteraciones[[
    length(modelo.iterativo$iteraciones)]]$V
  
  train.proyectados <- data.frame(as.matrix(train.p %>% 
                          dplyr::select(-grupo)) %*% V) %>%
    mutate(grupo = train.p$grupo)
  
  test.proyectados <- data.frame(as.matrix(test.p %>% 
                          dplyr::select(-grupo)) %*% V) %>%
    mutate(grupo = test.p$grupo)
  
  train.proyectados %>% head
  knn.model <- knn(train = train.proyectados[,c(1:20)], 
                   test = test.proyectados[, c(1:20)], 
                   cl = train.proyectados$grupo, k = 3)
  
  pp <- data.frame(original = test.proyectados$grupo, 
                   predicho = knn.model)
  
  pp <- pp %>% mutate(verdad = original != predicho)
  
  error <- sum(pp$verdad)/nrow(pp)
  list(modelo.iterativo=modelo.iterativo,iter = iter, V = V, 
       error = error, knn.model = knn.model)
}