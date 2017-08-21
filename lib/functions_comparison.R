comparacion.modelos <- function(train.p, test.p, 
                                dataset,numComp, 
                                fortran = FALSE){
  # lda iterativo
  print(paste("===Corriendo el modelo con la base",dataset))
  
  lda.iter <- sapply(1:14, function(x){
    print(paste("Iteración con: ", 
                numComp[x], 
                " componentes."))
    
    # Run model with different number of components
    if(fortran == FALSE){
    modelo.iterativo <- lda.iter.ef(train.p = train.p, 
                                    test.p = test.p, 
                                  espacio.proy = numComp[x])
    }else{
      modelo.iterativo <- lda.iter.fortran(train.p = train.p, 
                                      test.p = test.p, 
                                  espacio.proy = numComp[x])
    }
    # Data.frame with information of  (\rho and f(\rho))
    iter <- sapply(1:length(modelo.iterativo$iteraciones), 
                   function(x){
          data.frame(modelo.iterativo$iteraciones[[x]]$rho,
                  modelo.iterativo$iteraciones[[x]]$frho)%>% 
                setNames(c("rho", "frho")) %>% data.frame()
                   }) %>% t() %>% data.frame() %>% 
      mutate(iter = 1:nrow(.), 
             rho = unlist(rho),
             frho = unlist(frho))
    iter <- iter %>% data.frame() %>% 
      dplyr::mutate(frho2 = c(iter$frho[2:nrow(.)],NA ),
                    rho2 = c(iter$rho[2:nrow(.)], NA))
    
    # Proyected data into a 20-dimensional space
    V <- modelo.iterativo$iteraciones[[
      length(modelo.iterativo$iteraciones)]]$V
    
    train.proyectados <- data.frame(as.matrix(train.p %>% 
                          dplyr::select(-grupo)) %*% V) %>%
      mutate(grupo = train.p$grupo)
    
    test.proyectados <- data.frame(as.matrix(test.p %>% 
                          dplyr::select(-grupo)) %*% V) %>%
      mutate(grupo = test.p$grupo)
    
    # K-nn model
    
    print("Iniciando modelo knn")
  modelo <- knn(train = train.proyectados[,c(1:numComp[x])], 
                test = test.proyectados[, c(1:numComp[x])], 
                  cl = train.proyectados$grupo, k = 3)
    pred <- data.frame(original = test.proyectados$grupo, 
                       predicho = modelo)
    
    pred <- pred %>% mutate(verdad = original != predicho)
    sum(pred$verdad)/nrow(pred)
  })
  
  modelo.logistico <- lapply(1:14, function(x){
    modelo <- multinom(grupo~., 
          data = train.p[,c(1:numComp[x], dim(train.p)[2])],
                       MaxNWts = 10000)
    predichos <- predict(modelo, 
                         newdata = test.p[,c(1:numComp[x])])
    pred <- data.frame(original = test.p$grupo, 
                       predicho = predichos)
    pred <- pred %>% mutate(verdad = original != predicho)
    sum(pred$verdad)/nrow(pred)
  })
  
  lda.original <- lapply(1:14, function(x){
    modelo<- lda(grupo ~ ., 
                 train.p[,c(1:numComp[x], 
                            dim(train.p)[2])], 
                 prior = c(1,1,1,1,1,1,1,1,1,1)/10)
  predichos <- predict(modelo, test.p[,1:numComp[x]])$class
    pred <- data.frame(original = test.p$grupo, 
                       predicho = predichos)
    pred <- pred %>% mutate(verdad = original != predicho)
    sum(pred$verdad)/nrow(pred)
  })
  
  data.frame(lda.iter = unlist(lda.iter), 
             logistico = unlist(modelo.logistico), 
             lda.original = unlist(lda.original))
}

graficar.comparacion <- function(datos, numComp, label){
  plot1 <- datos %>% 
    #write.table(sep = ';', pipe('pbcopy'), row.names= F)
    mutate(numComp = numComp) %>% 
    gather(variable, value, lda.iter:lda.original) %>% 
    ggplot(aes(x = numComp , y = 1-value, 
      group = factor(variable), color = factor(variable))) +
    geom_point() + geom_line()+
    labs(title = "Tasa de reconocimiento conforme 
         número el espacio de proyección", 
         x = "número de entrenamiento por grupo", 
         y="tasa reconocimiento")
  
  png(filename =paste(getwd(), "/graphs/Chapter4_comp_", 
        label, ".png", sep =""), width = 600,  height = 400)
  grid.arrange(plot1, ncol =1)
  dev.off()
}