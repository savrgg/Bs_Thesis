prolifiling.model <- function(train.p, test.p, 
                                dataset,numComp){

print(paste("====Realizando profiling de la base",dataset))
lda.time <- lapply(1:14, function(x){
  print(paste("Iteración con: ", 
              numComp[x], " componentes."))
  
  lda.iter <- sapply(1:5, function(y){
    system.time(modelo.iterativo <- 
                  lda.iter.ef(train.p = train.p, 
                              test.p = test.p, 
                              espacio.proy = numComp[x]))
  }) %>% data.frame() %>% t() %>% data.frame() %>% 
    dplyr::select(elapsed) %>% 
    dplyr::summarise(mean = mean(elapsed))
  
  logistico <- sapply(1:5, function(y){
    system.time(modelo <- multinom(grupo~., 
          data = train.p[,c(1:numComp[x], dim(train.p)[2])], 
                                   MaxNWts = 10000))
  }) %>% data.frame() %>% t() %>% data.frame() %>% 
    dplyr::select(elapsed) %>% 
    dplyr::summarise(mean = mean(elapsed))
  
  lda.original <- sapply(1:5, function(y){
    system.time(modelo <- lda(grupo ~ .,
                train.p[,c(1:numComp[x], dim(train.p)[2])], 
                        prior = c(1,1,1,1,1,1,1,1,1,1)/10))
  })  %>% data.frame() %>% t() %>% data.frame() %>% 
    dplyr::select(elapsed) %>% 
    dplyr::summarise(mean = mean(elapsed))
  
  list(lda.iter, logistico, lda.original)
})

data.frame(unlist(lda.time)) %>% 
  mutate(var = rep(c("lda.iter", "logis", "lda.orig"), 14),
         ID = rep(1:(nrow(.)/3), each = 3)) %>% 
  setNames(c("time", "var", "ID")) %>% 
  spread(var, time)
}


graficar.profiling <- function(times.profiling, numComp,
                               dataset){
plot1 <- times.profiling %>% 
  mutate(numComp = numComp) %>% 
  gather(variable, value, lda.iter:logis) %>% 
  ggplot(aes(x = numComp , y = value,
             group = variable, color = variable)) +
  geom_point() + geom_line()+
  labs(title = "Tiempo de los modelos", 
       x = "Espacio de proyección", 
       y="Tiempo en (s)")
png(filename = paste(getwd(), "/graphs/Chapter4_profiling", 
      dataset, ".png", sep =""), width = 600, height = 400)
grid.arrange(plot1, ncol = 1)
dev.off()
}

