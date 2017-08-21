# 1) Data y proyeccion size --------------------------------
library(ProjectTemplate)
reload.project()

numComp.Jaffe <- c(10,13,16,19,22,25,28,31,
                   34,37,40,43,46,49) 
numComp.Mnist <- c(2,5,10,15,20,40,60,80,
                   100,120,140,160,180,193)

comp.error.MNIST<-comparacion.modelos(train.p = MNIST.train,
                                 test.p = MNIST.test, 
                                 dataset = "MNIST", 
                                 numComp = numComp.Mnist,
                                 fortran = FALSE)

comp.error.JAFFE<-comparacion.modelos(train.p = JAFFE.train,
                                  test.p = JAFFE.test, 
                                  dataset = "JAFFE", 
                                  numComp = numComp.Jaffe,
                                  fortran = FALSE)
# 2) Graphs ------------------------------------------------

graficar.comparacion(datos = comp.error.MNIST, 
                     numComp = numComp.Mnist, 
                     label = "MNIST")

graficar.comparacion(datos = comp.error.JAFFE, 
                     numComp = numComp.Jaffe, 
                     label = "JAFFE")