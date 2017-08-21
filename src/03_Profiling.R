# 1) Data y proyeccion size --------------------------------
library(ProjectTemplate)
reload.project()

# 1) Componentes ------------------------------------------
numComp.Jaffe <- c(10,13,16,19,22,25,28,31,
                   34,37,40,43,46,49) 
numComp.Mnist <- c(2,5,10,15,20,40,60,80,
                   100,120,140,160,180,193)
# 2) Profiling ----------------------------------------
prof.Mnist <- prolifiling.model(train.p = MNIST.train, 
                                test.p = MNIST.test,
                                dataset = "MNIST",
                                numComp = numComp.Mnist)
prof.Jaffe <- prolifiling.model(train.p = JAFFE.train, 
                                test.p = JAFFE.test,
                                dataset = "JAFFE",
                                numComp = numComp.Jaffe)

# 3) Graphs -------------------------------------------
graficar.profiling(times.profiling = prof.Mnist, 
                   numComp = numComp.Mnist, 
                   dataset = "MNIST")
graficar.profiling(times.profiling = prof.Jaffe, 
                   numComp = numComp.Jaffe, 
                   dataset = "JAFFE")