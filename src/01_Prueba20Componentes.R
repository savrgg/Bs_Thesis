library(ProjectTemplate)
reload.project()

# Jaffe.20 & Mnist.20  are 5 element lists:
# 1) iterative model
# 2) data frame with \rho, f\rho values
# 3) projection matrix V
# 4) test error
# 5) knn model

train.p <- otto.train
test.p <- otto.test
dataset <- "otto"


Otto.20 <- prueba.20comp(otto.train, otto.test, "Otto", dim = 9)
Jaffe.20 <- prueba.20comp(JAFFE.train, JAFFE.test, "JAFFE", dim = 10)
Mnist.20 <- prueba.20comp(MNIST.train, MNIST.test, "MNIST", dim = 10)

Otto.20$modelo.iterativo
Jaffe.20$modelo.iterativo
Mnist.20$modelo.iterativo