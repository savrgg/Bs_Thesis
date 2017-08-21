# 1) Load libraries ----------------------------------------
set.seed(12345)
library(plyr) # Tools for split, applying and combine data
library(dplyr) # package of grammar of data manipulation
library(tidyr) # package to efficiently "tidy" data
library(FactoMineR) # Package for multivariate analysis
library(ggplot2) # Graphing package "Grammar of Graphics"
library(gridExtra) # Package to work with grid graphics

# 1) Generate train and test from MNIST (run once) ---------
# darch::readMNIST("data/MNIST/")

# 2) Read MNIST data ---------------------------------------
# Original datasets
load("data/MNIST/train.RData")
load("data/MNIST/test.RData")

# Matrix with images in the rows and the variable in columns
numeros.ind.pix = rbind(data.frame(trainData, 
                  grupo = factor(apply(trainLabels[,], 1,  
                      function(x){ which(x == 1)-1 }))),
                  data.frame(testData, 
                      grupo = factor(apply(testLabels[,], 1,  
                      function(x){ which(x == 1)-1 })))) %>% 
  mutate(ID = 1:nrow(.))
base.mnist <- numeros.ind.pix
cache("base.mnist")
rm(testData, testLabels, trainData, trainLabels)

# 3) Train and test sets -----------------------------------
# Size of images per class
n.train <- 400

# Sample $n.train$ imager per class
train.id <- numeros.ind.pix %>% 
  group_by(grupo) %>% 
  sample_n(n.train) %>% 
  data.frame()

# Train and Test data.frames                    
train <- numeros.ind.pix[train.id$ID, ]
test <- numeros.ind.pix[-train.id$ID, ]

# Percentaje of images in train dataset
nrow(train)/(nrow(train)+nrow(test))

# 4) PCA ---------------------------------------------------
# PCA of MNIST dataset (each row is an image)
# 193 colums contains ~95% of the total variance
system.time(pca.train <- PCA(train %>% 
                               dplyr::select(-grupo, -ID), 
                             graph = F,
                             scale.unit = F,
                             ncp = 193))

# Matrix of loadings: 784 rows, 193 columns 
loadings <- sweep(pca.train$var$coord,2,  # 784 x 193
        sqrt(pca.train$eig[1:ncol(pca.train$var$coord),1]),
        FUN="/")

# Principal components 
train.p <- pca.train$ind$coord %>% data.frame() %>% 
  dplyr::mutate(grupo = train$grupo)

# [Revision] reconstuyendo primer componente 
as.matrix(scale(train %>% dplyr::select(-grupo, -ID), 
                center = apply(train %>% 
                      dplyr::select(-grupo, -ID), 2,mean), 
                scale = F)) %*% loadings[,1] %>% head

pca.train$ind$coord[,1] %>% head

# Projection of the test dataset with the train loadings 
# (centered with mean of train dataset)
system.time(test.p <- as.matrix(scale(test %>% 
            dplyr::select(-grupo, -ID), # 69,000 x 193
                                      
            center = apply(train %>% # centers of train
                    dplyr::select(-grupo, -ID), 2,mean), 
                          scale = F)) %*% loadings %>% 
              data.frame() %>% 
              mutate(grupo = test$grupo))
dim(test.p)
dim(train.p)
MNIST.test <- test.p
MNIST.train <- train.p
cache("MNIST.test")
cache("MNIST.train")

