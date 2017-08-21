# 1) Load libraries ----------------------------------------
set.seed(12345)
library(tiff) # package that reads *.tiff images 
library(tidyr) # package to efficiently "tidy" data
library(dplyr) # package of grammar of data manipulation
library(ProjectTemplate) # Template data analysis project
library(FactoMineR) # Package for multivariate analysis

# 2) Read databases ----------------------------------------
# Get the names of the files
files <- (Sys.glob("~/Downloads/imgs/State Farm/*.jpg"))

# Create an array that contains all the images paths 
nombres.personas <- apply(as.matrix(files),1,function(x){
  gsub("data/Jaffe/","",x)  
})

# Data.frame with info about each image (ID, name, class)
guia.personas.df <- data.frame(nombres.personas,
                               nombres.personas) %>%
  separate(nombres.personas.1, c("persona", 
                                 "postura", 
                                 "ID", 
                                 "extension")) %>%
  dplyr::select(-extension,
                -ID) %>%
  mutate(ID = 1:213, 
         nombres.personas = as.character(nombres.personas),
         persona = factor(persona),
         postura = factor(postura))

# List of length 213 that contains an image in each element
lista.completa.imagenes <- lapply(files, function(x){  
  imagen <- readTIFF(x)
  if(length(dim(imagen)) == 3){
    matrix <- as.vector(imagen[,,1])
  }else{
    matrix <- as.vector(imagen)
  }
})

# Data.frame, each column is an image
base.imagenes <- data.frame(lista.completa.imagenes)
names(base.imagenes) <- guia.personas.df$nombres.personas

# 2 matrices: In base.pix.ind.1, the columns are the images
#             In base.ind.pix.1, the rows are the images            
base.pix.ind.1 <- data.frame(base.imagenes)
base.ind.pix.1 <- data.frame(t(base.imagenes)) %>% 
  mutate(grupo = names(base.imagenes)) %>% 
  separate(grupo, c("grupo", 
                    "postura", 
                    "ID", 
                    "extension")) %>% 
  dplyr::select(-postura, 
                -ID, 
                -extension) %>% 
  mutate(ID = 1:nrow(.))

# 3) Explorar bases de datos -------------------------------
# sizes of the matrices
dim(base.pix.ind.1) # 65,536 rows and 213 columns
dim(base.ind.pix.1) # 213 rows and 65,536 columns

# save in cache the data.base
base.jaffe <- base.ind.pix.1
cache("base.jaffe")

# 4) Train and test  ---------------------------------------
# Size of images per class
n.train <- 5

# Sample $n.train$ imager per class
train.id <- base.jaffe %>% 
  group_by(grupo) %>% 
  sample_n(n.train) %>% 
  data.frame()

# Train and Test data.frames
train <- base.jaffe[train.id$ID, ]
test <- base.jaffe[-train.id$ID, ]

# Percentaje of images in train dataset
nrow(train)/(nrow(train)+nrow(test))

# 5) PCA ---------------------------------------------------

# PCA of JAFFE dataset (each row is an image)
# 52 colums contains ~95% of the total variance
system.time(pca.train <- PCA(train %>% 
                               dplyr::select(-grupo, -ID), 
                             graph = F,
                             scale.unit = F,
                             ncp = 49)) 

# Matrix of loadings: 65,536 rows, 49 columns
loadings <- sweep(pca.train$var$coord,2,
                  sqrt(pca.train$eig[1:ncol(pca.train$var$coord),1]),
                  FUN="/")

# Principal components 
train.p <- pca.train$ind$coord %>% data.frame() %>% 
  dplyr::mutate(grupo = train$grupo)

# [Revision] Reconstuyendo primer componente 
as.matrix(scale(train %>% dplyr::select(-grupo, -ID), 
                center = apply(train %>%
                                 dplyr::select(-grupo, -ID), 2,mean), 
                scale = F)) %*% loadings[,1] %>% head

pca.train$ind$coord[,1] %>% head

# Projection of the test dataset with the train loadings 
# (centered with mean of train dataset)
system.time(test.p <- as.matrix(scale(test %>% 
                                        dplyr::select(-grupo, -ID), 
                                      center = apply(train %>% 
                                                       dplyr::select(-grupo, -ID), 2,mean), 
                                      scale = F)) %*% loadings %>% 
              data.frame() %>% 
              mutate(grupo = test$grupo))

JAFFE.test <- test.p
JAFFE.train <- train.p
cache("JAFFE.test")
cache("JAFFE.train")

rm(list = ls())
