# 1) Load libraries ----------------------------------------
set.seed(12345)
library(plyr) # Tools for split, applying and combine data
library(dplyr) # package of grammar of data manipulation
library(tidyr) # package to efficiently "tidy" data
library(FactoMineR) # Package for multivariate analysis
library(ggplot2) # Graphing package "Grammar of Graphics"
library(gridExtra) # Package to work with grid graphics
library(class) # Functions for classification, including knn
library(nnet) # Software for multinomial linear models
library(MASS) # "Applied Statistics with S"



# 1) Fisher eficiente ---------------------------------
lda.iter.ef <- function (test.p, 
                         train.p, 
                         espacio.proy,
                         prior = proportions,
                         tol = 1e-10) {
  
  # Prepare matrix of data
  x <- train.p %>% dplyr::select(-grupo)
  x <- as.matrix(x)
  
  # Get dimensions
  n <- nrow(x)
  p <- ncol(x)
  
  # Get number of groups and the levels
  g <- as.factor(train.p$grupo)
  lev <- lev1 <- levels(g)
  
  # Get the number of images per class
  counts <- as.vector(table(g))
  proportions <- counts/n
  ng <- length(proportions)
  names(prior) <- names(counts) <- lev1
  
  # Group means and total mean
  group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)
  all.means <- as.matrix(colMeans(x)) %>% t()
  
  # Scatter Matrices 
  #Intra class
  B <- t(x - group.means[g, ]) %*% (x - group.means[g, ]) 
  # Between class
  A <- (t(group.means)-matrix(rep(all.means,10),ncol=10))%*% 
    diag(counts) %*% 
    t(t(group.means)-matrix(rep(all.means,10),ncol=10)) 
  
  # Initialization
  rhoAnt = -1000000
  iter=1
  dimension = espacio.proy
  # rho = (cota.inf + cota.sup)/2
  rho = 0
  frho = sum(eigen(A-rho*B)$values[1:dimension])
  V = eigen(A-rhoAnt*B)$vectors[,(1:dimension)]
  
  # list of results
  a=list()
  a[[1]] <- list(iter = 1, rho = rho, frho = frho, V = V) 
  
  # Iterative method (use of efficient irlba method)
  while(iter < 50 & abs(rho-rhoAnt)>tol){
    print(paste("iteracion: ",as.character(iter)))
    rhoAnt = rho
    iter=iter+1  
    V = eigen(A-rhoAnt*B)$vectors[,(1:dimension)] # Lanczos
    rho = sum(diag(t(V) %*% A %*% V)) /
    sum(diag(t(V) %*% B %*% V))
    frho = sum(eigen(A-rho*B)$values[1:dimension])
    a[[iter]] <- list(iter = iter, rho = rho,
                      frho = frho, V = V)
  }
  
  # Getting the projection matrix
  scaling <- V
  dimnames(group.means)[[2L]] <- colnames(x)
  cl <- match.call()
  cl[[1L]] <- as.name("lda.iter")
  structure(list(iteraciones = a, 
                 prior = prior, 
                 counts = counts, 
                 means = group.means, 
                 scaling = scaling, 
                 lev = lev, 
                 N = n, 
                 call = cl, 
                 class = "lda.iter"))
}

# 2) Fisher para pruebas ------------------------------
lda.iter <- function (test.p, 
                      train.p, 
                      espacio.proy,
                      prior = proportions, 
                      tol = 1e-10,
                      dim = 10) {
  
  # Prepare matrix of data
  x <- train.p %>% dplyr::select(-grupo) %>% as.matrix()
  
  # Get dimensions
  n <- nrow(x)
  p <- ncol(x)
  
  # Get number of groups and the levels
  g <- as.factor(train.p$grupo)
  lev <- lev1 <- levels(g)
  
  # Get the number of images per class
  counts <- as.vector(table(g))
  proportions <- counts/n
  ng <- length(proportions)
  names(prior) <- names(counts) <- lev1
  
  # Group means and total mean
  group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)
  all.means <- as.matrix(colMeans(x)) %>% t()
  
  # Scatter Matrices 
  #Intra class
  B <- t(x - group.means[g, ]) %*% (x - group.means[g, ]) 
  # Between class
  A <- (t(group.means)-matrix(rep(all.means,dim),ncol=dim))%*% 
    diag(counts) %*% 
    t(t(group.means)-matrix(rep(all.means,dim),ncol=dim)) 
  
  # [not run] Intra class + Between class = Total variance
  #diag(A+B) %>% head
  #diag(699*(as.matrix(x) %>% cov))
  
  # [not run] EigenValues Time
  #(eigen(B))$values
  #(eigen(A))$values
  
  # [not run] Boundaries
  cota.inf <- sum((eigen(A))$values[1:espacio.proy])/
    sum((eigen(B))$values[1:espacio.proy])
  cota.sup <- sum((eigen(A))$values[1:espacio.proy])/
sum((eigen(B))$values[(dim(A)[1]+1-espacio.proy):dim(A)[1]])
  
  # Initialization
  rhoAnt = -1000000
  iter=1
  dimension = espacio.proy
  rho = (cota.inf + cota.sup)/2
  
  frho = sum(eigen(A-rho*B)$values[1:dimension])
  V = eigen(A-rhoAnt*B)$vectors[,(1:dimension)]
  
  # list of results
  a=list()
  a[[1]] <- list(iter = 1, rho = rho, frho = frho, V = V) 
  
  # Iterative method (use of efficient irlba method)  
  while(iter < 50 & abs(rho-rhoAnt)>tol){
    print(paste("iteracion: ",as.character(iter)))
    rhoAnt = rho
    iter=iter+1  
    V = eigen(A-rhoAnt*B)$vectors[,(1:dimension)]
rho = sum(diag(t(V)%*%A %*% V)) /sum(diag(t(V) %*% B %*% V))
    frho = sum(eigen(A-rho*B)$values[1:dimension])
a[[iter]] <-list(iter = iter, rho = rho, frho = frho, V = V)
  }
  
  # Getting the projection matrix
  scaling <- V
  dimnames(group.means)[[2L]] <- colnames(x)
  cl <- match.call()
  cl[[1L]] <- as.name("lda.iter")
  structure(list(iteraciones = a, 
                 prior = prior, 
                 counts = counts, 
                 means = group.means, 
                 scaling = scaling, 
                 lev = lev, 
                 N = n, 
                 call = cl, 
                 cota.inf = cota.inf, 
                 cota.sup = cota.sup, 
                 class = "lda.iter"))
}

# 3) LDA original -------------------------------------
lda.original <- function (x, 
                          grouping, 
                          prior = proportions, 
                          tol = 1e-04, 
                          nu = 5) {
  
  # Prepare matrix of data
  x <- as.matrix(x)
  n <- nrow(x)
  
  # Get dimensions
  p <- ncol(x)
  g <- as.factor(grouping)
  
  # Get number of groups and the levels
  lev <- lev1 <- levels(g)
  counts <- as.vector(table(g))
  
  # Get the number of images per class
  proportions <- counts/n
  ng <- length(proportions)
  names(prior) <- names(counts) <- lev1
  
  # Group means 
  group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)
  f1 <- sqrt(diag(var(x - group.means[g, ])))
  scaling <- diag(1/f1, , p)
  
  # Calculate and proyect
  fac <-  1/(n - ng)
  X <- sqrt(fac) * (x - group.means[g, ]) %*% scaling
  X.s <- svd(X, nu = 0L)
  rank <- sum(X.s$d > tol)
  scaling <- scaling %*% X.s$v[, 1L:rank] %*% 
    diag(1/X.s$d[1L:rank], , rank)
  xbar <- colSums(prior %*% group.means)
  fac <-1/(ng - 1)
  X <- sqrt((n * prior) * fac) * scale(group.means, 
        center = xbar, scale = FALSE) %*% scaling
  X.s <- svd(X, nu = 0L)
  rank <- sum(X.s$d > tol * X.s$d[1L])
  scaling <- scaling %*% X.s$v[, 1L:rank]
  
  # Export
  dimnames(scaling) <- list(colnames(x), paste("LD",
          1L:rank, sep = ""))
  dimnames(group.means)[[2L]] <- colnames(x)
  cl <- match.call()
  cl[[1L]] <- as.name("lda")
  structure(list(prior = prior, counts = counts, 
                 means = group.means, 
                 scaling = scaling, lev = lev, 
                 svd = X.s$d[1L:rank], N = n, 
                 call = cl), class = "lda")
}

# 4) Lanczos Fortran -----------------------------------
# Give r0, A
lanczos.iter <- function(A){
  Dim = dim(A)[2]
  r0 = matrix(rep(0,Dim), ncol = 1)
  for(i in 1:Dim){
    r0[i] = runif(1, 0, 1) 
  }
r0 = r0 / sqrt(sum(r0^2))
r = list()
b = list()
a = list()
q = list()

q[[1]] = 0 # q0
r[[1]] = r0 #r0
b[[1]] = sqrt(sum(r[[1]]^2)) # b1
q[[2]] = as.matrix(r[[1]]/b[[1]], ncol =D) #q1
a[[1]] = t(q[[2]]) %*% A %*% q[[2]] #a1
q[[1]]
j <- 2
q = cbind(q[[1]], q[[2]])
while(b[[j-1]] > .0000001 | j < Dim+1){
  z = A %*% q[,j]
  z = z - q %*% (t(q) %*% z)
  z = z - q %*% (t(q) %*% z)
  b[[j]] = sqrt(sum(z^2))
  q.temp = z/b[[j]]
  q <- cbind(q, q.temp)
  a[[j]] = t(q[,j+1]) %*% A %*% q[,j+1]
  j = j+1
  print(j)
}

dyn.load("/usr/lib/liblapack.dylib")

Jobz <- as.character('V')#Get The eigenval and Eigenvector
N <- as.integer(Dim)#Matrix Dimension
D <- unlist(a)[1:Dim]#Diagonal
E <- unlist(b)[2:(Dim)]#Upper (and lower) diagonal
Z <- array(0.0,N*N)#storage for eigenvectors
Ldz <- N#Leading dimension of Z (in our case number of rows)
Work <- array(0.0,2*N)#Storage for processing
info <- as.integer(0)#Info flag

res<-.Fortran("dstev", Jobz, N, D, E, Z, Ldz, Work, info)

evalues <- res[[3]]
evectors <- matrix(res[[5]],N,N)
list(evalues, evectors)
}

# 5) Fisher eficiente ---------------------------------
lda.iter.fortran<- function (test.p = test.p, 
                         train.p = train.p, 
                         espacio.proy,
                         prior = proportions,
                         tol = 1e-10) {
  
  # Prepare matrix of data
  x <- train.p %>% dplyr::select(-grupo)
  x <- as.matrix(x)
  
  # Get dimensions
  n <- nrow(x)
  p <- ncol(x)
  
  # Get number of groups and the levels
  g <- as.factor(train.p$grupo)
  lev <- lev1 <- levels(g)
  
  # Get the number of images per class
  counts <- as.vector(table(g))
  proportions <- counts/n
  ng <- length(proportions)
  names(prior) <- names(counts) <- lev1
  
  # Group means and total mean
  group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)
  all.means <- as.matrix(colMeans(x)) %>% t()
  
  # Scatter Matrices 
  #Intra class
  B <- t(x - group.means[g, ]) %*% (x - group.means[g, ]) 
  # Between class
  A <- (t(group.means)-matrix(rep(all.means,10),ncol=10))%*% 
    diag(counts) %*% 
    t(t(group.means)-matrix(rep(all.means,10),ncol=10)) 
  
  # Initialization
  rhoAnt = -1000000
  iter=1
  dimension = espacio.proy
  # rho = (cota.inf + cota.sup)/2
  rho = 0
  frho = sum(eigen(A-rho*B)$values[1:dimension])
  V = eigen(A-rhoAnt*B)$vectors[,(1:dimension)]
  
  # list of results
  a=list()
  a[[1]] <- list(iter = 1, rho = rho, frho = frho, V = V) 
  
  # Iterative method (use of efficient irlba method)
  while(iter < 50 & abs(rho-rhoAnt)>tol){
    print(paste("iteracion: ",as.character(iter)))
    rhoAnt = rho
    iter=iter+1  
    
  V = lanczos.iter(A-rhoAnt*B)[[2]][,(1:dimension)]#Lanczos
    rho = sum(diag(t(V) %*% A %*% V)) /
      sum(diag(t(V) %*% B %*% V))
    frho = sum(lanczos.iter(A-rhoAnt*B)[[1]][1:dimension])
    a[[iter]] <- list(iter = iter, rho = rho,
                      frho = frho, V = V)
  }
  
  # Getting the projection matrix
  scaling <- V
  dimnames(group.means)[[2L]] <- colnames(x)
  cl <- match.call()
  cl[[1L]] <- as.name("lda.iter")
  structure(list(iteraciones = a, 
                 prior = prior, 
                 counts = counts, 
                 means = group.means, 
                 scaling = scaling, 
                 lev = lev, 
                 N = n, 
                 call = cl, 
                 class = "lda.iter"))
}