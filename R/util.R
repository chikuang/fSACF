#' @purpose: To store all the utility functions used in the calculation 

# Call spatial median -----------------------------------------------------
# source("./gervini/SpMed.R")
library(ggplot2)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             legend.position = "none")


# Spatial median ----------------------------------------------------------
gsj <- function(x, y){
  n <- nrow(x)
  m <- ncol(x)
  if (n == 1){
    x <-  t(x)
    y <- t(y)
    n <- nrow(x)
    m <- ncol(x)
  }
  
  if (n < 3)
    return(NULL)
  ind <- apply(x, 2, order)
  x <- apply(x, 2, sort)
  
  # Also sort the y accordingly using the indicator above
  for (j in 1:m) {
    y[ ,j] <- y[ind[ ,j], j]
  }
  
  a <- (x[3:n, ] - x[2:(n-1), ]) / (x[3:n, ] - x[1:(n-2), ])
  b <- (x[2:(n-1), ] - x[1:(n-2), ])/(x[3:n, ]-x[1:(n-2), ])
  c <- a^2 + b^2 + 1
  e <- a * y[1:(n-2), ] + b * y[3:n, ] - y[2:(n-1),]
  
  estvar <- colMeans(e^2/c)
  return(estvar)
}

SpMed <- function(tt, x, dtyp = 's'){
  tt <- as.matrix(tt, ncol = length(tt), nrow = 1)
  eps <- 2^(-52)
  # Input check
  if (nargs() < 2)
    stop("Not enough input variables")
  
  n <- nrow(x)
  m <- ncol(x)
  
  if (nrow(tt) > 1)
    tt <- t(tt)
  
  if (ncol(tt) != m)
    stop('Dimensions of T and X not compatible')
  
  if (!(dtyp != 'n' | dtyp != 's'))
    stop('Wrong input value for DTYP')
  
  # Inner-product matrix
  A <- 0.5 * (x[, 1:(m-1)] %*% diag(tt[2:m] - tt[1:(m-1)]) %*% t(x[, 1:(m-1)]) +
                x[, 2:m] %*% diag(tt[2:m] - tt[1:(m-1)]) %*% t(x[, 2:m])) 
  
  if( tolower(dtyp) == "n"){
    se2 <- gsj(kronecker(matrix(1, 1, n), tt), t(x))
    A1 <- A - diag(se2) * (tt[m] - tt[1]);
    if (min(eig(A1)>-1e-6))
      A <- A1
    else{
      print('Corrected inner-product matrix is not nonnegative definite \n Using uncorrected matrix instead ')
    }
  }
  
  ## Iterative minimization from sample mean
  w <- matrix(1, nrow = n, ncol = 1)/n # a very naive way
  norms <- sqrt(diag(A) + as.vector(t(w) %*% A %*% w) - 2 * A %*% w) # pay attention to the R syntax that it does not like to do arithmetic on a number to vector
  f <- sum(norms)
  err <- 1
  iter <- 0
  
  while( err > 1E-5 && iter < 50){
    iter <- iter + 1
    f0 <- f
    if (min(norms < eps)){
      i0 = find(norms < eps)
      w <- matrix(0, nrow = n, ncol = 1)
      w[i0] <- 1/length(i0)
    } else{
      w <- 1/norms
      w <- w/sum(w)
    }
    
    norms <- sqrt(diag(A) + as.vector(t(w) %*% A %*% w) - 2 * A %*% w) # same here
    f <- sum(norms)
    err <- abs(f/f0 - 1)
  }
  
  med <- t(w) %*% x
  return(list(med = med, w = w, norms = norms))
}



# Portmanteau test --------------------------------------------------------
# Calculate the Box-Pierce test
library(dplyr)
library(tibble)
calc_BP_test <- function(rho, H, alpha, N, cp){
  lapply(1:H, function(h){
    val <- N * sum(rho[1:h]^2)/cp^2
    pval <- pchisq(val, df = h, lower.tail = FALSE)
    data.frame(lag = h, val_bp = val, pval_bp = pval)
  }) %>% bind_rows()
}

# Calculate the Ljung-Box test 
calc_LB_test <- function(rho, H, alpha, N, cp){
  lapply(1:H, function(h){
    temp <- 0
    for(k in 1:h){
      temp <- temp + rho[k]^2 / (N-k)
    }
    val <- N * (N + 2) * sum(temp)/cp^2
    pval <- pchisq(val, df = h, lower.tail = FALSE)
    data.frame(lag = h, val_lb = val, pval_lb = pval)
  }) %>% bind_rows()
}


# Calculating the covariance function
calc_var <- function(xbr){
  Nt <- ncol(xbr)
  N <- nrow(xbr)
  res <- matrix(NA, Nt, Nt)
  for(s in 1:Nt){
    for(t in 1:Nt){
      val <- 0
      for(i in 1:N){
        val <- val + xbr[i, s] * xbr[i, t]
      }
      res[s, t] <- (val/N)^2
    }
  }
  sum(res)/(Nt-1)^2
}

# calculate variance when the input is a raw data
calc_var_raw <- function(x_raw){
  N <- nrow(x_raw)
  Nt <- ncol(x_raw)
  
  Spatial_med <- as.numeric(SpMed(tt = seq(0, 1, length.out = Nt), x_raw)$med)
  x_cen <- sapply(1:Nt, function(k){
    x_raw[, k] - Spatial_med[k]
  })
 
  my_norm <- calc_norm_matrix(x_cen)
  xbr <- x_cen/my_norm
  
  res <- matrix(NA, Nt, Nt)
  for(s in 1:Nt){
    for(t in 1:Nt){
      val <- 0
      for(i in 1:N){
        val <- val + xbr[i, s] * xbr[i, t]
      }
      res[s, t] <- (val/N)^2
    }
  }
  sum(res)/(Nt-1)^2
}

# Plot the portmanteau test for 1 simulation ------------------------------
plot_portmanteau <- function(df){
  df %>% dplyr::select(lag, pval_lb, pval_bp) %>% 
    pivot_longer(!lag, values_to = "val", names_to = "type") %>% 
    mutate(type = fct_recode(type,
                            "Box-Pierce" = "pval_bp",
                            "Ljung-Box" = "pval_lb")) %>% 
    ggplot() + 
    geom_jitter(aes(x = lag, y = val, col = type), width = 0.1, height = 0, size = 2) +
    geom_hline(yintercept  = 0.05) + 
    theme(plot.title = element_text(hjust = 0.5),
          legend.position=c(0.8,.75),
          legend.title = element_blank()) + 
    labs(x = "lag (h)", y = " ") 
}



# Functions used in the simulation ----------------------------------------

calc_norm <- function(obs){
  Nt <- length(obs)
  sqrt(sum(obs^2))/sqrt(Nt-1)
}

calc_norm_matrix <- function(obs){
  Nt <- ncol(obs)
  N <- nrow(obs)
  sapply(1:N, function(i){
    sqrt(sum(obs[i, ]^2))/sqrt(Nt-1)
  })
}

# Calculate the rho
my_new_receipt <- function(obs, H){
  N <- nrow(obs)
  Nt <- ncol(obs)
  
  norm_raw <- calc_norm_matrix(obs)
  
  SpMedian <- as.numeric(SpMed(seq(0, 1, length.out = Nt), obs)$med)
  
  obs_center <- sapply(1:N, function(i){
    obs[i, ] - SpMedian
  }) |>  t()
  
  norm_center <- calc_norm_matrix(obs_center)
  
  rho_raw <- sapply(1:H, FUN = function(h){
    res <- sapply(1:(N-h), function(i){
      sum(obs[i, ]/norm_raw[i] * obs[i+h, ]/norm_raw[i+h])/(Nt-1)
    })
    sum(res)/N
  })
  
  rho_cen <- sapply(1:H, FUN = function(h){
    res <- sapply(1:(N-h), function(i){
      sum(obs_center[i, ]/norm_center[i] * obs_center[i+h, ]/norm_center[i+h])/(Nt-1)
    })
    sum(res)/N
  })
  my_var_raw <- calc_var(obs/norm_raw)
  my_var_cen <- calc_var_raw(obs)
  tibble(h = 1:H, rho_raw = rho_raw, rho_cen = rho_cen, 
         std_0_raw = sqrt(my_var_raw),
         std_0_cen = sqrt(my_var_cen)) 
}


# Temp, without median
my_new_receipt_noMedian <- function(obs, H){
  N <- nrow(obs)
  Nt <- ncol(obs)
  
  norm_raw <- calc_norm_matrix(obs)
  
  rho <- sapply(1:H, FUN = function(h){
    res <- sapply(1:(N-h), function(i){
      sum(obs[i, ]/norm_raw[i] * obs[i+h, ]/norm_raw[i+h])/(Nt-1)
    })
    sum(res)/N
  })
  my_var <- calc_var_raw(obs)
  tibble(h = 1:H, rho = rho, std_0 = sqrt(my_var)) 
}

# Calculate the normalizing constant of the kernel
calc_ker_norm_const <- function(norm, ker) {
  f <- function(x) {
    g <- function(y) {
      ker(x, y) ^ 2
    }
    return(integrate(g, 0, 1)$value)
  }
  f <- Vectorize(f)
  val <- integrate(f, 0, 1)$value
  norm / val
}

# FSAR --------------------------------------------------------------------
#' Estimate the kernel of the FSAR model
#'
#' @param X the matrix of the observation. It should be N x Nt
#' @param p the number of PCA
#' @param my_lags the user specified lags
#' @param n_basis the number of basis used in the FPCA
#' @param percent_CPV the threshold of the cumulative percentage of variation
#' @return A list that contains the estimation of the kerne and the Phi
#' @export
FSAR <- function(X, p = 10, my_lags, n_basis = 10, percent_CPV = 0.95){
  Nt <- ncol(X)
  N <- nrow(X)
  s <- seq(0, 1, length.out = Nt)
  basis <- fda::create.bspline.basis(c(0, 1), nbasis = n_basis, norder = 4)
  # basis <- create.fourier.basis(c(0, 1), nbasis = basisfd)
  fdX <- fda::Data2fd(argvals = s, t(X), basis)
  fdXpca <- fda::pca.fd(fdX, nharm = p)
  p <- min(which(cumsum(fdXpca$values) >= percent_CPV * sum(fdXpca$values)))
  
  fdXpca <- fda::pca.fd(fdX, nharm = p)
  eigenvalues <- fdXpca$values; scoresX <- fdXpca$scores
  # jth column of scoresX contains scores of the jth EFPC
  harmonicsX <- fdXpca$harmonics # extract the EFPC's
  varnceprop <- fdXpca$varprop # proportion of variance explained by the EFP's
  
  # Now using the least squares ---------------------------------------------
  k <- length(my_lags)
  X_LP <- matrix(0, N - max(my_lags), k*p)
  for(i in (max(my_lags)+1):N){
    X_LP_i <- rep(0, k*p)
    for(j in 1:k){
      X_LP_i[1:p + (j-1)*p] <- scoresX[i - my_lags[j], ]
    }
    X_LP[i - max(my_lags), ] <- X_LP_i
  }
  
  X_RP <- matrix(0, nrow = N - max(my_lags), ncol = p)
  for(i in (max(my_lags)+1):N){
    X_RP[i-max(my_lags), ] <- scoresX[i,]
  }
  
  # Least square estimation
  Phi_hat <- solve(t(X_LP) %*% X_LP) %*% t(X_LP) %*% X_RP
  
  # Use it to estimate the kernel, there are k kernels
  Phi_hat <- lapply(1:length(my_lags), function(index){
    Phi_hat[1:p + (index-1)*p, ]
  })
  
  V <- sapply(1:p,  function(j){
    fda::eval.fd(evalarg = s, harmonicsX[j])
  })
  
  kernel_est <- lapply(1:length(my_lags), function(index){
    kernel <- matrix(0, Nt, Nt)
    for(tk in 1:Nt){
      for(sj in 1:Nt){
        kernel[tk, sj] <- t(V[tk, ]) %*% Phi_hat[[index]] %*% V[sj, ]
      }
    }
    kernel
  })
  
  list(Phi_hat = Phi_hat, kernel_est = kernel_est)
}