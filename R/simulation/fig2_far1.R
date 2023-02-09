pacman::p_load(doParallel, dplyr, pbapply, sde, ggplot2, tidyr, latex2exp,
               fdaACF)
source("./util.R")
theme_update(plot.title = element_text(hjust = 0.5, size = 18),
             legend.position = "none", 
             axis.text = element_text(size = 12),
             axis.title = element_text(size = 14))

ker <- function(tt, ss){
  exp( -(tt^2 + ss^2)/2)  
}
N <- 1000
my_magnitude <- -0.7 # 0.2 or 0.7
my_coef <- calc_ker_norm_const(my_magnitude, ker = ker)
alpha <- 0.05
if(N < 800){
  H <- 15
} else{
  H <- 30
}
set.seed(777)
Nt <- 101
tt <- s <- seq(0, 1, length.out = Nt)
Z <- rnorm(2)
season <- 1
err_t <- X <- matrix(0, nrow = N, ncol = Nt)

X[1, ] <- Z[1] * sqrt(2) * sin(2*pi*s) + Z[2] * sqrt(2) * cos(2*pi*s)
err_t[1, ] <- as.numeric(sde::BBridge(N = Nt - 1))

err_t <- sapply(1:N, function(i){
  as.numeric(sde::BBridge(N = Nt - 1))
}) %>% t()

for(i in 2:N){
  if(i %% 500 == 0 )
    print(i)
  err_t[i, ] <- as.numeric(sde::BBridge(N = Nt - 1))
  
  x1 <- rep(0, Nt)
  if(i <= season){
    X[i, ] <- err_t[i, ]
  } else{
    for(j in 1:Nt){ # for t
      for(k in 1:Nt){ # for s
        x1[j] <- x1[j] + my_coef * ker(tt[j], s[k]) * X[i-season, k] 
      }
    }
    X[i, ] <- x1/(Nt-1) + err_t[i, ]
  }
}

# Plot the observations ---------------------------------------------------
Nplot <- 50
df_x <- cbind(time = s, val = t(X[2:(Nplot+1), ])) %>% as_tibble()
names(df_x) <-  c("time", paste0("obs_", 1:50))

df_res <- pivot_longer(df_x, !time, names_to = "obs", values_to = "val")
ggplot(df_res, aes(x = time, y = val, col = obs)) +
  geom_line() + labs(x = "Time (t)", y = "",
                     title = TeX(sprintf("$FAR(1, %f)$", my_magnitude))) 

# Calculate and plot the FACF ---------------------------------------------
res_FACF <- obtain_FACF(Y = X, v = s, 
                                nlags = 30, 
                                ci = 1 - alpha, 
                                figure = FALSE)
h <- 1:H
df_facf <- tibble(rho = res_FACF$rho, h = h, blueline = res_FACF$Blueline)
plot_facf_raw <- ggplot(data = df_facf, 
                        mapping = aes(x = h, y = rho)) +
  geom_hline(aes(yintercept = blueline), 
             linetype = "longdash", color = "blue") + 
  geom_segment(mapping = aes(xend = h, yend = 0), 
               size = 1) + 
  labs(x = "h", y = " ", title = "fACF") + 
  scale_color_manual(values=c("black", "red"), guide = "none") 
plot_facf_raw

# Calculate and plot the fSACF -------------------------------------------------
plot_fSACF_raw <- my_new_receipt(X, H) %>% 
  mutate(lb = qnorm(alpha/2) * std_0_cen/sqrt(N), 
         ub = qnorm(1 - alpha/2) * std_0_cen/sqrt(N))
ggplot(data = plot_fSACF_raw, mapping = aes(x = h, y = rho_cen)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = h, yend = 0), size = 1, col = "black") + 
  geom_ribbon(aes(ymin = lb, ymax = ub), 
              linetype = "longdash", fill = NA, color = "blue") + 
  labs(x = "h", y = " ", 
       title = "fSACF")
plot_fSACF_raw