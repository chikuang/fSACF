N <- 100
Nt <- 101
D <- 7
set.seed(777)
source("./util.R")
library(pbapply)
library(fda)
library(ggplot2)
library(dplyr)
library(forcats)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 18),
             legend.position = "none", 
             axis.text = element_text(size = 12),
             axis.title = element_text(size = 16))
s <- seq(0, 1, length.out = Nt)

my_coef <- matrix(rexp(N*D), ncol = D)
bs <- eval.basis(s, create.bspline.basis(c(0, 1), nbasis = D))

x <- my_coef %*% t(bs)
# x <- sapply(1:N, function(i){
#  
#   
#   bs %*% t(my_coef[]
# }) |> t()

norm_raw <- sapply(1:N, function(i){
  sqrt(sum(x[i, ]^2))/(Nt-1)
})

SpMedian <- SpMed(s, x)

x_center <- sapply(1:N, function(i){
  x[i,] - SpMedian$med
}) |>  t()

norm_center <- sapply(1:N, function(i){
  sqrt(sum(x_center[i, ]^2))/(Nt-1)
})

res_plot_raw <- pblapply(1:N, function(i){
  tibble(id = i, val = x[i, ], time = s)
}) |> bind_rows()


res_plot_raw %>% mutate(id = as_factor(id)) %>% group_by(id) %>%
  ggplot() + 
  geom_line(aes(x = time, y = val, col = id)) +
  theme(legend.position = "none") + xlab("Time (t)") + ylab("") +
  ggtitle(expression(paste("Bspline ", BS[i](t)))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data = tibble(med = as.numeric(SpMedian$med), time = s), 
            aes(x = time, y = med), col = "black", size = 1.1)


# Plot ACF ----------------------------------------------------------------
set.seed(777)
N <- 1000
Nt <- 101
H <- 1:30
D <- 7

s <- seq(0, 1, length.out = Nt)

BS_basis <- eval.basis(s, create.bspline.basis(c(0, 1), nbasis = D))
x <- sapply(1:N, function(i){
  # bet <- rbeta(D, 2.5, 5)
  bet <- rexp(D)
  BS_basis %*% bet
}) |> t()

# Plot basis
colnames(BS_basis) <- paste0("Basis_", 1:D)
reshape2::melt(BS_basis) %>% 
  mutate(Var1 = Var1/Nt, Var2 = factor(Var2)) %>% 
  ggplot(aes(x = Var1, y = value, col = Var2)) + 
  geom_line() + 
  theme(legend.position = "none") + labs(x = "Time (t)", y = "")

alpha <- 0.05
res <- my_new_receipt(x, H = 30) %>% 
  mutate(lb = qnorm(alpha/2) * std_0_cen/sqrt(N), 
         ub = qnorm(1 - alpha/2) * std_0_cen/sqrt(N))

ggplot(data = res, mapping = aes(x = h, y = rho_cen)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = h, yend = 0), size = 1, col = "black") +
  geom_ribbon(aes(ymin = lb, ymax = ub), linetype = "longdash", 
              fill = NA, color = "blue") +
  labs(x = "h", y = " ")