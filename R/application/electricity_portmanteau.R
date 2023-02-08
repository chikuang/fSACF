source("./util.R")
theme_set(theme_bw())
pacman::p_load(fdaACF, ggplot2, tibble, dplyr, tidyr, forcats, latex2exp,
               forecast, fda)
theme_update(plot.title = element_text(hjust = 0.5, size = 18),
             legend.text = element_text(size = 14), 
             legend.title = element_blank(),
             legend.position = "none", 
             axis.text = element_text(size = 12),
             axis.title = element_text(size = 14))
data(elec_prices)
dat <- as.matrix(elec_prices)
X <- diff(dat)

CPV_val <- 0.90
alpha <- 0.05
H <- 20
s <- seq(0, 1, length.out = 24)
basis <- create.bspline.basis(c(0, 1), nbasis = 24, norder = 4)
fdX <- Data2fd(argvals = s, t(X), basis)
p <- 5
fdXpca <- pca.fd(fdX, nharm = p)
cumsum(fdXpca$varprop)

N <- nrow(X)

score_1 <- ts(fdXpca$scores[, 1], frequency = 7)
score_2 <- ts(fdXpca$scores[, 2], frequency = 7)
score_3 <- ts(fdXpca$scores[, 3], frequency = 7)
score_4 <- ts(fdXpca$scores[, 4], frequency = 7)
score_5 <- ts(fdXpca$scores[, 5], frequency = 7)

mod1 <- auto.arima(score_1)
mod2 <- auto.arima(score_2)
mod3 <- auto.arima(score_3)
mod4 <- auto.arima(score_4)
mod5 <- auto.arima(score_5)
checkresiduals(mod1)
checkresiduals(mod2)
checkresiduals(mod3)
checkresiduals(mod4)
checkresiduals(mod5)

coef_fitted_fpc1 <- mod1$fitted
coef_fitted_fpc2 <- cbind(mod1$fitted, mod2$fitted)
coef_fitted_fpc3 <- cbind(mod1$fitted, mod2$fitted, mod3$fitted)
coef_fitted_fpc4 <- cbind(mod1$fitted, mod2$fitted, mod3$fitted, mod4$fitted)
coef_fitted_fpc5 <- cbind(mod1$fitted, mod2$fitted, mod3$fitted, mod4$fitted, mod5$fitted)


my_fitted_fpc1 <-  t(fdXpca$harmonics$coefs[,1] %*% t(coef_fitted_fpc1))
my_fitted_fpc2 <-  t(fdXpca$harmonics$coefs[,1:2] %*% t(coef_fitted_fpc2))
my_fitted_fpc3 <-  t(fdXpca$harmonics$coefs[,1:3] %*% t(coef_fitted_fpc3))
my_fitted_fpc4 <-  t(fdXpca$harmonics$coefs[,1:4] %*% t(coef_fitted_fpc4))
my_fitted_fpc5 <-  t(fdXpca$harmonics$coefs[,1:5] %*% t(coef_fitted_fpc5))


my_resid_fpc1 <- X - my_fitted_fpc1
my_resid_fpc2 <- X - my_fitted_fpc2
my_resid_fpc3 <- X - my_fitted_fpc3
my_resid_fpc4 <- X - my_fitted_fpc4
my_resid_fpc5 <- X - my_fitted_fpc5

res_diff_fpc1 <- my_new_receipt(my_resid_fpc1, H) %>% 
  mutate(lb = qnorm(alpha/2) * std_0_cen/sqrt(N-1), 
         ub = qnorm(1 - alpha/2) * std_0_cen/sqrt(N-1))

res_diff_fpc2 <- my_new_receipt(my_resid_fpc2, H) %>% 
  mutate(lb = qnorm(alpha/2) * std_0_cen/sqrt(N-1), 
         ub = qnorm(1 - alpha/2) * std_0_cen/sqrt(N-1))

res_diff_fpc3 <- my_new_receipt(my_resid_fpc3, H) %>% 
  mutate(lb = qnorm(alpha/2) * std_0_cen/sqrt(N-1), 
         ub = qnorm(1 - alpha/2) * std_0_cen/sqrt(N-1))

res_diff_fpc4 <- my_new_receipt(my_resid_fpc4, H) %>% 
  mutate(lb = qnorm(alpha/2) * std_0_cen/sqrt(N-1), 
         ub = qnorm(1 - alpha/2) * std_0_cen/sqrt(N-1))

res_diff_fpc5 <- my_new_receipt(my_resid_fpc5, H) %>% 
  mutate(lb = qnorm(alpha/2) * std_0_cen/sqrt(N-1), 
         ub = qnorm(1 - alpha/2) * std_0_cen/sqrt(N-1))

plot3 <- ggplot(data = res_diff_fpc3, mapping =
         aes(x = h, y = rho_cen,
             colour = factor((h %% 7) == 0))) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = h, yend = 0), size = 1) +
  geom_ribbon(aes(ymin = lb, ymax = ub), linetype = "longdash",
              fill = NA, color = "blue") +
  labs(x = "h (day)", y = " ",
       title = "Residual of HU(3)")  +
  scale_x_continuous(breaks = c(1, (1:4)*7)) +
  scale_color_manual(values = c("black", "red"), guide = "none")


plot5 <- ggplot(data = res_diff_fpc5, mapping = 
         aes(x = h, y = rho_cen,
             colour = factor((h %% 7) == 0))) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = h, yend = 0), size = 1) + 
  geom_ribbon(aes(ymin = lb, ymax = ub), linetype = "longdash", 
              fill = NA, color = "blue") + 
  labs(x = "h (day)", y = " ",
       title = "Residual of HU(5)")  + 
  scale_x_continuous(breaks = c(1, (1:4)*7)) + 
  scale_color_manual(values = c("black", "red"), guide = "none")

# Portmanteau -------------------------------------------------------------

res_1 <- calc_BP_test(rho = res_diff_fpc1$rho_cen, H = H, 
                      alpha = alpha, N = N, cp = res_diff_fpc1$std_0_cen[1]) %>% 
  rename(val_1 = val_bp, pval_1 = pval_bp)

res_2 <- calc_BP_test(rho = res_diff_fpc2$rho_cen, H = H, 
                      alpha = alpha, N = N, cp = res_diff_fpc2$std_0_cen[1]) %>% 
  rename(val_2 = val_bp, pval_2 = pval_bp)


res_3 <- calc_BP_test(rho = res_diff_fpc3$rho_cen, H = H, 
                      alpha = alpha, N = N, cp = res_diff_fpc3$std_0_cen[1]) %>% 
  rename(val_3 = val_bp, pval_3 = pval_bp)

res_4 <- calc_BP_test(rho = res_diff_fpc4$rho_cen, H = H,
                      alpha = alpha, N = N, cp = res_diff_fpc4$std_0_cen[1]) %>%
  rename(val_4 = val_bp, pval_4 = pval_bp)

res_5 <- calc_BP_test(rho = res_diff_fpc5$rho_cen, H = H,
                      alpha = alpha, N = N, cp = res_diff_fpc5$std_0_cen[1]) %>%
  rename(val_5 = val_bp, pval_5 = pval_bp)


# FSAR and raw
res_diff_raw <- my_new_receipt(X, H) %>% 
  mutate(lb = qnorm(alpha/2) * std_0_cen/sqrt(N-1), 
         ub = qnorm(1 - alpha/2) * std_0_cen/sqrt(N-1))
res_raw <- calc_BP_test(rho = res_diff_raw$rho_cen, H = H,
                      alpha = alpha, N = N, cp = res_diff_raw$std_0_cen[1]) %>%
  rename(val_raw = val_bp, pval_raw = pval_bp)

# FSAR --------------------------------------------------------------------
X <- diff(as.matrix(elec_prices))
N <- nrow(X)
Nt <- ncol(X)
my_lags <- c(1, 7)
fsar <- FSAR(X, p = 10, my_lags = my_lags, 
             n_basis = 10, percent_CPV = CPV_val)

Xpred <- matrix(0, nrow = N, ncol = Nt)

for(i in (max(my_lags)+1):N){
  temp <- lapply(1:length(my_lags), function(index){
    x1 <- rep(0, Nt)
    for(j in 1:Nt){ # for t
      for(k in 1:Nt){ # for s
        x1[j] <- x1[j] + fsar$kernel_est[[index]][j, k] * X[i-my_lags[index], k]
      }
    }
    x1
  }) %>% rlist::list.rbind()
  Xpred[i, ] <- colSums(temp)/(Nt-1)
}

X_trim <- X[-c(1:max(my_lags)), ]
Xpred_trim <- Xpred[-c(1:max(my_lags)),]
resid_fit_FSAR <- X_trim - Xpred_trim
res_fit_FSAR <- my_new_receipt(resid_fit_FSAR, 30) %>% 
  mutate(lb = qt(alpha/2, N - 1) * std_0_cen/sqrt(N-1), 
         ub = qt(1 - alpha/2, N - 1) * std_0_cen/sqrt(N-1))

res_FSAR <- calc_BP_test(rho = res_fit_FSAR$rho_cen, H = H, 
                              alpha = alpha, 
                              N = N, cp = res_fit_FSAR$std_0_cen[1]) %>% 
  rename(val_FSAR = val_bp, pval_FSAR = pval_bp)


df_res <- inner_join(res_1, res_3, by = "lag") %>%
  # inner_join(res_1, by = "lag") %>%
  # inner_join(res_4, by = "lag") %>%
  inner_join(res_5, by = "lag") %>%
  inner_join(res_raw, by = "lag") %>% 
  inner_join(res_FSAR, by = "lag") %>% 
  # inner_join(res_10, by = "lag") %>%
  # filter(lag %in% c(20:23)) %>% 
  dplyr::select(lag, starts_with("pval_"))


df_res %>% dplyr::select(lag, pval_raw, pval_FSAR, pval_1, pval_3, pval_5) %>% 
  dplyr::rename("Diff. Y(t)" = pval_raw, "FSAR" = pval_FSAR,
      "HU(1)" = pval_1, "HU(3)" = pval_3, "HU(5)" = pval_5) %>% 
  pivot_longer(-lag, names_to = "Method", values_to = "pval") %>% 
  ggplot(aes(x = lag, y = pval, 
             colour = Method, shape = Method, size = Method)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0.05), colour = "black") +
  labs(x = "H", y = "P-value") + 
  ggtitle(TeX(paste("P-value for test statistics $Q_{364,H}$"))) + 
  scale_x_continuous(breaks = c(1, (1:4)*7)) +
  theme(legend.position = c(0.83, 0.5)) +
  scale_color_manual(values = c("blue", "black", "red", "purple", "darkgreen")) +
  scale_size_manual(values = c(4, 4, 7, 5, 5)) +
  scale_shape_manual(values = c(5, 3, 2, 4, 1))
