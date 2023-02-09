#' The code to reproduce the plots in Figure 5.

# Load the electricity data -----------------------------------------------
pacman::p_load(fdaACF, ggplot2, tibble, dplyr, 
               tidyr, forcats, latex2exp, kableExtra)
source("./R/util.R")
theme_update(plot.title = element_text(hjust = 0.5, size = 18),
             legend.text = element_text(size = 14),
             legend.title = element_blank(),
             legend.position = "none", 
             axis.text = element_text(size = 12),
             axis.title = element_text(size = 14))

data(elec_prices)
colnames(elec_prices) <- 1:24

plot_Y <- tibble(elec_prices) %>% rowid_to_column("id") %>% 
  pivot_longer(!id, names_to = "time", values_to =  "val") %>% 
  mutate(time = as.numeric(time), id = as.factor(id)) %>% 
  ggplot(aes(x = time, y = val, col = id)) + geom_line() + 
  labs(x = "Time (hour)", y = "Price (€/MWh)",
       title = expression(Y[i](t)))
plot_Y
# plot the difference Y_i(t) - Y_{i-1}(t)
plot_Y_diff <- as_tibble(diff(as.matrix(elec_prices))) %>% 
  rowid_to_column("id") %>% 
  pivot_longer(!id, names_to = "time", values_to =  "val") %>%
  mutate(time = as.numeric(time), id = as.factor(id)) %>%
  ggplot(aes(x = time, y = val, col = id)) + geom_line() +
  labs(x = "Time (hour)", y = "Price (€/MWh)",
       title = expression(Y[i](t) - Y[i-1](t)))
plot_Y_diff


# Plot fACF ---------------------------------------------------------------
H <- 20
fACF_obs <- obtain_FACF(Y = as.matrix(elec_prices),
                    v = 1:24,
                    nlags = H,
                    ci = 0.95,
                    figure = FALSE)
h <- 1:H
df_fACF <- tibble(rho = fACF_obs$rho, h = h, blueline = fACF_obs$Blueline)
plot_fACF_raw <- ggplot(data = df_fACF, 
                        mapping = aes(x = h, y = rho,
                                      colour = factor((h %% 7) == 0))) +
  geom_hline(aes(yintercept = blueline), 
             linetype = "longdash", color = "blue") + 
  geom_segment(mapping = aes(xend = h, yend = 0), 
               size = 1) + 
  labs(x = "h (day)", y = " ",
       title = expression(paste("fACF of ", Y[i](t)))) +
  scale_x_continuous(breaks = c(1, (1:4)*7)) +
  scale_color_manual(values=c("black", "red"), guide = "none") 
plot_fACF_raw

fACF_obs_diff <- obtain_FACF(Y = diff(as.matrix(elec_prices)),
                        v = 1:24,
                        nlags = H,
                        ci = 0.95,
                        figure = FALSE)
df_fACF_diff <- tibble(rho = fACF_obs_diff$rho, h = h, 
                  blueline = fACF_obs_diff$Blueline)
plot_fACF_diff <- ggplot(data = df_fACF_diff, 
                         mapping = aes(x = h, y = rho,
                                       colour = factor((h %% 7) == 0))) +
  geom_hline(aes(yintercept = blueline), 
             linetype = "longdash", color = "blue") + 
  geom_segment(mapping = aes(xend = h, yend = 0), 
               size = 1) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "h (day)", y = " ", 
       title = expression(paste("fACF of ", Y[i](t)-Y[i-1](t)))) + 
  scale_x_continuous(breaks = c(1, (1:4)*7)) + 
  scale_color_manual(values=c("black", "red"), guide = "none") 
plot_fACF_diff

# fSACF --------------------------------------------------
elec_prices <- as.matrix(elec_prices)
N <- nrow(elec_prices)
Nt <- ncol(elec_prices)
s <- seq(0, 1, length.out = Nt)
alpha <- 0.05
res_raw <- my_new_receipt(elec_prices, H) %>% 
  mutate(lb = qt(alpha/2, N - 1) * std_0_cen/sqrt(N), 
         ub = qt(1 - alpha/2, N - 1) * std_0_cen/sqrt(N))

plot_fSACF <- ggplot(data = res_raw, 
                    mapping = aes(x = h, y = rho_cen,
                                  colour = factor((h %% 7) == 0))) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = h, yend = 0), size = 1) + 
  geom_ribbon(aes(ymin = lb, ymax = ub), 
              linetype = "longdash", fill = NA, color = "blue") + 
  labs(x = "h (day)", y = " ",
       title = expression(paste("fSACF of ", Y[i](t))))  + 
  scale_x_continuous(breaks = c(1, (1:4)*7)) + 
  scale_color_manual(values=c("black", "red"), guide = "none") 
plot_fSACF

# Detrend --------------------------------------------------
res_diff_raw <- my_new_receipt(diff(elec_prices), H) %>% 
  mutate(lb = qt(alpha/2, N - 1) * std_0_cen/sqrt(N-1), 
         ub = qt(1 - alpha/2, N - 1) * std_0_cen/sqrt(N-1))

ggplot(data = res_diff_raw, mapping = 
         aes(x = h, y = rho_cen,
             colour = factor((h %% 7) == 0))) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = h, yend = 0), size = 1) + 
  geom_ribbon(aes(ymin = lb, ymax = ub), linetype = "longdash", 
              fill = NA, color = "blue") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "h (day)", y = " ",
       title = expression(paste("fSACF of ", Y[i](t)-Y[i-1](t))))  + 
  scale_x_continuous(breaks = c(1, (1:4)*7)) + 
  scale_color_manual(values = c("black", "red"), guide = "none")