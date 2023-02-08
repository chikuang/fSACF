library(readr)
library(pbapply)
library(lubridate)
library(ggplot2)
library(scales)
source("./util.R")
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 20),
             # legend.position = "none",
             legend.background = element_rect(fill = "white", color = "black"),
             legend.text = element_text(size=16),
             legend.title = element_blank(),
             axis.text = element_text(size = 12),
             axis.title = element_text(size = 16))

scale2 <- function(x, na.rm = FALSE) (x * 100)

df_stock <- read_csv("./data/stock_data.csv") %>% 
  mutate(Date =  mdy(Date), 
         Time_scale = (as.numeric(Time)/60 - 570)/390) %>% 
  dplyr::select(Date, Time, Time_scale, everything())

#' Delete the first observation, because the return is defined from 9:35 - 16:00
df_return <- df_stock %>% filter(!is.na(sp_Ret)) %>% 
  dplyr::select(Date, Time, Time_scale, contains("Ret")) %>% 
  mutate(sp_Ret = sp_Ret * 100, cl_Ret = cl_Ret*100, ec_Ret = ec_Ret*100,
         aapl_Ret = aapl_Ret*100 , wfc_Ret = wfc_Ret*100, xom_Ret = xom_Ret*100) %>% 
  mutate(sp_Ret2 = sp_Ret^2, cl_Ret2 = cl_Ret^2, ec_Ret2 = ec_Ret^2,
         aapl_Ret2 = aapl_Ret^2 , wfc_Ret2 = wfc_Ret^2, xom_Ret2 = xom_Ret^2) %>% 
  group_by(Date) %>% 
  mutate(sp_CIDR = sp_Ret - first(sp_Ret),
         ec_CIDR = ec_Ret - first(ec_Ret),
         cl_CIDR = cl_Ret - first(cl_Ret),
         aapl_CIDR = aapl_Ret - first(aapl_Ret),
         sp_CIDR2 = sp_CIDR^2,
         ec_CIDR2 = ec_CIDR^2,
         cl_CIDR2 = cl_CIDR^2,
         aapl_CIDR2 = aapl_CIDR^2) %>%
  ungroup() %>% 
  mutate(Time_scale = (as.numeric(Time)/60 - 575)/385) %>% 
  group_by(Date) %>% mutate(day  = cur_group_id()) %>% ungroup()

N <- distinct(df_return, day) %>% pull() %>% length()
Nt <- distinct(df_return, Time) %>% pull() %>% length()

# Make the data frame and use the fdaACF
calc_dacf <- function(my_df, stock, alpha, H){
  N <- distinct(my_df, day) %>% pull() %>% length()
  Nt <- distinct(my_df, Time) %>% pull() %>% length()
  s <- seq(0, 1, length.out = Nt)
  
  df_longer <- sapply(1:N, function(i){
    my_df %>% filter(day == i) %>% 
      pull(!!stock)
  }) %>% t()
  
  colnames(df_longer) <- sapply(1:Nt, function(i) paste0("t", i))
  rownames(df_longer) <- sapply(1:N, function(i) paste0("Day", i))
  
  my_new_receipt(df_longer, H) %>%
    mutate(lb = qnorm(alpha/2) * std_0_cen/sqrt(N),
           ub = qnorm(1 - alpha/2) * std_0_cen/sqrt(N))
}  

alpha <- 0.05

# sp_Ret, ec_Ret, cl_Ret, aapl_Ret, 
# sp_Ret2, ec_Ret2, cl_Ret2, aapl_Ret2
# sp_CIDR, ec_CIDR, cl_CIDR, aapl_CIDR
# sp_CIDR2, ec_CIDR2, cl_CIDR2, aapl_CIDR2
res <- calc_dacf(df_return, stock = "cl_Ret2", 0.05, 20)
temp <- calc_BP_test(rho = res$rho_cen, H = 20, 
                      alpha = alpha, N = N, cp = res$std_0_cen[1]) %>% 
  mutate_all(~ round(., 4)) %>% 
  filter(lag == 10)
temp

ggplot(data = res, mapping = aes(x = h, y = rho_cen)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = h, yend = 0), size = 1, col = "black") +
  geom_ribbon(aes(ymin = lb, ymax = ub), 
              linetype = "longdash", fill = NA, color = "blue") +
  labs(x = "h (trading day)", y = " ") 


# Raw return series -------------------------------------------------------
df_return %>% filter(Date < "2014-01-08") %>% 
  mutate(Date = factor(Date),
         Time = as.POSIXct(hms::parse_hm(Time))) %>% 
  group_by(Date) %>% 
  ggplot(aes(Time, cl_Ret, col = Date)) + geom_line() +
  labs(x = "Time (hour)", title = "CL log return", y = "") + 
  geom_hline(aes(yintercept = 0)) +
  theme(legend.position = c(0.8, 0.80),
        legend.text = element_text(size=12)
        ) +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") + 
  scale_y_continuous(limits = c(-0.3, 0.5),
                     breaks = seq(-0.3, 0.3, length.out = 7))

# Squared series  ---------------------------------------------------------
theme_update(legend.position = "none")
df_return %>% filter(Date < "2014-01-08") %>% 
  mutate(Date = factor(Date),
         Time = as.POSIXct(hms::parse_hm(Time))) %>% 
  group_by(Date) %>% 
  ggplot(aes(Time, cl_Ret2, col = Date)) + geom_line() +
  labs(x = "Time (hour)", title = "Squared CL log return", y = "") + 
  geom_hline(aes(yintercept = 0)) +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") #+ 
  
# CIDR --------------------------------------------------------------------
df_return %>% dplyr::select(Date, Time, contains("CIDR")) %>% 
  filter(Date < "2014-01-08") %>% 
  mutate(Date = factor(Date),
       Time = as.POSIXct(hms::parse_hm(Time))) %>% 
  ggplot(aes(Time, cl_CIDR, col = Date)) + geom_line() +
  # labs(x = "Time", y = "S&P 500 CIDR") + 
  labs(x = "Time (hour)", title = "CL CIDR", y = "") + 
  geom_hline(aes(yintercept = 0)) +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M")

# Squared CIDR ------------------------------------------------------------
df_return %>% dplyr::select(Date, Time, contains("CIDR")) %>% 
  filter(Date < "2014-01-08") %>% 
  mutate(Date = factor(Date),
         Time = as.POSIXct(hms::parse_hm(Time))) %>% 
  ggplot(aes(Time, cl_CIDR2, col = Date)) + geom_line() +
  labs(x = "Time (hour)", title = "Squared CL CIDR", y = "") + 
  geom_hline(aes(yintercept = 0)) +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") 
