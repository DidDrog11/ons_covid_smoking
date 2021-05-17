library(tidyverse)

#prior predictive simulation for RQ1 (current vs. never smoking)

rq1_mean <- log(0.71)
rq1_sd <- (log(0.82)-log(0.61))/3.92

rq1_csmok <- inv.logit(rnorm(10000,mean=rq1_mean, sd=rq1_sd))
hist(rq1_csmok)

rq1_nsmok <- inv.logit(rnorm(10000,mean=0, sd=0.25))
hist(rq1_nsmok)

dat_rq1 <- tibble(proportion = c(rq1_csmok, rq1_nsmok),
              smok_status = c(rep("Current smokers", 10000),
                              rep("Never smokers", 10000)))

ggplot(dat_rq1) +
  geom_histogram(aes(x = proportion)) +
  facet_wrap(~ smok_status) +
  theme_minimal() +
  theme(panel.border=element_rect(colour = "black", size = 1, fill = NA)) +
  labs(y = element_blank(),
      x = "Proportion testing positive for SARS-CoV-2")

#prior predictive simulation for RQ2 (former vs. never)

rq2_mean <- log(1.03)
rq2_sd <- (log(1.11)-log(0.95))/3.92

rq2_fsmok <- inv.logit(rnorm(10000,mean=rq2_mean, sd=rq2_sd))
hist(rq2_fsmok)

rq2_nsmok <- inv.logit(rnorm(10000,mean=0, sd=0.25))
hist(rq2_nsmok)

dat_rq2 <- tibble(proportion = c(rq2_fsmok, rq2_nsmok),
                  smok_status = c(rep("Former smokers", 10000),
                                  rep("Never smokers", 10000)))

ggplot(dat_rq2) +
  geom_histogram(aes(x = proportion)) +
  facet_wrap(~ smok_status) +
  theme_minimal() +
  theme(panel.border=element_rect(colour = "black", size = 1, fill = NA)) +
  labs(y = element_blank(),
       x = "Proportion testing positive for SARS-CoV-2")

#prior predictive simulation for RQ3 (nicotine users who smoke tobacco vs. nicotine users who do not smoke tobacco)

rq3_mean <- log(0.71)
rq3_sd <- (log(0.82)-log(0.61))/3.92

rq3_nicsmok <- inv.logit(rnorm(10000,mean=rq3_mean, sd=rq3_sd))
hist(rq3_nicsmok)

rq3_nic <- inv.logit(rnorm(10000,mean=rq3_mean, sd=rq3_sd))
hist(rq3_nic)

dat_rq3 <- tibble(proportion = c(rq3_nicsmok, rq3_nic),
                  smok_status = c(rep("Nicotine users who smoke", 10000),
                                  rep("Nicotine users", 10000)))

ggplot(dat_rq3) +
  geom_histogram(aes(x = proportion)) +
  facet_wrap(~ smok_status) +
  theme_minimal() +
  theme(panel.border=element_rect(colour = "black", size = 1, fill = NA)) +
  labs(y = element_blank(),
       x = "Proportion testing positive for SARS-CoV-2")

#prior predictive simulation for RQ4 (nicotine use vs. no nicotine use among people who do not smoke tobacco)

rq4_mean <- log(0.71)
rq4_sd <- (log(0.82)-log(0.61))/3.92

rq4_nic <- inv.logit(rnorm(10000,mean=rq4_mean, sd=rq4_sd))
hist(rq4_nic)

rq4_nonic <- inv.logit(rnorm(10000,mean=rq2_mean, sd=rq2_sd))
hist(rq4_nonic)

dat_rq4 <- tibble(proportion = c(rq4_nic, rq4_nonic),
                  smok_status = c(rep("Nicotine use", 10000),
                                  rep("No nicotine use", 10000)))

ggplot(dat_rq4) +
  geom_histogram(aes(x = proportion)) +
  facet_wrap(~ smok_status) +
  theme_minimal() +
  theme(panel.border=element_rect(colour = "black", size = 1, fill = NA)) +
  labs(y = element_blank(),
       x = "Proportion testing positive for SARS-CoV-2")
