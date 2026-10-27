#############################################################################################################
# 1. LOAD PACKAGES AND MCS DATA
#############################################################################################################

library(tidyverse)
library(eClosure) # closed e-BH procedure
library(Rmpfr) 

setwd("~/github/multiverse.evalues/")
source('routines.R')
mcs = new.env()
load('data/mcs.Rdata', mcs)
set.seed(35)

attach(mcs)

datareg = na.omit(data[c(x_vars, cvars)])
names(datareg) = c(x_names, names(cvars))
datareg= rename(datareg, Treatment1=TV, Treatment2=Electronic_games, Treatment3=Social_media, Treatment4=Other_internet, Treatment5=Own_computer)
datareg= scale(datareg)


#############################################################################################################
# 2. DEFINE DATA-GENERATING TRUTH
#############################################################################################################

noutcomes = 5
ntreatments = length(x_vars)
beta = matrix(0, nrow=noutcomes, ncol=ntreatments)
rownames(beta) = paste0("Outcome", 1:nrow(beta))
colnames(beta) = paste0("Treatment", 1:ncol(beta))
beta[2,] = c(0, 0, 0.05, 0.1, 0.15)
beta[3,] = c(0, 0, 0.1, 0.15, 0.2)
beta[4,] = c(0, 0, 0.15, 0.2, 0.25)
beta[5,] = c(0, 0, 0.2, 0.25, 0.3)

eta = double(length(cvars))
names(eta) = names(cvars)
eta[1:10] = seq(1, 0.1, by=-0.1)


#############################################################################################################
# 3. DEFINE FUNCTION TO SIMULATE DATA
#############################################################################################################

# Simulate values of several outcomes using treatment and control values in dataset datareg
# Input
# - beta: treatment effects (one row per outcome, one column per treatment)
# - eta: effect of control covariates (same for all outcomes)
# - n: sample size to be simulated (same for all outcomes)
# - data: data.frame where treatment and control values should be sampled from (n samples with replacement)
# Output: matrix with n rows and number of treaments columns containing the simulated output, equal to the linear predictor + iid standard Gaussian noise
simdata = function(beta, eta, n, datareg)  {
  # Sample treatment and control covariate values
  idx = sample(1:nrow(datareg), size=n, replace=TRUE)
  xvals = as.matrix(datareg[idx,colnames(beta)])
  zvals = as.matrix(datareg[idx,names(eta)])
  # Simulate outcome values  
  ysim = matrix(NA, nrow=n, ncol=nrow(beta))
  colnames(ysim) = paste0("Outcome", 1:ncol(ysim))
  for (i in 1:nrow(beta)) {
    xbeta = xvals %*% matrix(beta[i,], ncol=1)
    zeta = zvals %*% matrix(eta, ncol=1)
    ysim[,i] = xbeta + zeta + rnorm(n)
  }
  return(data.frame(ysim, xvals, zvals))
}


#############################################################################################################
# 4. RUN SIMULATION STUDY
#############################################################################################################

# Simulate a single dataset and obtain e-values (mixture calibrator, inverse root calibrator) and p-values
# Also, run closed e-BH and Benjamini-Yekutieli P-value adjustment
simevalues = function(beta, eta, n, datareg) {
  # Simulate data
  datasim = simdata(beta=beta, eta=eta, n=n, datareg=datareg)
  # Obtain p-values and e-values for all outcomes
  simi = vector("list", nrow(beta))
  for (j in 1:noutcomes) {
    # construct formula
    my.formula.string = paste(paste0("Outcome",j, " ~"), paste(names(datasim)[-1:-5], collapse = " + "))
    my.formula= as.formula(my.formula.string)
    # compute evalues
    tt = hypsupp(formula=my.formula, data=datasim, family='normal', softrank=FALSE, mixtevalue=FALSE, BF=FALSE, p.to.e = TRUE, logpvalue = TRUE)
    simi[[j]] = data.frame(simulation = i, outcome = paste0("Outcome",j), tt$stats) |>
      select(outcome, var, anov.pvalue, logcalib1, logcalib3) |>
      rename(pvalue = anov.pvalue, logmixture.evalue = logcalib1, loginvroot.evalue = logcalib3) |>
      filter(var %in% paste0("Treatment", 1:ncol(beta)))
  }
  simi = do.call(rbind, simi)
  simi$true_beta = as.vector(t(beta))
  # Closed e-BH procedure and Benjamini-Yekutieli P-value adjustment  
  k_mixture <- closedeBH(exp(simi$logmixture.evalue), alpha = 0.05) # size of the largest rejection set 
  k_invroot <- closedeBH(exp(simi$loginvroot.evalue), alpha = 0.05) # size of the largest rejection set 
  ans = arrange(simi, desc(logmixture.evalue)) |>
    mutate(closed_eBH_mixture = (1:nrow(simi)) <= k_mixture) |>
    arrange(simi, desc(loginvroot.evalue)) |>
    mutate(closed_eBH_invroot = (1:nrow(simi)) <= k_invroot) |>
    arrange(outcome, var) |>
    mutate(pvalue.BY = p.adjust(pvalue, method='BY'), n = n)
  return(ans)
}

nsims = 1000
nseq = c(10^3, 10^4)
simoutput = vector("list", length(nseq))
for (i in 1:length(nseq)) {
  cat("Simulating for n=", nseq[i])
  simi = vector("list", length(nsims))
  for (j in 1:nsims) {
    simi[[j]] = simevalues(beta=beta, eta=eta, n=nseq[i], datareg=datareg) |>
      mutate(simulation = j)
    if (j %% (nsims * 0.05) == 0) {
      percent <- (j / nsims) * 100
      cat(paste0("Progress: ", percent, "%\n"))
    }
  }
  simoutput[[i]] = do.call(rbind, simi)
  cat("\n")
}

simoutput = do.call(rbind, simoutput)
save(simoutput, file="simulations_multiverse/multiverse_simulation_evalues.RData", compress=TRUE)


# Plot power for closed_eBH_mixture, closed_eBH_invroot, pvalue.BY

load("simulations_multiverse/multiverse_simulation_evalues.RData")

simpower = group_by(simoutput, n, true_beta) |>
  summarize(eBH_mixture = mean(closed_eBH_mixture), 
            eBH_invroot = mean(closed_eBH_invroot), 
            BY = mean(pvalue.BY < 0.05))

ggplot(simpower, aes(x = true_beta)) +
  geom_hline(yintercept = 0.05, col = 'gray', linewidth = 1) +
  # calib.c1
  geom_line(aes(y = eBH_mixture, linetype = "calib.c1")) +
  geom_point(aes(y = eBH_mixture, shape = "calib.c1")) +
  # calib.c2
  geom_line(aes(y = eBH_invroot, linetype = "calib.c2")) +
  geom_point(aes(y = eBH_invroot, shape = "calib.c2")) +
  # BY
  geom_line(aes(y = BY, linetype = "BY")) +
  geom_point(aes(y = BY, shape = "BY")) +
  scale_linetype_manual(name = "Method", values = c("calib.c1" = 3, "calib.c2" = 2, "BY" = 1)) +
  scale_shape_manual(name = "Method", values = c("calib.c1" = 3, "calib.c2" = 2, "BY" = 5)) +
  facet_wrap(~ paste0("n = ", n)) +
  theme_bw() +
  theme(legend.position = c(.8, .4)) +
  labs(x = "True regression coefficient", y = "Proportion of rejected hypotheses")

ggsave("paper/images/sim.multiverse_power.pdf")


# Plot average log-evidence 

simevid = group_by(simoutput, n, true_beta) |>
  summarize(loge_mixture = mean(logmixture.evalue), 
            loge_invroot = mean(loginvroot.evalue))

ggplot(simevid, aes(x = true_beta)) +
  geom_hline(yintercept = 0.05, col = 'gray', linewidth = 1) +
  # calib.c1
  geom_line(aes(y = loge_mixture, linetype = "calib.c1")) +
  geom_point(aes(y = loge_mixture, shape = "calib.c1")) +
  # calib.c2
  geom_line(aes(y = loge_invroot, linetype = "calib.c2")) +
  geom_point(aes(y = loge_invroot, shape = "calib.c2")) +
  # manual legend
  scale_linetype_manual(name = "Method", values = c("calib.c1" = 3, "calib.c2" = 2)) +
  scale_shape_manual(name = "Method", values = c("calib.c1" = 3, "calib.c2" = 2)) +
  facet_wrap(~ paste0("n = ", n)) +
  theme_bw() +
  theme(legend.position = c(.2, .8)) +
  labs(x = "True regression coefficient", y = "log e-value")

ggsave("paper/images/sim.multiverse_logevalue.pdf")



#############################################################################################################
# 5. LARGER MULTIVERSE
#############################################################################################################

noutcomes = 20
ntreatments = length(x_vars)
beta = matrix(0, nrow=noutcomes, ncol=ntreatments)
rownames(beta) = paste0("Outcome", 1:nrow(beta))
colnames(beta) = paste0("Treatment", 1:ncol(beta))
beta[2,] = c(0, 0, 0.05, 0.1, 0.15)
beta[3,] = c(0, 0, 0.1, 0.15, 0.2)
beta[4,] = c(0, 0, 0.15, 0.2, 0.25)
beta[5,] = c(0, 0, 0.2, 0.25, 0.3)
beta[6,] = beta[1,]
beta[7,] = beta[2,]
beta[8,] = beta[3,]
beta[9,] = beta[4,]
beta[10,] = beta[5,]
beta[11,] = beta[1,]
beta[12,] = beta[2,]
beta[13,] = beta[3,]
beta[14,] = beta[4,]
beta[15,] = beta[5,]
beta[16,] = beta[1,]
beta[17,] = beta[2,]
beta[18,] = beta[3,]
beta[19,] = beta[4,]
beta[20,] = beta[5,]


eta = double(length(cvars))
names(eta) = names(cvars)
eta[1:10] = seq(1, 0.1, by=-0.1)


nsims = 1000
nseq = c(10^3, 10^4)
simoutput = vector("list", length(nseq))
for (i in 1:length(nseq)) {
  cat("Simulating for n=", nseq[i])
  simi = vector("list", length(nsims))
  for (j in 1:nsims) {
    simi[[j]] = simevalues(beta=beta, eta=eta, n=nseq[i], datareg=datareg) |>
      mutate(simulation = j)
    if (j %% (nsims * 0.05) == 0) {
      percent <- (j / nsims) * 100
      cat(paste0("Progress: ", percent, "%\n"))
    }
  }
  simoutput[[i]] = do.call(rbind, simi)
  
  cat("\n")
}

simoutput = do.call(rbind, simoutput)
save(simoutput, file="simulations_multiverse/largemultiverse_simulation_evalues.RData", compress=TRUE)


# Plot power for closed_eBH_mixture, closed_eBH_invroot, pvalue.BY

load("simulations_multiverse/largemultiverse_simulation_evalues.RData")

simpower = group_by(simoutput, n, true_beta) |>
  summarize(eBH_mixture = mean(closed_eBH_mixture), 
            eBH_invroot = mean(closed_eBH_invroot), 
            BY = mean(pvalue.BY < 0.05))

ggplot(simpower, aes(x = true_beta)) +
  geom_hline(yintercept = 0.05, col = 'gray', linewidth = 1) +
  # calib.c1
  geom_line(aes(y = eBH_mixture, linetype = "calib.c1")) +
  geom_point(aes(y = eBH_mixture, shape = "calib.c1")) +
  # calib.c2
  geom_line(aes(y = eBH_invroot, linetype = "calib.c2")) +
  geom_point(aes(y = eBH_invroot, shape = "calib.c2")) +
  # BY
  geom_line(aes(y = BY, linetype = "BY")) +
  geom_point(aes(y = BY, shape = "BY")) +
  scale_linetype_manual(name = "Method", values = c("calib.c1" = 3, "calib.c2" = 2, "BY" = 1)) +
  scale_shape_manual(name = "Method", values = c("calib.c1" = 3, "calib.c2" = 2, "BY" = 5)) +
  facet_wrap(~ paste0("n = ", n)) +
  theme_bw() +
  theme(legend.position = c(.8, .4)) +
  labs(x = "True regression coefficient", y = "Proportion of rejected hypotheses")

ggsave("paper/images/sim.multiverse_large_power.pdf")


# Plot average log-evidence 

simevid = group_by(simoutput, n, true_beta) |>
  summarize(loge_mixture = mean(logmixture.evalue), 
            loge_invroot = mean(loginvroot.evalue))

ggplot(simevid, aes(x = true_beta)) +
  geom_hline(yintercept = 0.05, col = 'gray', linewidth = 1) +
  # calib.c1
  geom_line(aes(y = loge_mixture, linetype = "calib.c1")) +
  geom_point(aes(y = loge_mixture, shape = "calib.c1")) +
  # calib.c2
  geom_line(aes(y = loge_invroot, linetype = "calib.c2")) +
  geom_point(aes(y = loge_invroot, shape = "calib.c2")) +
  # manual legend
  scale_linetype_manual(name = "Method", values = c("calib.c1" = 3, "calib.c2" = 2)) +
  scale_shape_manual(name = "Method", values = c("calib.c1" = 3, "calib.c2" = 2)) +
  facet_wrap(~ paste0("n = ", n)) +
  theme_bw() +
  theme(legend.position = c(.2, .8)) +
  labs(x = "True regression coefficient", y = "log e-value")

ggsave("paper/images/sim.multiverse_large_logevalue.pdf")

