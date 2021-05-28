# Perform an analysis that predicts whether the target sentence in a simulated
# priming experiment is passive.
# 
# Author: Y.H. Khoe
# 
# This script needs to be called with the filename of a configuration file as a
# command line argument. That configuration file needs to define:
# - training_path: A string containing the path to the directory that contains 
#                 the simulation output from the Dual-path model during training
# - priming_path: A string containing the path to the directory that contains 
#                 the simulation output from the Dual-path model during the  
#                 priming experiment
# - exp_abr: A string containing an abbreviation that designates the experiment, 
#         for example: "1_es_en"
# - file_suffix: A string containing a short designation of the specific version
#               of the experiment, for example: "revised_final"
# - brm_seed: An integer that can be used as random seed by brms
# - n_participants: An integer representing the number of participants in the 
#                   experiment

# Load libraries -----------------------------------------------------------####

library(dplyr)
library(tidyr)
library(brms)
library(ggplot2)
library(xtable)
source("brmsfit_to_df.R")

# Load paths from external config file -------------------------------------####

args = commandArgs(trailingOnly=TRUE)
print("config file:")
print(args[1])
source(args[1])

# Load data ----------------------------------------------------------------####

print("training path:")
print(training_path)
print("priming path:")
print(priming_path)
print("exp_abr:")
print(exp_abr)
print("brm_seed:")
print(brm_seed)
print("n_participants:")
print(n_participants)

n_trials_per_participant <- 800
n_trials_per_lang_comb <- (n_participants * n_trials_per_participant) / 2

only_top_80_participants <- (n_participants == 80)

if(only_top_80_participants == FALSE){
  file_suffix <- paste0(file_suffix,"_120")
}

print("file_suffix:")
print(file_suffix)

options(mc.cores = parallel::detectCores())

if(only_top_80_participants){
  performance <- read.csv(paste0(training_path,"/performance.csv"))
  
  performance <- performance[
    performance$epoch == 16,
    c("network_num", "meaning", 
      "meaning_percentage", "grammaticality_percentage")]
  
  performance_top <- order(performance$meaning_percentage, decreasing=TRUE)[1:80]
  
  performance_top
  
  performance_top_all <- performance[performance_top,]
  
  summary(performance_top_all)
  
  top <- performance[performance_top,]
  
  print(
    paste0(
      "The accuracy scores for these models varied from ",
      round(min(performance_top_all$meaning_percentage), 2), 
      "% to ", 
      round(max(performance_top_all$meaning_percentage), 2),
      "%, with a mean of ",
      round(mean(performance_top_all$meaning_percentage), 2),
      "%."
    )
  )
  print(
    paste0(
      "The percentage of grammatically correct sentences for these models varied from ",
      round(min(performance_top_all$grammaticality_percentage), 2), 
      "% to ", 
      round(max(performance_top_all$grammaticality_percentage), 2),
      "%, with a mean of ",
      round(mean(performance_top_all$grammaticality_percentage), 2),
      "%."
    )
  )
}

filenames_bl <- list.files(priming_path, 
                        pattern="test_priming", recursive = TRUE, 
                        full.names=TRUE)

trials_pr_bl <- data.frame()

for(filename in filenames_bl){
  #print(filename)
  participant_trials_pr_bl <- read.csv(filename)
  participant_trials_pr_bl <- participant_trials_pr_bl[
    participant_trials_pr_bl$epoch > 14 & participant_trials_pr_bl$same_as_prime != 'None',
    c("epoch", "same_as_prime", "prime_structure", "target_structure", "prime_lang",
      "target_lang", "prime_id", "participant", "message")]
  participant_trials_pr_bl$participant_epoch_id <- paste(
    participant_trials_pr_bl$participant, participant_trials_pr_bl$epoch, sep = '-')
  
  participant_trials_pr_bl$message_pref <- 
    ifelse(grepl("X:1", participant_trials_pr_bl$message), "active", "passive")
  participant_trials_pr_bl$is_passive <- 
    ifelse(participant_trials_pr_bl$target_structure == 'passive', 1, 0)

  trials_pr_bl <- rbind(trials_pr_bl,participant_trials_pr_bl) 
}

if(only_top_80_participants){
  # Select only 80 best performing models
  trials_pr_bl <- trials_pr_bl[trials_pr_bl$participant %in% performance_top, ]
}
# Adjust prime_id to 50 unique items
trials_pr_bl$prime_id_50 <- substr(
  trials_pr_bl$prime_id, 
  2, 
  nchar(as.character(trials_pr_bl$prime_id))
)

trials_pr_bl$prime_id_50 <- as.numeric(trials_pr_bl$prime_id_50) %% 100

trials_pr_bl$prime_id <- factor(trials_pr_bl$prime_id_50)

trials_pr_bl_1 <- trials_pr_bl

save(trials_pr_bl_1, file = paste0("output/trials_pr_bl_", exp_abr, "_", 
                                   file_suffix, ".rda"))


# Setup predictors ---------------------------------------------------------####

print("Setup predictors:")

# Inspect is_passive

print('Summary for is_passive:')
summary(trials_pr_bl$is_passive)


# Set contrasts for message_pref

trials_pr_bl$message_pref <- factor(trials_pr_bl$message_pref)

contrasts(trials_pr_bl$message_pref) <- c(-0.5,+0.5)

print('Contrasts for message_pref:')
contrasts(trials_pr_bl$message_pref)


# Set contrasts for prime_structure

trials_pr_bl$prime_structure <- factor(trials_pr_bl$prime_structure)

contrasts(trials_pr_bl$prime_structure) = c(-0.5,+0.5)

print('Contrasts for prime_structure:')
contrasts(trials_pr_bl$prime_structure) 


# Set contrasts for prime_lang
trials_pr_bl$prime_lang <- factor(trials_pr_bl$prime_lang)

contrasts(trials_pr_bl$prime_lang) = c(-0.5,+0.5)

print('Contrasts for prime_lang:')
contrasts(trials_pr_bl$prime_lang) 


# Create predictor Language Combination

trials_pr_bl$lang_comb <- ifelse(
  trials_pr_bl$prime_lang == trials_pr_bl$target_lang, "within", "between")

trials_pr_bl$lang_comb <- factor(trials_pr_bl$lang_comb)

print('Contrasts for lang_comb:')
contrasts(trials_pr_bl$lang_comb)


# Organize data for slope plot ---------------------------------------------####

# Frequency of is_passive by language combination

freqs_lang_comb_is_pass <- data.frame(table(trials_pr_bl$is_passive,
                                            trials_pr_bl$lang_comb,
                                            trials_pr_bl$prime_structure))

names(freqs_lang_comb_is_pass) <- c('is_passive', 'lang_comb', 'prime_structure', 'frequency')
#freqs_lang_comb_is_pass

freqs_lang_comb_is_pass$lang_comb <- ifelse(freqs_lang_comb_is_pass$lang_comb == "between",
                                    "Cross-language", "Within-language")


freqs_lang_comb_is_pass_wide <- freqs_lang_comb_is_pass %>%
  spread(is_passive, frequency)
names(freqs_lang_comb_is_pass_wide) <- c('lang_comb', 'prime_structure',
                                    'is_active', 'is_passive')

freqs_lang_comb_is_pass_wide$percent_is_pass <-
  100 * (freqs_lang_comb_is_pass_wide$is_passive /
           (freqs_lang_comb_is_pass_wide$is_passive + freqs_lang_comb_is_pass_wide$is_active))

save(freqs_lang_comb_is_pass_wide, 
     file = paste0("output/freqs_lang_comb_is_pass_wide_", exp_abr, "_", 
                   file_suffix,".rda"))

# Frequency of is_passive by language combination, by participant

freqs_lang_comb_is_pass_participant <- data.frame(table(trials_pr_bl$is_passive,
                                            trials_pr_bl$participant,
                                            trials_pr_bl$lang_comb,
                                            trials_pr_bl$prime_structure))

names(freqs_lang_comb_is_pass_participant) <- c('is_passive', 'participant', 'lang_comb', 'prime_structure', 'frequency')

freqs_lang_comb_is_pass_participant_wide <- freqs_lang_comb_is_pass_participant %>%
  spread(is_passive, frequency)

names(freqs_lang_comb_is_pass_participant_wide) <- c('participant', 'lang_comb', 'prime_structure',
                                         'is_active', 'is_passive')

freqs_lang_comb_is_pass_participant_wide$percent_is_pass <-
  100 * (freqs_lang_comb_is_pass_participant_wide$is_passive /
           (freqs_lang_comb_is_pass_participant_wide$is_passive + freqs_lang_comb_is_pass_participant_wide$is_active))


freqs_lang_comb_is_pass_participant_wide$lang_comb <- ifelse(
  freqs_lang_comb_is_pass_participant_wide$lang_comb == "between", 
  "Cross-language", 
  "Within-language")


# Descriptives by language combination -------------------------------------####

print('Descriptives by language combinations:')

cross_lang_trials <- sum(freqs_lang_comb_is_pass[freqs_lang_comb_is_pass$lang_comb == 'Cross-language',]$frequency)

print('Percentage of cross-language trials included:')
(round((cross_lang_trials / n_trials_per_lang_comb) * 100, digits = 2))


within_lang_trials <- sum(freqs_lang_comb_is_pass[freqs_lang_comb_is_pass$lang_comb == 'Within-language',]$frequency)

print('Percentage of within-language trials included:')
(round((within_lang_trials / n_trials_per_lang_comb) * 100, digits = 2))

cross_pass_after_pass <- freqs_lang_comb_is_pass_wide[
  freqs_lang_comb_is_pass_wide$lang_comb == "Cross-language" & 
    freqs_lang_comb_is_pass_wide$prime_structure == "passive", 
  c("percent_is_pass")]

cross_pass_after_act <- freqs_lang_comb_is_pass_wide[
  freqs_lang_comb_is_pass_wide$lang_comb == "Cross-language" & 
    freqs_lang_comb_is_pass_wide$prime_structure == "active", 
  c("percent_is_pass")]

print(paste0("For cross-language trials in Experiment 2a, ", 
             round(cross_pass_after_pass, 2), 
             "% of sentences were passives after a passive prime, while ",
             round(cross_pass_after_act, 2),
             "% of sentences were passive after an active prime."))

within_pass_after_pass <- freqs_lang_comb_is_pass_wide[
  freqs_lang_comb_is_pass_wide$lang_comb == "Within-language" & 
    freqs_lang_comb_is_pass_wide$prime_structure == "passive", 
  c("percent_is_pass")]

within_pass_after_act <- freqs_lang_comb_is_pass_wide[
  freqs_lang_comb_is_pass_wide$lang_comb == "Within-language" & 
    freqs_lang_comb_is_pass_wide$prime_structure == "active", 
  c("percent_is_pass")]

print(paste0("For within-language trials in Experiment 2a, ", 
             round(within_pass_after_pass, 2), 
             "% of sentences were passives after a passive prime, while ",
             round(within_pass_after_act, 2),
             "% of sentences were passive after an active prime."))

# Fit brms model -----------------------------------------------------------####

bm1_is_pass <- brm(is_passive ~ lang_comb * prime_structure + 
                          prime_lang + message_pref + 
                         (lang_comb * prime_structure || participant) +
                         (1 || prime_id), 
                       family = bernoulli(link = logit),
                       data = trials_pr_bl, 
                       iter = 4000,
                       seed= brm_seed,
                       prior = c(
                         prior(normal(0, 1), class = Intercept),
                         prior(normal(0, 1), class = b),
                         prior(gamma(1, 0.5), class = b, coef = "message_pref1"),
                         prior(normal(0, 2), class = sd)
                       )
)

get_prior(is_passive ~ lang_comb * prime_structure +
prime_lang + message_pref +
  (lang_comb * prime_structure || participant) +
  (1 || prime_id),
  family = bernoulli(link = logit),
  data = trials_pr_bl
)
summary(bm1_is_pass)

pdf(paste0("plots/Priming_cross_lang_LCandN_Exp", exp_abr, "_",
           file_suffix,".pdf"))
plot(bm1_is_pass)
dev.off() 

save(bm1_is_pass,
     file = paste0("output/Priming_cross_langLCandN_Exp", exp_abr, "_",
                   file_suffix,".rda"))


# Setup formatted labels ---------------------------------------------------####

predictor_to_label <- data.frame(
  predictor  = c("Intercept","prime_structure1", 
                 "message_pref1", "lang_combwithin", 
                 "prime_lang1", "lang_combwithin:prime_structure1", 
                 "dutch_passive2", "dutch_passive2:prime_structure1"),
  label = c("{\\sc Intercept}", "{\\sc Prime Structure}", 
            "{\\sc Target-Message Bias}", "{\\sc Language Combination}", 
            "{\\sc Prime Language}", "{\\sc Language Combination $\\times$ Prime Structure}", 
            "{\\sc Dutch Passive Structure}", "{\\sc Dutch Passive Structure $\\times$ Prime Structure}")
)

# Generate tables in LaTeX format-------------------------------------------####

fixed_effects_exp1 <- brmsfit_to_df(bm1_is_pass, predictor_to_label, 
                                    odds_ratios=FALSE)

table_caption <- paste0(
  "Summary of the fixed effects in the Bayesian logistic mixed-effects model (N = ", 
  nrow(bm1_is_pass$data), ") for Experiment 1.")

print(xtable(fixed_effects_exp1, align=c("l", "l", "r", "r", "r"), 
             caption =  table_caption), 
      include.rownames=FALSE, sanitize.text.function = function(x){x})



