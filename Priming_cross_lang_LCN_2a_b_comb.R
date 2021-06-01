# Perform an analysis that predicts whether the target sentence in a simulated
# priming experiment is passive.
# Use combined data from 2 experiments
# 
# Author: Y.H. Khoe


# Load libraries -----------------------------------------------------------####

library(dplyr)
library(tidyr)
library(brms)
library(ggplot2)
library(xtable)
source("brmsfit_to_df.R")

brm_seed = 20214
print("brm_seed:")
print(brm_seed)

options(mc.cores = parallel::detectCores())

# Load data ----------------------------------------------------------------####

# Use trials from revised final
trials_2a <- "output/trials_pr_bl_2a_nl_en_revised_final.rda"
trials_2b <- "output/trials_pr_bl_2b_nl_vm_en_revised_final.rda"

# Or trials from revised final 120 participants
# trials_2a <- "output/trials_pr_bl_2a_nl_en_revised_final_120.rda"
# trials_2b <- "output/trials_pr_bl_2b_nl_vm_en_revised_final_120.rda"

load(trials_2a)
trials_pr_bl_2a <- trials_pr_bl_1

load(trials_2b)
trials_pr_bl_2b <- trials_pr_bl_1

print("trials 2a:")
print(trials_2a)
print("trials 2b:")
print(trials_2b)

file_suffix <- "revised_final_clean2"

# Add Dutch Passive Structure
trials_pr_bl_2a$dutch_passive <- "vf"
trials_pr_bl_2b$dutch_passive <- "vm"

# Distinguish particpant ID's across the two expeiments
trials_pr_bl_2b$participant <- trials_pr_bl_2b$participant + 120

# Change prime_lang and target_lang from "nl_vm" to "nl" for Exp 2b
trials_pr_bl_2b$prime_lang <- ifelse(trials_pr_bl_2b$prime_lang == "nl_vm",
                                     "nl",
                                     "en")
trials_pr_bl_2b$target_lang <- ifelse(trials_pr_bl_2b$target_lang == "nl_vm",
                                     "nl",
                                     "en")

# Combine experiments, including only cross-language trials
trials_pr_bl <- rbind(
  trials_pr_bl_2a[trials_pr_bl_2a$prime_lang != trials_pr_bl_2a$target_lang,], 
  trials_pr_bl_2b[trials_pr_bl_2b$prime_lang != trials_pr_bl_2b$target_lang,])


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

# Create predictor Dutch Passive

trials_pr_bl$dutch_passive <- factor(trials_pr_bl$dutch_passive)

contrasts(trials_pr_bl$dutch_passive) = c(-0.5,+0.5)

print('Contrasts for dutch_passive:')
contrasts(trials_pr_bl$dutch_passive)


# Fit brms model -----------------------------------------------------------####

bm1_nl_en_is_pass <- brm(is_passive ~ dutch_passive * prime_structure + 
                          prime_lang + message_pref + 
                         (prime_structure || participant) +
                         (1 || prime_id), 
                       family = bernoulli(link = logit),
                       data = trials_pr_bl, 
                       iter = 8000,
                       seed = brm_seed,
                       warmup = 2000,
                       prior = c(
                         prior(normal(0, 1), class = Intercept),
                         prior(normal(0, 1), class = b),
                         prior(gamma(1, 0.5), class = b, coef = "message_pref1"),
                         prior(normal(0, 2), class = sd)#, 
                       )
)

summary(bm1_nl_en_is_pass)


save(bm1_nl_en_is_pass,
     file = paste0("output/Priming_cross_lang_LCandN.Exp2a_b_comb_nl_en_",
                   file_suffix,".rda"))


pdf(paste0("plots/Priming_cross_lang_act_pass_is_pass_nl_en_LCandN.Exp2a_b_comb_",
           file_suffix,".pdf"))
plot(bm1_nl_en_is_pass)
dev.off() 

# Setup formatted labels ---------------------------------------------------####

predictor_to_label <- data.frame(
  predictor  = c("Intercept","prime_structure1", 
                 "message_pref1", "lang_combwithin", 
                 "prime_lang1", "lang_combwithin:prime_structure1", 
                 "dutch_passive1", "dutch_passive1:prime_structure1"),
  label = c("{\\sc Intercept}", "{\\sc Prime Structure}", 
            "{\\sc Target-Message Bias}", "{\\sc Language Combination}", 
            "{\\sc Prime Language}", "{\\sc Language Combination $\\times$ Prime Structure}", 
            "{\\sc Dutch Passive Structure}", "{\\sc Dutch Passive Structure $\\times$ Prime Structure}")
)

# Generate tables in LaTeX format-------------------------------------------####

fixed_effects_exp1 <- brmsfit_to_df(bm1_nl_en_is_pass, predictor_to_label, 
                                    odds_ratios=FALSE)

table_caption <- paste0(
  "Summary of the fixed effects in the Bayesian logistic mixed-effects model (N = ", 
  nrow(bm1_nl_en_is_pass$data), ") for Experiment 1.")

print(xtable(fixed_effects_exp1, align=c("l", "l", "r", "r", "r"), 
             caption =  table_caption), 
      include.rownames=FALSE, sanitize.text.function = function(x){x})

