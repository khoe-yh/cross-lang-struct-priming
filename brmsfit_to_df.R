# brmsfit_to_df ------------------------------------------------------------####
#
# Return results from a brmsfit object as a dataframe that can be printed as a
# LaTeX table
# Also prints probabilities of Est > 0 fro logging purposes.
#
# Author: Y.H. Khoe
# 
# Arguments
# brmsfit: a brmsfit object
# predictor_to_label: a data frame containing predictor labels in the brmsfit 
# object and the labels they should have in the output
# odds_ratios: Boolean indicating whether estimates should be converted to odds
# ratio's by exponentiating or not.
#
# Example of predictor_to_label data frame:
# predictor_to_label <- data.frame(
# predictor  = c("Intercept","prime_structure_bin2", 
#                "message_pref2", "c_meaning_percentage", 
#                "c_grammaticality_percentage", "c_input_percentage"),
# label = c("{\\sc Intercept}", "{\\sc Prime Structure}", 
#           "{\\sc Target-Message Bias}", "{\\sc Meaning Accuracy}", 
#           "{\\sc Syntactic Accuracy}", "{\\sc Input}")
# )


# Load libraries -----------------------------------------------------------####
library(dplyr)
library(tidyr)
library(brms)

brmsfit_to_df <- function(brmsfit, predictor_to_label, odds_ratios){
  
  # Get fixed effects ------------------------------------------------------####
  
  fixed_effects <- as.data.frame(fixef(brmsfit, summary = TRUE))
  
  # Get posterior probabilities of Est > 0, for null model -----------------####
  
  print("Posterior probabilities:")
  
  post_samples <- posterior_samples(brmsfit)
  
  fixed_effects[,"P(Est. $>$ 0)"] <- NA
  
  for (fixed_effect in rownames(fixed_effects)){
    print(fixed_effect)
    post_prob <- round(mean(post_samples[,paste0("b_",fixed_effect)] > 0), 2)
    print(post_prob)
    fixed_effects[fixed_effect,"P(Est. $>$ 0)"] <- post_prob
  }
  
  # Convert results to latex table -----------------------------------------####
  
  # add predictors in a column
  fixed_effects$Predictor <- rownames(fixed_effects)
  
  if (odds_ratios) {
    # Convert log-odds estimates to odds ratio's by exponentiating
    fixed_effects[,c("Estimate","Q2.5","Q97.5")] <- 
      exp(fixed_effects[,c("Estimate","Q2.5","Q97.5")])
  }
  
  # round estimates
  fixed_effects[,c("Estimate","Q2.5","Q97.5")] <- 
    round(fixed_effects[,c("Estimate","Q2.5","Q97.5")], digits = 2)
  
  # combine Q2.5 and Q97.5 into Credible Interval
  
  fixed_effects[,"95\\% CrI"] <- 
    paste0('[',sprintf("%.2f",fixed_effects$Q2.5), ", 
           ", sprintf("%.2f",fixed_effects$Q97.5),']')
  
  # Drop Estimated Error column
  fixed_effects <- fixed_effects[,c("Predictor", "Estimate","95\\% CrI",
                                    "P(Est. $>$ 0)")]
  
  if (odds_ratios) {
    colnames(fixed_effects)[colnames(fixed_effects) == "Estimate"] <- 
      "Odds Ratio"
  }

  # Rename predictors
  for (i in 1:nrow(predictor_to_label)){
    fixed_effects[
      fixed_effects$Predictor == predictor_to_label[i,"predictor"],c("Predictor")] <- 
      predictor_to_label[i,"label"]
    # print(predictor_to_label[i,"predictor"])
    # print(predictor_to_label[i,"label"])
  }
  
  return(fixed_effects)
}