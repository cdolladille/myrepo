# say i am working on something.

# and i keep working on it

# transformation to absolute and relative chances of success
# this function is meant to create contrasts (absolute, relative) of meta-regression of single proportion. See pierre_ollitrault/af_ablation/todo.docx for more details

# this is the place to the new feature
# a bit of extra work
# and its done!

arcs <- function(
  data,
  est_col, # character string, column name of estimate from logit transformed regression
  std_err_col, # character string, column name of standard error of estimate
  clinical_coef, # character string, column name of clinically meaningful relative probability of success (column value is numeric)
  mean_prob = .65 # numeric, what is the mean prediction of your model (which is also the baseline value to which absolute and relative chances of success will be drawn)? see to do.docx for details
){
  # as.data.table checker and transformer
  if(!is.data.table(data)){
    data <- as.data.table(data)
  }
  
  # syms

  est_s <- sym(est_col)
  std_s <- sym(std_err_col)
  clin_s <- sym(clinical_coef)
  
  mean_prob_logit <- log(mean_prob / (1 - mean_prob))
  
  # relative chances from mean probability
  eval(expr(
    data %>%
      mutate(
        abs_est =
          logit2prob(mean_prob_logit +
                       !!clin_s * !!est_s) - mean_prob,
        rel_est = 
          abs_est / mean_prob,
        
        abs_ci_1 = 
          logit2prob(mean_prob_logit +
                       !!clin_s * (!!est_s - qnorm(1 - .025) * !!std_s)) - mean_prob,
        abs_ci_2 =
          logit2prob(mean_prob_logit +
                       !!clin_s * (!!est_s + qnorm(1 - .025) * !!std_s)) - mean_prob,
        abs_low_ci = pmin(abs_ci_1, abs_ci_2),
        abs_up_ci = pmax(abs_ci_1, abs_ci_2),
        
        rel_ci_1 = 
          abs_ci_1 / mean_prob,
        rel_ci_2 =
          abs_ci_2 / mean_prob,
        
        rel_low_ci = 
          pmin(rel_ci_1, rel_ci_2),
        rel_up_ci =
          pmax(rel_ci_1, rel_ci_2),
        across(c(abs_est, rel_est, abs_low_ci, abs_up_ci, rel_low_ci, rel_up_ci), ~ . * 100),
        abs_leg =
          cff(num = abs_est,
              low_ci = abs_low_ci,
              up_ci = abs_up_ci,
              method = "num_ci",
              dig = 0),
        rel_leg =
          cff(num = rel_est,
              low_ci = rel_low_ci,
              up_ci = rel_up_ci,
              method = "num_ci",
              dig = 0)
      )
  ))
}
