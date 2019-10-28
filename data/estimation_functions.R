library('foreach'); library(doParallel); library('tidyverse')

drop_constcols <- function(df){
  df[,!apply(df, MARGIN = 2, function(x) length(unique(x))==1)]
}

demean <- function(x){
  x - mean(x, na.rm = TRUE)
}


mse <- function(x, y, outcome){
  coefs <- coef(x)
  coefs <- coefs[is.na(coefs) == FALSE]
  if(length(coefs) <= 2){
    mean((y[[outcome]] - mean(y[[outcome]] ))^2 )
  }
  else{
    mean((y[[outcome]] -
          (model.matrix(as.formula(paste("~ 1 + ",
                                         paste(names(coefs)[-1], collapse = " + "))), y)
            %*%
            coefs))^2)
  }
}


lasso_ols <- function(df, s, outcome, covar_names, block_names, lambda){
  ## This function runs a lasso regression to select covariates for inclusion in an ols model
  ##"df" must have a variable called "resid_outcome" which is  an outcome variable "residualized" for block effects.
  lasso_mod <- suppressWarnings(glmnet::glmnet(x = as.matrix(df[, c(covar_names)]),
                                               y = df[[outcome]], alpha = 1,
                                               lambda = lambda, pmax = 1000))
  if(lasso_mod$df[1] == 0){
    vars_lasso <- 0
  }
  else{
    vars_lasso <- na.omit(covar_names[unlist(predict(lasso_mod, df, s = s, type = "nonzero"))] )
  }
  if(length(vars_lasso) == 0){
    lm.fit(x = model.matrix(as.formula(paste("~ 1")),
                            data = df[, covar_names]),
           y = df[[outcome]])
  }
  else{
    lm.fit(x = model.matrix(as.formula(paste("~ 1 + ", paste(vars_lasso, collapse = " + "))),
                            data = df[, covar_names]),
           y = df[[outcome]])
  }
}


cv_error_lasso_ols <- function(data, s, outcome, lambda, covar_names, block_names){

  test_indxs <- caret::createFolds(data[[outcome]], k = 10)
  test_sets <- lapply(test_indxs, function(x) data[x,])
  train_sets <- lapply(test_indxs, function(x) data[-1 * x,])

  cv_models <- lapply(train_sets, function(x)
    lasso_ols(df = x, s = s, outcome = outcome, lambda = lambda, covar_names = covar_names, block_names = block_names)
    )
  mse_models <- ((mapply(mse, x = cv_models, y = test_sets, MoreArgs = list(outcome = outcome),
                        SIMPLIFY = TRUE)))
  se_cv <- sd(mse_models)/sqrt(10)
  data_frame(mse_cv= mean(mse_models), se_cv = se_cv, lambda = s)
}

permute_estim <- function(reg_data, block, treatment, outcome, mod_form, moderator = NULL){
  reg_data$block <- block
  reg_data$treat <- reg_data[[treatment]]
  reg_data <- group_by(reg_data, block) %>%
    mutate(treat_perm = sample(treat),
           tr_prob = mean(treat_perm))
  reg_data[[treatment]] <- reg_data$treat_perm
  mod <- lm(mod_form, data = reg_data)

  ests <- get_ests(mod = mod, unadj_mod = mod,
                  treatment = treatment, moderator = moderator) #Note that does not work for unadjusted estimator yet
  ests$mod <- NULL
  ests$unadj_mod <- NULL
  ests
}

get_ests <- function(mod, unadj_mod, treatment, moderator = NULL){
  ate_est <- coef(mod)[treatment]
  #Calculate Robust Standard Errors
  vcov_hc2  <- sandwich::vcovHC(mod, type = "HC")
  ate_se <- sqrt(vcov_hc2[treatment, treatment])
  ate_tstat <- ate_est / ate_se
  ate_pval <- pt(abs(ate_tstat), df.residual(mod), lower.tail = FALSE)

  if(is.null(moderator) == FALSE){
    coefs <- coef(mod)
    inter_indx <- which(paste0(treatment, ":demean(", moderator, ")") == names(coefs))
    inter_est <- coefs[inter_indx]
    inter_indx <- which(paste0(treatment, ":demean(", moderator, ")") == rownames(vcov_hc2))
    inter_se <- sqrt(vcov_hc2[inter_indx, inter_indx])
    inter_tstat <- inter_est / inter_se
    inter_pval <- pt(abs(inter_tstat), df.residual(mod), lower.tail = FALSE)
  }

  vcov_hc2_unadj <- sandwich::vcovHC(unadj_mod, type = "HC")
  ate_unadj <- coef(unadj_mod)[treatment]
  ate_se_unadj <- sqrt(vcov_hc2_unadj[treatment, treatment])
  if(is.null(moderator)){
    estimate <- tibble(outcome =  as.character(mod$terms[[2]]),
                      treatment = treatment,
                      ate_est = ate_est, ate_unadj = ate_unadj,
                      ate_se = ate_se, ate_se_unadj = ate_se_unadj,
                      ate_tstat = ate_tstat, ate_pval = ate_pval,
                      n = nobs(mod),
                      mod = list(mod),
                      unadj_mod = list(unadj_mod),
                      mod_vcov = list(vcov_hc2)
                      )
  }
  if(is.null(moderator) == FALSE){
    estimate <- tibble(outcome =  as.character(mod$terms[[2]]), treatment = treatment,
                      ate_est = ate_est, ate_unadj = ate_unadj,
                      ate_se = ate_se, ate_se_unadj = ate_se_unadj,
                      ate_tstat = ate_tstat, ate_pval = ate_pval,
                      n = nobs(mod), moderator = moderator,
                      inter_est = inter_est, inter_se = inter_se,
                      inter_tstat = inter_tstat, inter_pval = inter_pval,
                      mod = list(mod),
                      unadj_mod = list(unadj_mod),
                      mod_vcov = list(vcov_hc2))
  }
  estimate
}



est_ate <- function(outcome, treatment, covars, block, data, moderator = NULL,
                   unregularized_covars = NULL, permute = TRUE, permute_num = 1000,
                   regularize = TRUE, num_cores = detectCores()){
  ## This function implements the lasso / ols estimator for experimental treatment effects

  demean <- function(x){  x - mean(x, na.rm = TRUE)}
  #Create a matrix of block dummies
  data[[block]] <- as.factor(data[[block]])
  data[, lapply(data, class) == "character"] <-  apply((data[, lapply(data, class) == "character"]), 2, as.factor)

  ## Remove observations with missing outcome data
  num_missing_outcome <- sum(is.na(data[outcome]))
  if(num_missing_outcome > 0 ){
    # cat(paste0("Dropping ", num_missing_outcome, " units due to missing outcome data\n"))
    data <- data[!is.na(data[outcome]), ]
  }

  treatment_vector <- data[[treatment]]
  data$treatment_vector <-treatment_vector
  data <- group_by_(data, block) %>%
    mutate(n_block= n(),
           tr_prob = mean(treatment_vector),
           co_prob = 1- tr_prob) %>%
    filter(tr_prob != 0 & tr_prob != 1)
  data$treatment_vector <- NULL
  treatment_vector <- data[[treatment]]

  block_matrix <- model.matrix(formula(paste0("~ ", block)), data,
                              contrasts.arg = lapply(data[, block], contrasts, contrasts = FALSE))[,-1] %>%
    drop_constcols() ## The "contrasts.arg" setting ensures that model.matrix does not remove one of the columns when transforming a factor variable to a dummy matrix.
  if(length(covars)>0){
    #Create a matrix of covariates
    if (sum(sapply(data[, covars], is.factor)) > 0) {
      covar_matrix <- model.matrix(formula(paste0("~ -1 + ", paste(covars, collapse = "+"))), data[,covars],
                                  contrasts.arg = lapply(data[,covars][,sapply(data[, covars], is.factor)], contrasts, contrasts = FALSE)) %>%
        drop_constcols()
    }
    else{
      covar_matrix <- model.matrix(formula(paste0("~ -1 + ", paste(covars, collapse = "+"))), data[,covars]) %>%
        drop_constcols()
    }
    ## Create a "residualized" outcome that removes block effects
    data$resid_outcome <- data[[outcome]] - ave(data[[outcome]], paste0(data[[treatment]], data[[block]]), FUN = mean)
    colnames(block_matrix) <- make.names(colnames(block_matrix))
    colnames(covar_matrix) <- make.names(colnames(covar_matrix))
    if(regularize == TRUE){

      covar_matrix_resid <- covar_matrix
      covar_matrix_resid[treatment_vector == 1,] <- apply(covar_matrix_resid[treatment_vector == 1,], 2,
                                                         function(x) lm.fit(y = x,
                                                                            x = block_matrix[treatment_vector == 1,])$residuals)
      covar_matrix_resid[treatment_vector == 0,] <- apply(covar_matrix_resid[treatment_vector == 0,], 2,
                                                         function(x) lm.fit(y = x,
                                                                            x = block_matrix[treatment_vector == 0,])$residuals)
      ## Create data set for estimation
      reg_data <- cbind(data[outcome], data[treatment], block_matrix,  covar_matrix_resid)
      reg_data$resid_outcome <- data$resid_outcome
      treat_lasso <- glmnet::cv.glmnet(x = covar_matrix_resid[treatment_vector == 1, ],
                                      y = reg_data[treatment_vector == 1,][[outcome]], alpha = 1, nlambda = 100)
      contr_lasso <- glmnet::cv.glmnet(x = covar_matrix_resid[treatment_vector == 0, ],
                                      y = reg_data[treatment_vector == 0 ,][[outcome]], alpha = 1, nlambda = 100)

      ## Smoothing parameters to evaluate with cross-validation
      lambda_treat <- sort(treat_lasso$lambda, decreasing = TRUE)
      lambda_contr <- sort(contr_lasso$lambda, decreasing = TRUE)
      ##  Perform CV to estimate MSE for different values of smoothing parameters
      ## Choose optimal S

      treat_lasso <- suppressWarnings(glmnet::cv.glmnet(x = covar_matrix_resid[treatment_vector == 1, ],
                                                       y = reg_data[treatment_vector == 1,][[outcome]], alpha = 1, nlambda = 100, pmax = 1000))
      contr_lasso <- suppressWarnings(glmnet::cv.glmnet(x = covar_matrix_resid[treatment_vector == 0, ],
                                                       y = reg_data[treatment_vector == 0 ,][[outcome]], alpha = 1, nlambda = 100, pmax = 1000))

      ## Select covariates for the treatment group
      ## cat("\nUsing Cross Validation to Choose Optimal Regularization Parameter\n")
      cl <- makeCluster(num_cores)
      registerDoParallel(cl)
      num_lambda <- length(lambda_treat)
      treat_cv_mse <- foreach(i = 1:num_lambda,
                             .export = c("cv_error_lasso_ols", "lasso_ols", "mse"),
                             .packages = c("caret", "glmnet", "dplyr")
                             ) %dopar%
        cv_error_lasso_ols(data = filter_(reg_data, paste(treatment, "==1")), s = lambda_treat[i], outcome = outcome,
                           lambda = lambda_treat,  covar_names = colnames(covar_matrix_resid),
                           block_names = colnames(block_matrix))
      stopCluster(cl)
      treat_cv_mse <- treat_cv_mse %>%
        bind_rows() %>%
        arrange(mse_cv)


      min_lambda <- with(treat_cv_mse, lambda[which(mse_cv == min(mse_cv))])
      min_lambda_se <- with(treat_cv_mse, se_cv[which(mse_cv == min(mse_cv))])
      treat_cv_mse <- filter(treat_cv_mse, mse_cv > (min(mse_cv) + min_lambda_se))
      if(nrow(treat_cv_mse) == 0){
        treat_optimal_lambda <- min_lambda
      }
      else{
        treat_optimal_lambda <- treat_cv_mse$lambda[1]
      }
      treat_selected_covars <- rownames(coef(treat_lasso))[predict(treat_lasso, s = treat_optimal_lambda, type = "nonzero")$X1]
      treat_selected_covars <- treat_selected_covars[treat_selected_covars != "(Intercept)" & treat_selected_covars %in% colnames(block_matrix) == FALSE]

      ## Select Covariates for the control group


      cl <- makeCluster(num_cores)
      registerDoParallel(cl)
      num_lambda <- length(lambda_contr)
      contr_cv_mse <- foreach(i = 1:num_lambda,
                             .export = c("cv_error_lasso_ols", "lasso_ols", "mse"),
                             .packages = c("caret", "glmnet", "dplyr")
                             ) %dopar%
        cv_error_lasso_ols(data = filter_(reg_data, paste(treatment, "==0")), s = lambda_contr[i], outcome = outcome,
                           lambda = lambda_contr,  covar_names = colnames(covar_matrix_resid),
                           block_names = colnames(block_matrix))
      stopCluster(cl)
      contr_cv_mse <- contr_cv_mse %>%
        bind_rows() %>%
        arrange(mse_cv)

      min_lambda <- with(contr_cv_mse, lambda[which(mse_cv == min(mse_cv))])
      min_lambda_se <- with(contr_cv_mse, se_cv[which(mse_cv == min(mse_cv))])
      contr_cv_mse <- filter(contr_cv_mse, mse_cv > (min(mse_cv) + min_lambda_se))
      if(nrow(contr_cv_mse) == 0){
        contr_optimal_lambda <- min_lambda
      }
      else{
        contr_optimal_lambda <- contr_cv_mse$lambda[1]
      }
      contr_selected_covars <- rownames(coef(contr_lasso))[predict(contr_lasso, s = contr_optimal_lambda,
                                                                  type = "nonzero")$X1]
      contr_selected_covars <- contr_selected_covars[contr_selected_covars != "(Intercept)" &
                                                    contr_selected_covars %in% colnames(block_matrix) == FALSE]

      lasso_selected_covars <- base::union(treat_selected_covars, contr_selected_covars)
    }

    reg_data <- cbind(data[outcome], data[treatment], block_matrix,  covar_matrix)
  }
  ##Specify model with demeaned covariates and treatment by covariate interactions
  if(length(covars) == 0){
    reg_data <- cbind(data[outcome], data[treatment], block_matrix)
    lasso_selected_covars <- NA
  }
  unadj_reg_data <- cbind(data[outcome], data[treatment], block_matrix)
  if(regularize == FALSE){
    lasso_selected_covars <- c()
    unregularized_covars <- c(covars, unregularized_covars)
  }
  if(is.null(moderator) == TRUE){
    mod_form <- formula(paste0(outcome, "~",
                              paste(c(treatment,
                                      paste0(treatment,
                                             " * demean(", make.names(c(unregularized_covars,
                                                                        lasso_selected_covars, colnames(block_matrix))),")")),
                                    collapse = "+")))

    unadj_mod_form <- formula(paste0(outcome, "~",
                                    paste(c(treatment,
                                            paste0(treatment, " * demean(", make.names(c(colnames(block_matrix))),")")),
                                          collapse = "+")))
  }
  if(is.null(moderator) == FALSE){
    reg_data <- unique(cbind(reg_data, data[moderator]))
    unadj_reg_data <- cbind(unadj_reg_data, data[moderator])
    mod_form <- formula(paste0(outcome, "~",
                              paste(c(treatment, paste0(treatment, " * demean(", moderator, ")"),
                                      paste0(treatment, " * demean(", make.names(c(unregularized_covars,
                                                                     lasso_selected_covars, colnames(block_matrix))),")")),
                                    collapse = "+")))

    if(length(covars) == 0){
      lasso_selected_covars <- NA
    }

    unadj_mod_form <- formula(paste0(outcome, "~",
                                    paste(c(treatment, paste0(treatment, " * demean(", moderator, ")"),
                                            paste0(treatment, " * demean(", make.names(c(colnames(block_matrix))),")")),
                                          collapse = "+")))
  }
  reg_data <- reg_data[ , !duplicated(colnames(reg_data))]
  unadj_mod <- lm(unadj_mod_form, data = unadj_reg_data)

  if(length(covars) == 0 & length(unregularized_covars) == 0){
    reg_data <- unadj_reg_data
    mod_form <- unadj_mod_form
  }
  mod <- lm(mod_form, data = reg_data)

  results <- get_ests(mod = mod, unadj_mod = unadj_mod, treatment = treatment, moderator = moderator)
  results$selected_covars <- list(lasso_selected_covars)
  results$contr_mean <- mean(reg_data[[outcome]][treatment_vector == 0], na.rm = TRUE)
  results$mod_data <- list(reg_data)

  ## cat("\n\n Implementing a Permutation Test\n")
  if(permute == TRUE){

    cl <- makeCluster(num_cores)

    registerDoParallel(cl)
    perm_results <- foreach(i=1:permute_num,
                           .export = c("permute_estim", "get_ests", "demean"),
                           .packages = c('tidyverse', 'sandwich')) %dopar%
      permute_estim(reg_data, block, treatment, outcome, mod_form, moderator = moderator)
    stopCluster(cl)
    if(is.null(moderator) == TRUE){
      perm_results <- perm_results %>%
        bind_rows() %>%
        summarise(perm_pvalue = ifelse(results$ate_tstat > 0, mean(ate_tstat >= results$ate_tstat),
                                       mean(ate_tstat <= results$ate_tstat)))
      results$perm_pvalue <- perm_results$perm_pvalue
    }
    if(is.null(moderator) == FALSE){

      perm_results <- perm_results %>%
        bind_rows() %>%
        summarise(perm_pvalue = ifelse(results$ate_tstat > 0, mean(ate_tstat >= results$ate_tstat),
                                       mean(ate_tstat <= results$ate_tstat)),
                  inter_perm_pvalue = ifelse(results$inter_tstat > 0,
                                             mean(inter_tstat >= results$inter_tstat),
                                             mean(inter_tstat <= results$inter_tstat)))
      results$perm_pvalue <- perm_results$perm_pvalue
      results$perm_inter_pvalue <- perm_results$inter_perm_pvalue
    }
  }
  results
}



binary_po_maker <- function(y, treat_vector, ate, block) {
  block_means <- ave(y, as.factor(block), FUN=mean)
  prob_t <- block_means + ate
  prob_t[prob_t >  1] <- 1
  prob_t[prob_t < 0] <- 0
  prob_c <- block_means
  po_t <- rbinom(n=length(prob_t), size=1, prob=prob_t)
  po_c <- rbinom(n=length(prob_c), size = 1, prob = prob_c)
  y_obs <- (treat_vector * po_t) + ((1-treat_vector) * po_c)
  y_obs
}




treat_sim <- function(outcome, treatment, covars, block, data, outcome_model, ate) {
  print(ate)
  data <-  group_by_(data, block) %>%
    mutate_(treat_perm=paste('sample(', treatment, ')'))
  data$sim_outcome <- outcome_model(data[[outcome]], treat_vector=data[['treat_perm']], ate=ate, block=data[[block]])
  ests <- est_ate(outcome = 'sim_outcome', treatment = 'treat_perm',
                  covars = covars, block = block,
                  data = data)
  ests$sim_ate <- ate
  ests$outcome <- outcome
  ests$treatment <- treatment
  ests$model <- NULL
  ests
}


get_rsq <-  function(mod_list){map_df(mod_list,
                                     function(x) glance(x$mod[[1]]), .id = "model") %>%
                                select(model, r.squared)}
