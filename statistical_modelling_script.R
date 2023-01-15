library('dplyr')
library('tidyr')
library('readxl')
library('tibble')
library('purrr')
library('mgcv')
library('itsadug')
library('gratia')
infoMessages('off')
set.seed(1)

dat <- readRDS(file = 'outputs/cleaned_data.RDS')

#Statistical Model Creation begins:

#a couple useful functions
#gets first obs for a given Tag. uses the fact that data is presented in chronological order. Necessary for bam with AR and grouping
first_observation <- function(data){
  Tag_new <- as.list(rep(TRUE, with(data, length(unique(Tag)))))
  names(Tag_new) <- with(data, unique(Tag))
  out <- c()
  for(i in 1:nrow(data)){
    new <- unlist(Tag_new[as.character(with(data, Tag[i]))])
    if(new){
      Tag_new[as.character(with(data, Tag[i]))] <- FALSE 
    }
    out[i] <- new
  }
  return(out)
}
lag <- function(subject, Tag, n = 1){
    tmp <- arrange(data.frame(subject,tag = Tag,position = 1:length(subject)), tag)
    out <- data.frame(subject = c(), position = c())
    for(t in unique(Tag)){
      tmp2 <- filter(tmp, tag == t)$subject
      tmp_pos <- filter(tmp, tag == t)$position
      out <- data.frame(subject = c(out$subject,
                                    c(rep(NA,n),tmp2[1:(length(tmp2)-n)])),
                              position = c(out$position, tmp_pos))
    }
    return(arrange(out, position)$subject)
}
#reduces data to binomial, lower recordings are inconsistent, drastically simplifies analysis and results
threshold <- function(x, threshold = 3) as.numeric(x>=threshold)


#bam enables us to construct AR(1) models with a fixed autoregression coefficient, but we cannot natively
#optimise for this. A naive solution (in that it has the potential to be very slow) is to just use model
#selection to home in on the appropriate constant, assuming there exists a single local maxima
#...it looks later like it turns out they're all either 0 or ~1 anyways

#finds best rho to (slightly better than) the nearest 1/2^precision. 

find_rho <- function(formula, data, first_observations, precision = 5, family = binomial){
  upper_rho_limit <- 0.99     #0<=rho<1, ensures errors are long run stationary around 0
  best_model <-suppressWarnings(bam(formula, rho = 0, data = data, 
                                    family = family, discrete = TRUE, AR.start = first_observations, method = 'fREML'))
  best_rho <- 0    
  #non-gam models can't be evaluated as fREML->can't use discrete option-> bam AR function fails.
  #there are ways to create AR(1) GLMs, but they're not trivial, and these models are not the main focus here
  if(attr(best_model$gcv.ubre, '') == 'REML') return(list(model = best_model, rho = best_rho))
  
  test_rho <- upper_rho_limit
  for(i in 1:precision+1){
    next_rho <- (best_rho+test_rho)/2
    test_model <- suppressWarnings(bam(formula, rho = test_rho, data = data,
                                       family = family, discrete = TRUE, AR.start = first_observations, method = 'fREML'))
    
    #compareML from itsadug is preferable to AIC/direct fREML selection for AR models. 
    comparison <- compareML(best_model, test_model,print.output = FALSE)
    #prefers a model if fREML is lower and Edf equal or lower. if this is true for neither model it's an
    #'inderminate case' & significance is determined by chi^2 with df equal to the difference of Edf
    #and value according to 2*difference of fREML. In truth, with fixed Edf it's easy inside find_rho
    if(ifelse(!is.na(comparison$advice),grepl('test',comparison$advice, fixed = T),FALSE)|
       ifelse(!is.null(comparison$table$Sig.),
              comparison$table$Sig.[2]!="   "& grepl('test',comparison$table$Model[2]),FALSE)){
      best_rho <- test_rho
      best_model <- test_model
    }
  }
  return(list(model = best_model, rho = best_rho))
}

antecedents <- list(buds = NA, flowers = 'buds',
                    green_fruits = 'flowers', ripe_fruits = 'green_fruits', fruits = 'flowers')


results <- data.frame(row.names = c('Namecode','subject', 'formulae','fREML', 'Best'))
non_AR_results <- data.frame(row.names = c('Namecode','subject', 'formulae','fREML', 'Best'))
best_models <- list()
best_non_AR_models <- list()
last_time <- Sys.time()

for(species in c(unique(dat$Namecode), 'global')){
  this_time <- Sys.time()
  print(paste0('Analysing ', species, '. Last Analysis took ', this_time-last_time, '. '))
  last_time <- this_time
  

  rho_precision = 7 #I question severely if you need this much. I was being thorough, but every model
                    #selected rho of either 0.99 or 0
  #NB: using lagged copies of e.g buds in the buds models is... easier, but flawed, better is AR(1)
  stored_data <- dat
  if(species!='global') {stored_data <- filter(dat, Namecode==species)}
  #species are considered to have 'bloomed' proportional to the max they ever have. Slightly flawed system,
  #as an extension of the flawed system of ranking from 0 to 5.
  stored_data <-mutate(stored_data, buds_lagged = lag(buds, Tag), flowers_lagged = lag(flowers,Tag), 
                       green_fruits_lagged = lag(green_fruits,Tag),
                       buds = threshold(buds, ceiling(max(buds,na.rm = T)/2)),
                       flowers = threshold(flowers, ceiling(max(flowers,na.rm = T)/2)),
                       green_fruits = threshold(green_fruits, ceiling(max(green_fruits,na.rm = T)/2)),
                       ripe_fruits = threshold(ripe_fruits, ceiling(max(flowers,na.rm = T)/2)),
                       fruits = threshold(fruits))
  #yeah, helps prevent intercept errors from going to crazy and makes them more interpretable.
  stored_data$year <- stored_data$year-median(stored_data$year)
  
  for(subject in c('buds', 'flowers', 'green_fruits', 'ripe_fruits', 'fruits')){
    if (subject!='buds'){
      active_data <- stored_data[!is.na(stored_data[,subject]) &
                                   !is.na(stored_data[,antecedents[[subject]]]),]
    } else active_data <- stored_data[!is.na(stored_data[,subject]),]
    
    active_data <- mutate(active_data, first_observations = first_observation(active_data))
    
    #NB: k values are fundamentally arbitrary, and issues due to low k are generally detected with methods that
    # fail for non-gaussian residuals. generally striking a balance between size and computational speed, but
    #we're often limited above here because k can't be greater than distinct data points
    #Apparently a point doesn't count for the above if it's unique and the first one, lmao.
    #There isn't profoundly strong justification for picking tp smooths, but all choices agree in most cases, 
    # and tp are considered a generally good choice, and they're in some sense 'best
    
    k_year <- n_distinct(active_data$year) - !(min(active_data$year) %in% active_data$year[-1]) 
    year_components <-c('1', 'year', paste0('s(year, bs = "tp", k = ',k_year,')')) #1 doesn't impact formulae
    tag_components <- ifelse(n_distinct(active_data$Tag)>1,c('', ' + s(Tag, bs = "re")'),c('')) #can't have random effects if there's only 1 tree
    
    k_climate <- ifelse(nrow(active_data)>144,12,floor(sqrt(nrow(active_data))))
    climate_components <- c('', ' + te(temperatures,vpds, bs = "tp", k = 12)',
                            ' + te(temperatures, vpds, bs ="tp", k = 12)+s(julian_month, bs = "cc", k = 12)')#not interested in models with month/no climate
    
    
    
    #lagged variables presented as smooths rather than factors. this is not really a value neutral decision.
    #Tags as smooths are less formally justified than GLM style factor usage: makes (slightly) more assumptions
    #about the structure between the ratings (due to wiggliness penalties). 
    #However, this way enables models consisting of just lags to be compared to others. GLMs can't use fREML
    antecedent_components <- list(buds = c(), 
                                  flowers = c('',ifelse(n_distinct(active_data$buds_lagged,na.rm = T)>2,
                                                        paste0(' + s(buds_lagged, bs = "tp", k =',
                                                               n_distinct(active_data$buds_lagged,na.rm = T),')'),
                                                        list(NULL))%>%unlist),
                                  green_fruits = c('',ifelse(n_distinct(active_data$flowers_lagged,na.rm = T)>2,
                                                             paste0(' + s(flowers_lagged, bs = "tp", k =',
                                                                    n_distinct(active_data$flowers_lagged,na.rm = T),')'),
                                                             list(NULL))%>%unlist),
                                  ripe_fruits = c('',ifelse(n_distinct(active_data$green_fruits_lagged,na.rm = T)>2,
                                                            paste0(' + s(green_fruits_lagged, bs = "tp", k =',
                                                                   n_distinct(active_data$green_fruits_lagged,na.rm = T),')'),
                                                            list(NULL))%>%unlist),
                                  fruits = c('',ifelse(n_distinct(active_data$flowers_lagged,na.rm = T)>2,
                                                       paste0(' + s(flowers_lagged, bs = "tp", k =',
                                                              n_distinct(active_data$flowers_lagged,na.rm = T),')'),
                                                       list(NULL))%>%unlist))
    
    #all combinations are fed to as.formula() as strings, from whence they can be fed into bam() 
    formulae <- expand_grid(year_components, climate_components, tag_components,
                            antecedent_components[[subject]])%>%
      apply(1, function(x) paste(x, collapse = '')%>%
              paste(subject,'~', .))%>% #only take formulae with at least one smooth, breaks analysis elsewise
      .[grepl('(',.,fixed = T)]#due to GLMs being REML only and AR(1) models being fREML only.
    
    best_formula <- 1
    best_model <- suppressWarnings(find_rho(as.formula(formulae[[1]]), active_data,
                                            first_observations = active_data$first_observations, precision = rho_precision))
    best_non_AR_formula <- 1
    best_non_AR_model <- suppressWarnings(bam(as.formula(formulae[[1]]), family = binomial, data = active_data,
                                              discrete = T, method = 'fREML'))
    tmp <-data.frame(Namecode = species, subject = subject, formula = formulae[[1]], 
                     fREML = unname(best_model$model$gcv.ubre), Best = FALSE)
    tmp_non_AR <- data.frame(Namecode = species, subject = subject, formula = formulae[[1]], 
                             fREML = unname(best_non_AR_model$gcv.ubre), Best = FALSE)
    pb <- txtProgressBar(min = 1, max = length(formulae), initial = 1)
    for (i in 2:length(formulae)){
      setTxtProgressBar(pb, i)
      test_model <-suppressWarnings(find_rho(as.formula(formulae[[i]]), active_data,
                                             first_observations = active_data$first_observations, precision = rho_precision))
      test_non_AR_model <- suppressWarnings(bam(as.formula(formulae[[i]]), family = binomial, data = active_data,
                                                discrete = T, method = 'fREML'))
      #see findRho() for an explanation of the comparison process
      comparison <- compareML(best_model$model, test_model$model,print.output = FALSE)%>%suppressWarnings()
      if(ifelse(!is.na(comparison$advice),grepl('test',comparison$advice, fixed = T),FALSE)|
         ifelse(!is.null(comparison$table$Sig.),
                comparison$table$Sig.[2]!="   "& grepl('test',comparison$table$Model[2]),FALSE)){
        best_formula <- i
        best_model <- test_model
      }
      comparison <- compareML(best_non_AR_model, test_non_AR_model,print.output = FALSE)%>%suppressWarnings()
      if(ifelse(!is.na(comparison$advice),grepl('test',comparison$advice, fixed = T),FALSE)|
         ifelse(!is.null(comparison$table$Sig.),
                comparison$table$Sig.[2]!="   "& grepl('test',comparison$table$Model[2]),FALSE)){
        best_non_AR_formula <- i
        best_non_AR_model <- test_non_AR_model
      }
      tmp <- rbind(tmp, data.frame(Namecode = species, subject = subject,formula = formulae[[i]], 
                                   fREML = unname(test_model$model$gcv.ubre),Best = FALSE))
      tmp_non_AR <-rbind(tmp_non_AR, data.frame(Namecode = species, subject = subject,formula = formulae[[i]], 
                                                fREML = unname(test_non_AR_model$gcv.ubre),Best = FALSE))
    }
    close(pb)
    tmp$Best[best_formula]=TRUE
    tmp_non_AR$Best[best_non_AR_formula]=TRUE
    results <- rbind(results, tmp)
    non_AR_results <- rbind(non_AR_results, tmp_non_AR)
    best <- list(best_model)
    names(best) <- species
    names(best) <- paste(species,subject, sep = '_')
    best_non_AR <- list(best_non_AR_model)
    names(best_non_AR) <- species
    names(best_non_AR) <- paste(species,subject, sep = '_')
    best_models <- append(best_models, best)
    best_non_AR_models <- append(best_non_AR_models, best_non_AR)
  }
  saveRDS(best_models, file = 'outputs/best_models.RDS')
  saveRDS(results, file = 'outputs/formula_results.RDS')
  saveRDS(best_non_AR_models, file = 'outputs/best_non_AR_models.RDS')
  saveRDS(non_AR_results, file = 'outputs/non_AR_formula_results.RDS')
}