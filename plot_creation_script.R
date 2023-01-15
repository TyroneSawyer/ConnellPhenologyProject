library('dplyr')
library('tidyr')
library('readxl')
library('tibble')
library('purrr')
library('mgcv')
library('itsadug')
library('gratia')
library('ggplot2')
library('patchwork')
library('RColorBrewer')
infoMessages('off')
set.seed(1) #shouldn't do anything, I'm just paranoid

####################################################################################################
#Data Presentation: Doesn't save any outputs unless you've set do_plots=T in the environment somewhere.
#in a faintly similar vein, the other interesting little tables and such I've built are just generated here
#cuz they're pretty quick

dat <- readRDS(file = 'outputs/cleaned_data.RDS')
best_models <- readRDS('outputs/best_models.RDS')
best_non_AR_models <- readRDS('outputs/best_non_AR_models.RDS')
results <- readRDS('outputs/formula_results.RDS')
non_AR_results <- readRDS('outputs/non_AR_formula_results.RDS')

#some useful categorisations for later queries, plus a summary statistic of which models were used
#any new splines for model consideration need to be added here /manually/
models <- results[results$Best,]$formula
non_AR_models <- non_AR_results[non_AR_results$Best,]$formula
model_components <- models%>%{data.frame(year_spline = grepl('s(year',., fixed = T),
                               climate_spline = grepl('vpd', .),
                               month_spline = grepl('julian', .),
                               tag_spline = grepl('Tag', .),
                               buds_spline = grepl('s(buds', ., fixed = T),
                               flowers_spline = grepl('s(flowers', ., fixed = T),
                               green_fruits_spline = grepl('s(green_fruits', ., fixed = T))}
non_AR_model_components <- non_AR_models%>%{data.frame(year_spline = grepl('s(year',., fixed = T),
                                                                climate_spline = grepl('vpd', .),
                                                                month_spline = grepl('julian', .),
                                                                tag_spline = grepl('Tag', .),
                                                                buds_spline = grepl('s(buds', ., fixed = T),
                                                                flowers_spline = grepl('s(flowers', ., fixed = T),
                                                                green_fruits_spline = grepl('s(green_fruits', ., fixed = T))}
model_components <- models%>%{cbind(model_components,
                                     data.frame(linear_year = grepl('year',.) & !model_components$year_spline))}
non_AR_model_components <- non_AR_models%>%{cbind(non_AR_model_components,
                                                  data.frame(linear_year = grepl('year',.) & !non_AR_model_components$year_spline))}
component_counts <- apply(model_components,2,sum)
non_AR_component_counts <- apply(non_AR_model_components,2,sum)
num_components <- apply(model_components,1,sum)
num_non_AR_components <- apply(non_AR_model_components,1,sum)



#complexity of models
outs_full <- cbind(data.frame(
  'Number of AR Terms' = num_components,
  'AR fREML' = sapply(best_models, function(x) x$model$gcv.ubre[[1]]),
  'AR Effective Degrees of Freedom' = sapply(best_models,function(x){
    sum(x$model$edf+ length(x$model$sp)+
          sum(sapply(x$model$smooth, function(y) y$null.space.dim)))
  }),
  'Rho' = sapply(best_models, function(x) x$rho)),
  'Number of non AR Terms' = num_non_AR_components,
  'Non-AR fREML' = sapply(best_non_AR_models, function(x) x$gcv.ubre[[1]]),
  'Non-AR Effective Degrees of Freedom' = sapply(best_non_AR_models, function(x){
    sum(x$edf+ length(x$sp)+
          sum(sapply(x$smooth, function(y) y$null.space.dim)))
  }),
  'Non-AR Deviance Explained' = sapply(best_non_AR_models, function(x) summary(x)%>%.$dev.expl),
  model_components[,8], model_components[,c(1:4)],
  model_components[,5]| model_components[,6]|model_components[,7],
  non_AR_model_components[,8], non_AR_model_components[,c(1:4)],
  non_AR_model_components[,5]| non_AR_model_components[,6]|non_AR_model_components[,7])
names(outs_full)[9:20] <- c('AR contains Parametric Year','AR contains Year Smooth',
                             'AR contains Climate Smooth',
                             'AR contains Month Smooth', 'AR contains Tag Random Effect',
                             'AR contains Antecedent Spline','non-AR contains Parametric Year',
                             'non-AR contains Year Smooth',
                             'non-AR contains Climate Smooth','non-AR contains Month Smooth',
                             'non-AR contains Tag Random Effect','non-AR contains Antecedent Spline')
summary_out <- data.frame(apply(outs_full, 2, summary))%>%slice(c(4,1:3, 5,6))


#summary of those 15 models with rho = 0
rho_0_models <- sapply(best_models,function(x) x$rho)==0
rho_0_models <- outs_full[rho_0_models,]

subject <- c('1'= 'Buds', '2' = 'Flowers', '3' = 'Green Fruits', '4' = 'Ripe Fruits', '5' = 'Fruits')

by_subject <- data.frame(row.names = rownames(summary_out))
for(i in 1:5){
  by_subject <- cbind(by_subject, apply(outs_full[seq(i,210, by = 5),c(1,3,4,5,7,8)],2,summary))
                                              
}
by_subject <- by_subject%>%{data.frame(cbind('Buds' = c(.[4,1],.[3,2],.[4,3:4],.[3,5:6]),
                               'Flowers' = c(.[4,7],.[3,8],.[4,9:10],.[3,11:12]),
                               'Green Fruits' = c(.[4,13],.[3,14],.[4,15:16],.[3,17:18]),
                               'Ripe Fruits' = c(.[4,19],.[3,20],.[4,21:22],.[3,23:24]),
                               'Fruits' = c(.[4,25],.[3,26],.[4,27:28],.[3,29:30])))}
by_subject[3,] <- unlist(by_subject[3,])%>%`*`(42/0.99)
rownames(by_subject) <- c('Mean # of AR terms', 'Median Edf of AR models',
                          '# AR Models with Rho = 0', 'Mean # of non-AR terms', 'Median Edf of non-AR models',
                          'Median Deviance Explained by non-AR models')

averages <- data.frame(cbind('AR Models' = c(summary_out[4,1:3],100*summary_out[1,c(9:14)]), 'Non-AR Models' = c(summary_out[4,5:7],100*summary_out[1,c(15:20)])))
rownames(averages) <- c('Number of Terms', 'fREML', 'Effective Degrees of Freedom', 'Percentage Containing Parametric Year', 'Percentage Containing Year Spline' , 'Percentage Containing Climate Smooth',
                        'Percentage Containing Month Spline', 'Percentage Containing Tag Random Effect', 'Percentage Containing Antecedent Spline')



#honestly wouldn't be that shocked if some of this code broke somewhat if any of your models have tag splines, and hence, significant grouping. 
#None had it here and hence it wasn't tested. 100% expect it if you include factor smooths!
produce_graphs <- function(model_list, starting_k = 1, model_components, is_AR, rho_list = NULL){

  for (i in starting_k:length(model_list)){
    model <- model_list[[i]]
    component_absence <- !unlist(model_components[i,][-c(4,8)]) #even if linear year is here it won't be present in smooth_estimates
    component_absence <- c(component_absence[1:3], all(component_absence[4:6]))
    #the idea here is fundamentally gratia's (and hence, ofc, stolen from gavin simpson). By putting all the data into one dataframe, the scaling naturally comes out in a very natural fashion
    #I want my y axis (being the effect of the smooth) to be consistent across all the variables to accurately present their relative size. By having all components
    #use the same estimate column, the ggplot naturally sets the ylim to the same value for all graphs. The only change I'm implementing is adding in enough dummy
    #variables (again, using a similar technique) that this extends to variables that aren't in the model being presented!
    plot_df <- smooth_estimates(model, unconditional = T, overall_uncertainty = F)
    variables <- list('year', c('temperatures','vpds'), 'julian_month', 'antecedents')
    plot_df[unlist(variables[component_absence])] <- NA
    
    #change name of antecedents for constancy:
    if(model_components[i,5])names(plot_df)[which(names(plot_df)=='buds_lagged')] <- 'antecedents'
    if(model_components[i,6])names(plot_df)[which(names(plot_df)=='flowers_lagged')] <- 'antecedents'
    if(model_components[i,7])names(plot_df)[which(names(plot_df)=='green_fruits_lagged')] <- 'antecedents'
    
    
    
    #if linear year is a component of the model
    if(model_components[i,8]){
      p_effects <- parametric_effects(model)
      plot_df <- rbind(plot_df, data.frame(year = p_effects$value, est = p_effects$partial, se = p_effects$se, smooth = 'year', type = 'numeric', by = NA,
                                           temperatures = NA, vpds = NA, julian_month = NA,  antecedents = NA))
    }
     
    #extracting intercept. There's possibly an easier way to do it; this works.
    summary <- summary(model)
    plot_df$int_dummy <- NA
    plot_df <- rbind(plot_df,data.frame(smooth = 'Intercept', type = 'numeric', by = NA, est = summary$p.coeff[[1]], se = summary$se[[1]], int_dummy = 1,
                                        year = NA, temperatures = NA, vpds = NA, julian_month = NA,  antecedents= NA))
    
    #turn se + est into upper and lower ci_s
    plot_df <- add_confint(plot_df, coverage = 0.95)
    
    #need extract the original data points for overlayed residualish plotting
    tmp <- ifelse(is_AR,list(select(model$model,-'(AR.start)')), list(model$model))%>%.[[1]]
    
    #if component_absence is false, return false. If component_absence is true, and model_component is true
    #return false.  return true if component absense is true, and model components is false
    component_absence[1] <- component_absence[1]&!model_components[i,8]
    tmp[unlist(variables[component_absence])] <- NA
    tmp[c('smooth', 'type','by','est','se','int_dummy','lower_ci','upper_ci')] <- NA
    
    if(model_components[i,5]){names(tmp)[which(names(tmp)=='buds_lagged')] <- 'antecedents'
    }else if(model_components[i,6]){names(tmp)[which(names(tmp)=='flowers_lagged')] <- 'antecedents'
    }else if(model_components[i,7]){names(tmp)[which(names(tmp)=='green_fruits_lagged')] <- 'antecedents'
    }else(tmp$antecedents <-  NA)
  
    names(tmp)[1] <- 'subject'
    plot_df$subject <- NA
    
    plot_df <- rbind(plot_df,tmp)
    range <- range(plot_df$est, na.rm =T)
    
    #It appears that climate splines often have vastly greater error bars than the other splines, greatly 
    #inflating their plots when their scaling is relevant. Whilst the error bars of the tensor splines are
    #important, they are unable to be presented in these plots anyways. They will hpoefully be in an appendix
    plot_df_retain_climate <- plot_df
    plot_df <- filter(plot_df, is.na(temperatures) | !is.na(subject))
    range_big <- range(c(plot_df$upper_ci,plot_df$lower_ci),na.rm=T)
  
    #undoing year scaling performed before modelling. Involves a complicated inverse of the process by which species are turned to i values
    plot_df$year <- ifelse(ceiling(i/5)<42, list(filter(dat, Namecode==unique(dat$Namecode)[ceiling(i/5)])$year), list(dat$year))%>%
      unlist()%>%median()+plot_df$year
    
    #by implementing these dummy terms, ggplot understands a) that NA is not a data type for any of these variables and therefore there is nothing to plot
    # and b) the appropriate ranges for all the data. This enables very consistent plotting over the right ranges even for models that don't contain e.g. vpd
    plot_df <- rbind(plot_df,
                     data.frame(smooth = NA, type = NA, by = NA, est = NA, se = NA, year = c(1998,2021), temperatures = c(21.802, 9.483), vpds = c(1.212, 12.959), 
                                julian_month = c(1,12), antecedents = c(0,5), int_dummy = NA, subject = NA, upper_ci = NA, lower_ci = NA))
    
    
    
    #having NA values at the data points puts breaks into the geom_lines. ggplot can't recognise that there's a second
    #non NA value and interpolate
    plot_df_no_response <- filter(plot_df, is.na(subject))

    y_jit <- (range[2]-range[1])/50 #jitter of y variables, scaled for y scale
    
    #plot of the intercept with uncertainties
    p_0 <- ggplot(plot_df, aes(x = int_dummy))+
      geom_point(aes(y = est), na.rm = T)+
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), na.rm = T)+theme_bw()+
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
      labs(x = 'Model Intercept', y = 'Log Odds Contribution' )+
      geom_point(aes(x = jitter(rep(1, nrow(plot_df)), amount = 0.5),y = jitter(subject* (range_big[2]-range_big[1]) +range_big[1],amount = y_jit)),cex = 1, alpha = 0.3, na.rm = T)
    #plot of year, either spline or parametric. geom_ribbon doesn't like empty data, so adding it needs to check there's something to add
    
    p_1 <- ggplot(plot_df, aes(x = year))+
      geom_line(data = plot_df_no_response, aes(y = est), na.rm = T)+
      theme_bw()+labs(x = 'Year', y = 'Log Odds Contribution')
    if(model_components[i,1]|model_components[i,8]){
      p_1 <- p_1+geom_ribbon(aes(y = est, ymin = lower_ci, ymax = upper_ci),fill ='Red', alpha = 0.3,na.rm = T)+guides(alpha = 'none', fill = 'none')+
        geom_point(aes(x = jitter(year, amount = 0.3),y = jitter(subject* (range_big[2]-range_big[1]) +range_big[1],amount = y_jit)),cex = 1, alpha = 0.3, na.rm = T)
    }
    
    
    #plot of climate spline
    climate_range <- filter(plot_df_retain_climate, !is.na(temperatures))$est%>%range(na.rm = T)
    p_2 <- ggplot(plot_df_retain_climate, aes(x = temperatures, y = vpds))+
      expand_limits(data = plot_df, x= temperatures, y = vpds)+
      labs(x = 'Temperature (°C)', y = 'Vapour Pressure Deficit (hPa)', colour = 'Status', fill = 'Log Odds Contribution')+
      theme_bw()
    if(climate_range[1]!=Inf){
      p_2 <- p_2 +
              geom_contour_filled(na.rm = T, aes(z = est, alpha = 0.4), breaks = pretty(range, n = 15))+
              geom_point(aes(colour = as.logical(subject)),na.rm = T,cex = 0.5, pch = 19)+
              scale_fill_viridis_d(option = 'E',begin =(climate_range[1]-range[1])/(range[2]-range[1]), end =(climate_range[2]-range[1])/(range[2]-range[1]))+
              scale_colour_manual(na.value = rgb(0,0,0,0), values = c('TRUE' = '#00ff00', 'FALSE' = '#ff2200'), labels = c('Producing', 'Not Producing'))+
              guides(alpha = 'none', fill =guide_legend(ncol = 3, keywidth = 0.7))
    }
    
    #plot of julian month
    p_3 <- ggplot(plot_df, aes(x = julian_month))
    if(model_components[i,3]){
      p_3 <- p_3+geom_ribbon(aes(y = est, ymin = lower_ci, ymax = upper_ci),fill ='Blue', alpha = 0.3,na.rm = T)+guides(alpha = 'none', fill = 'none')+
        geom_point(aes(x = jitter(julian_month, amount = 0.3),y = jitter(subject* (range_big[2]-range_big[1]) +range_big[1],amount = y_jit)),cex = 1, alpha = 0.3, na.rm = T)
    }
    p_3 <- p_3+
      geom_line(data = plot_df_no_response, aes(y = est), na.rm = T)+
      theme_bw()+ labs(x = 'Julian Month', y = 'Log Odds Contribution')
     
    #just accessing some info about where we are
    species <- ifelse(i<205, unique(dat$Namecode)[ceiling(i/5)], 'Global')
    subject <- c('1'= 'Buds', '2' = 'Flowers', '3' = 'Green Fruits', '4' = 'Ripe Fruits', '5' = 'Fruits')
    antecedents <- c('1' = 'NA', '2' = 'Buds', '3' = 'Flowers', '4' = 'Green Fruits', '5' = 'Flowers')
    
    #plot of antecedents
    p_4 <- ggplot(plot_df, aes(x = antecedents))
    if(any(model_components[i,4:6])){
      p_4 <- p_4+geom_ribbon(aes(y = est, ymin = lower_ci, ymax = upper_ci),fill ='Green', alpha = 0.3,na.rm = T)+guides(alpha = 'none', fill = 'none')
    } 
    p_4 <- p_4+geom_line(data = plot_df_no_response,aes(y = est), na.rm = T, size = 1)+
      geom_point(aes(x = jitter(antecedents, amount = 0.3),y = jitter(subject* (range_big[2]-range_big[1]) +range_big[1],amount = 0.2)),cex = 1, alpha = 0.3, na.rm = T)+
      theme_bw()+ labs(x = paste0('Lagged ', antecedents[[as.character((i-1)%%5+1)]], ' Recording'), y = 'Log Odds Contribution')
      
    AR_status <- ifelse(is_AR, paste0(' AR Model with rho = ', rho_list[[i]]),' Non-AR Model')
    title <-   paste0(species,', ', subject[[(i-1)%%5+1]],AR_status)
    
    p_final <- p_0+p_1+p_2+p_3+p_4+plot_layout(design  = "
                                  #BBCC
                                  ABBCC
                                  ADDEE
                                  #DDEE")+plot_annotation(title = title)
    
    ggsave(paste0('outputs/graphs/', species,'_',subject[[(i-1)%%5+1]],', ',
                  ifelse(is_AR, 'AR', 'non-AR'),'.jpeg'),plot = p_final, device = 'jpeg',
           width = 3600, height = 1800, units = 'px')
    }
}
#just a little wrapper so that report.Rmd doesn't have to generate the plots. It's not super slow, but it's
#about 5 minutes
if (do_plots){
  produce_graphs(sapply(best_models, function(x) x$model), model_components = model_components, is_AR = TRUE, rho_list = sapply(best_models, function(x) x$rho))
  produce_graphs(best_non_AR_models, model_components = non_AR_model_components, is_AR = FALSE)
}
saveRDS(outs_full, file = 'outputs/aggregated_results.RDS')
saveRDS(summary_out, file = 'outputs/results_summary.RDS')




