
ri_pval = function(data, outcome, nsims, estimator, beep, X, baseline_year){
  
  if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
  if("tjbal" %in% rownames(installed.packages()) == FALSE) {install.packages("tjbal")}
  if("purrr" %in% rownames(installed.packages()) == FALSE) {install.packages("purrr")}
  if("beepr" %in% rownames(installed.packages()) == FALSE) {install.packages("beepr")}
  
  set.seed(303073)
  
  print("Inferring...")
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  data = data 
  data$outcome = outcome 
  
  
  tjbal_unbalance = function(data, outcome, baseline_year, 
                             X,
                             estimator){
    
    force_balance = function(data, outcome, baseline_year){
      
      data = data 
      data$outcome = outcome 
      
      data <- data %>% 
        group_by(DISTID) %>% 
        filter(year >= baseline_year) %>% 
        mutate(na_ever = mean(outcome)) %>% 
        filter(is.na(na_ever) == F) %>% 
        as.data.frame()
      
      return(data)
    }
    
    data = force_balance(data, outcome, baseline_year)
    
    m = tjbal(data, Y = 'outcome', 
              D = "treat", 
              index = c("DISTID", "year"), 
              X = X,
              demean = F, estimator = estimator, 
              vce = 'none')
    
    return(m)
  }
  
  placebo_treatment <- function(data, outcome, 
                                baseline_year, 
                                X,
                                estimator){
    
    pgks <- c('dplyr','tjbal', 'purrr')
    lapply(pgks, require, character.only = T)
    
    df <- data 
    outcome <- outcome 
    
    i_g_table <- df %>% 
      filter(year == min(year)) %>% 
      dplyr::select('DISTID', 'cohort')
    
    n = NROW(i_g_table)
    randIndex <-
      sample.int(n = n,
                 size = n,
                 replace = F)
    
    i_g_table$g <- i_g_table$cohort[randIndex]
    i_g_table = i_g_table %>% 
      dplyr::select(DISTID, g)
    
    df <- merge(df, i_g_table, by = 'DISTID', all.x = T)
    
    df = df %>% 
      filter(treated_ever ==0)
    
    df$treat <- ifelse(df$g > 0, as.numeric(df$year>=df$g, 1,0), 0)
    
    m = quiet(tjbal_unbalance(data = df, 
                              outcome = df$outcome, baseline_year = baseline_year, 
                              X = X,
                              estimator = estimator))
    att_avg = m$att.avg
    return(att_avg)
    
  }
  
  placebo_treatment(data = data, outcome= data$outcome, baseline_year=baseline_year, X=X, 
                    estimator = estimator)
  
  map_progress <- function(.x, .f, ..., .id = NULL) {
    .f <- purrr::as_mapper(.f, ...)
    pb <- progress::progress_bar$new(total = length(.x), format = " [:bar] :current/:total (:percent) eta: :eta", force = TRUE)
    
    f <- function(...) {
      pb$tick()
      .f(...)
    }
    purrr::map(.x, f, ..., .id = .id)
  }
  
  result_list <- unlist(map_progress(seq_len(nsims), 
                                     ~placebo_treatment(data = data, 
                                                        outcome= data$outcome, 
                                                        baseline_year=baseline_year, X=X, 
                                                        estimator = estimator), .progress = TRUE ))
  
  if(beep == TRUE){
    beepr::beep(sound = 2)
    return(result_list)
  }
  else{return(result_list)}
  
}

make_ri_hist <- function(result_list, est_effect, title){ 
  
  pgks <- c('ggplot2', 'ggthemes')
  lapply(pgks, require, character.only = T)
  
  rate_label <- sum(as.numeric(abs(result_list) >= abs(est_effect), 1,0))/(length(result_list))
  
  result_list <- as.data.frame(result_list)
  colnames(result_list)[1] <- 'values'
  
  share_char <- as.character(round(rate_label, 6))
  
  ri_hist <- ggplot(result_list, aes(values)) + 
    geom_histogram() + 
    geom_vline(xintercept = est_effect, lty = 2, col = 'red') + 
    xlab('Estimated ATT') + 
    ylab('Number of Estimates') + 
    ggtitle(title) + 
    annotate("label", x = -Inf, y = Inf, label = paste('Share Rejected:', share_char), 
             hjust=0, vjust=1, family = "Times") +       
    theme_tufte()
  
  return(ri_hist)
  
}


ri_event_plot = function(data, outcome, nsims, estimator, beep, X, baseline_year){
  
  if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
  if("tjbal" %in% rownames(installed.packages()) == FALSE) {install.packages("tjbal")}
  if("purrr" %in% rownames(installed.packages()) == FALSE) {install.packages("purrr")}
  if("beepr" %in% rownames(installed.packages()) == FALSE) {install.packages("beepr")}
  
  set.seed(303073)
  
  print("Inferring...")
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  data = data 
  data$outcome = outcome 
  
  
  tjbal_unbalance = function(data, outcome, baseline_year, 
                             X,
                             estimator){
    
    force_balance = function(data, outcome, baseline_year){
      
      data = data 
      data$outcome = outcome 
      
      data <- data %>% 
        group_by(DISTID) %>% 
        filter(year >= baseline_year) %>% 
        mutate(na_ever = mean(outcome)) %>% 
        filter(is.na(na_ever) == F) %>% 
        as.data.frame()
      
      return(data)
    }
    
    data = force_balance(data, outcome, baseline_year)
    
    m = tjbal(data, Y = 'outcome', 
              D = "treat", 
              index = c("DISTID", "year"), 
              X = X,
              demean = F, estimator = estimator, 
              vce = 'none')
    
    return(m)
  }
  
  placebo_treatment <- function(data, outcome, 
                                baseline_year, 
                                X,
                                estimator){
    
    pgks <- c('dplyr','tjbal', 'purrr')
    lapply(pgks, require, character.only = T)
    
    df <- data 
    outcome <- outcome 
    
    i_g_table <- df %>% 
      filter(year == min(year)) %>% 
      dplyr::select('DISTID', 'cohort')
    
    n = NROW(i_g_table)
    randIndex <-
      sample.int(n = n,
                 size = n,
                 replace = F)
    
    i_g_table$g <- i_g_table$cohort[randIndex]
    i_g_table = i_g_table %>% 
      dplyr::select(DISTID, g)
    
    df <- merge(df, i_g_table, by = 'DISTID', all.x = T)
    
    df$treat <- ifelse(df$g > 0, as.numeric(df$year>=df$g, 1,0), 0)
    
    
    m = tjbal(data, Y = 'outcome', 
              D = "treat", 
              index = c("DISTID", "year"), 
              X = X,
              demean = F, estimator = estimator, 
              vce = 'none')
    m = quiet(tjbal_unbalance(data = df, 
                              outcome = df$outcome, baseline_year = baseline_year, 
                              X = X,
                              estimator = estimator))
    att_ts = m$att
    return(att_ts)
    
  }
  
  map_progress <- function(.x, .f, ..., .id = NULL) {
    .f <- purrr::as_mapper(.f, ...)
    pb <- progress::progress_bar$new(total = length(.x), format = " [:bar] :current/:total (:percent) eta: :eta", force = TRUE)
    
    f <- function(...) {
      pb$tick()
      .f(...)
    }
    purrr::map(.x, f, ..., .id = .id)
  }
  
  result_list <- unlist(map_progress(seq_len(nsims), 
                                     ~placebo_treatment(data = data, 
                                                        outcome= data$outcome, 
                                                        baseline_year=baseline_year, X=X, 
                                                        estimator = estimator), 
                                     progress = TRUE ))
  
  sim_id = 1:nsims
  event_time_min = baseline_year-(max(as.numeric(data$cohort)[data$treat==1])-1)
  event_time_max = max(data$year)-(min(as.numeric(data$cohort)[data$treat==1])-1)
  event_times = seq(event_time_min, event_time_max , 1)
  
  result_data_frame = merge(sim_id, event_times)
  result_data_frame = cbind.data.frame(result_data_frame, result_list)
  
  if(beep == TRUE){
    beepr::beep(sound = 2)
    return(result_data_frame)
  }
  else{return(result_data_frame)}
  
}
