
tjbal_unbalance = function(data, outcome, baseline_year, 
                           X,
                           estimator_type, vce){
  
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
            demean = F, estimator = estimator_type, 
            vce = vce)
  
  return(m)
}
