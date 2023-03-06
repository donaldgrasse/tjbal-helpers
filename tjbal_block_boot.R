tjbal_block_boot = function(data, unit, time, 
                            outcome, treated, nboot){

  lapply(c('dplyr', 'purrr', 'tjbal'), require, character.only = T)
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
data = data %>% 
    select(unit, time, treated, outcome) 
colnames(data)[1:4] = c('unit', 'time', 'treated', 'outcome')

baseline_year = data %>% 
  group_by(time) %>% 
  summarise(na_ever = mean(outcome, na.rm = T), 
            is_na_year = as.numeric(is.na(na_ever) == FALSE, 1,0)) %>% 
  filter(is_na_year == 1) %>% 
  summarise(min_year = min(time))

baseline_year = as.numeric(baseline_year)

tjbal_boot = function(data, unit, time, 
                     outcome, treated, baseline_year){
  
  get_block_index <- function(n_block, # Number of block
                              size # Number of observation in each block
  ) {
    
    block1 <- 1:size
    
    step <- 0:(n_block-1)*size
    step_boot <- sample(step, n_block, replace = TRUE)
    
    step_expand <- rep(step_boot, each = size)
    block_expand <- rep(block1, n_block)
    
    block_expand + step_expand
    
  }
  
  force_balance = function(data, outcome, baseline_year){
    
    data <- data %>% 
      group_by(unit) %>% 
      filter(time >= baseline_year) %>% 
      mutate(na_ever = mean(outcome)) %>% 
      filter(is.na(na_ever) == F) %>% 
      select(-c(na_ever)) %>% 
      as.data.frame()
    
    return(data)
  }
  
  data = force_balance(data = data, outcome = outcome, 
                       baseline_year = baseline_year)
  
  n_block = length(unique(data$unit)) 
  size = length(unique(data$time))
  
  block_index = get_block_index(n_block = n_block, size)
  block_data = data[block_index, ]$unit %>% unique()
  block_data = as.data.frame(cbind(block_data,1))
  colnames(block_data)[1:2] <- c('unit', 'sample')
  df2 <- merge(data, block_data, by = 'unit', all.x = T)
  
  df2 <- df2 %>% 
    filter(sample == 1)
  
  m = quiet(tjbal(data = df2, Y = 'outcome', D = "treated", 
        index = c("unit", "time"), 
        demean = F, estimator = "mean", 
        vce = "none"))
  att_avg = m$att.avg
  return(att_avg )
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

result_list = map_progress(seq_len(nboot), ~
               tjbal_boot(data, 'unit', 'time', 'outcome', 'treat', baseline_year)
)

boot_se = as.vector(unlist(result_list))
boot_se = sd(boot_se)

return(boot_se)
}
