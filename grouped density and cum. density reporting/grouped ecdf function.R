# Categorically applied cumulative distribution

group_ecdf <- function(df,SCORE, ..., lower_limit = NULL, upper_limit = NULL, step_size = NULL){
  
  library(tidyverse)
  
  score = enquo(SCORE)
  group_columns <-enquos(...)
  
  if(is.null(upper_limit)){
    upper_limit = max(df[[quo_name(score)]])
  } 
  
  if(is.null(lower_limit)){
    lower_limit = min(df[[quo_name(score)]])
  }
  
  if(is.null(step_size)){
    step_size = (upper_limit - lower_limit)/1000
  }
  
  # Making use of lapply property of group_by summarize pattern
  df_dist <- df %>% ungroup() %>% # in case the data frame fed has existing grouping implemented
    select(!!!group_columns, SCORE = !!score) %>% # use the argumement as a symbol in dplyr NSE
    group_by(!!!group_columns) %>%
    # Using list inside summarize within a group_by structure introduces lapply similar operation
    # COUNT_TABLES will be a list column that contains dataframe as elements.
    summarize(TOTAL_OBSERVATIONS = n(),
              COUNT_TABLES = list(
                # After converting table object to dataframe, your score (probability) column will become a factor.
                # It is important to convert to character first, so that it can be converted to numerical decimals.
                # If using as.numeric directly on score, it will be wrongly coerced to factor integer 1,2,3...
                as.data.frame(table(SCORE)) %>%
                  mutate(SCORE = as.numeric(as.character(SCORE)),
                         SCORE_GROUP = replace_na(as.character(cut(SCORE, seq(lower_limit, upper_limit, step_size))),as.character(lower_limit))) %>%
                  group_by(SCORE_GROUP) %>%
                  summarize(FREQ = sum(Freq))
                
              ),
              ecdfFUN = list(ecdf(SCORE))
    )
  
  df_dist$DISTRIBUTION_TABLES <- apply(df_dist, MARGIN = 1, function(i){
    df = data.frame(SCORE = seq(lower_limit,upper_limit,step_size)) %>%
      mutate(SCORE_GROUP = replace_na(as.character(cut(SCORE, breaks = SCORE)), as.character(lower_limit))) %>%
      left_join(i$COUNT_TABLES %>% select(SCORE_GROUP, FREQ), by = c("SCORE_GROUP" = "SCORE_GROUP")) %>%
      mutate(FREQ = replace_na(FREQ, 0),
             DENSITY = FREQ/sum(FREQ),
             CUMULATIVE_FREQ = cumsum(FREQ),
             CUMULATIVE_DENSITY = CUMULATIVE_FREQ/sum(FREQ),
             # Serve as a check by R ecdf caculation,
             #Should be equal to the one above
             ECDF_CUMULATIVE_DENSITY = i$ecdfFUN(SCORE))
    return(df)
  })
  
  df_dist <- df_dist %>% 
    select(-COUNT_TABLES, -ecdfFUN) %>%
    unnest(c(DISTRIBUTION_TABLES)) %>%
    ungroup() # Ungrouped it so that when people use the final output, no unexpected behavior due to grouped_df class
  
  return(df_dist)
  
}
