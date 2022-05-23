#Funciton to extract proportion who answered yes to a yes/no question, returns 0 if no 'yes' responses present

yes_proportions <- function(df, col) {
  yes_no_count <- count(df, get(colnames(df[col])))
  n_rep <- nrow(df)
  if ("Yes" %in% yes_no_count[[1]] == TRUE) {
    yes_no_count$n <- round(yes_no_count$n / n_rep * 100, 2)
    extract_yes <- dplyr::filter(yes_no_count, yes_no_count[[1]] == "Yes") %>% pull(n)
    return(extract_yes)
  }
  else {
    return(0)
  }
}
