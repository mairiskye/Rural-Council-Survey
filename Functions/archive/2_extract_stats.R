#import custom function from file: yes_proportions()
#parameters are dataframe and column index to retrieve stats for
source("functions/1_yes_propo.R")


#extract_stats() extracts a df of 'yes' proportions for a subset of questions
#params:  df = suvery dataframe
#         col1 = column index of first question in subset (e.g. int where Q10.1 is found)
#         col2 = column index of last question to be included in subset (e.g. int where Q10.9 is found)
extract_stats <- function(df, col1, col2) {
  mtx <- matrix(ncol = 2, nrow = (col2 - col1 + 1)) #create empty matrix
  p <- col1
  i <- 1
  #for each column in subset of the dta, calculate the proportion who answered yes
  #an populate matrix with result and corresponding question
  while (p <= col2) {
    mtx[i, 2] <- yes_proportions(df, p)
    mtx[i, 1] <- names(df[p])
    i <- i + 1
    p <- p + 1
  }
  res <- as.data.frame(mtx)
  return(res)
}
