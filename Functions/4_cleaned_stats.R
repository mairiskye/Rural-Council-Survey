source("functions/3_trim_questions.R")
source("functions/2_extract_stats.R")

#generate_stats() creates table of summary stats for a group of sub questions (e.g. Q10.1 - Q10.9)
#params:  df = survey dataframe
#         query = search string to select column names by (e.g. "Q10")
generate_stats <- function(df, query) {
  subdf <- df[, grepl(query , names(df))]
  last <- ncol(subdf)
  table <- extract_stats(subdf, 1 ,last) #custom function
  names(table) <- c("Question", "% who answered 'Yes'")
  table$`% who answered 'Yes'` <- as.numeric(paste(table$`% who answered 'Yes'`)) #convert second column from factor
  table$Question <- as.character(paste(table$Question)) #convert first column factor
  final_stats <- trim_questions(table) #custom function
  return(final_stats)
}
