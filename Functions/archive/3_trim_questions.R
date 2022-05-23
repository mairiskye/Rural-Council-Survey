
#trim_question() trims occurences of (e.g.....) and removes question numbers
#param: table = table of summary stats (generated with extract_stats() in parent call within cleaned_stats()
trim_questions <- function(table) {
  if ((identical(str_detect(table$Question, "e.g."), grepl("e.g.", table$Question))) == TRUE) {
  find_eg <- as.data.frame(str_locate(table$Question, "e.g."))
  eg_found_at <- filter(find_eg, find_eg$start > 0)
  start_trim_at <- eg_found_at$start - 2
  locations <- str_which(table$Question, coll("e.g.", ignore_case = F))
  table$Question[locations] <- str_sub(table$Question[locations], start = 1, end = start_trim_at)
  }
  table$Question <- gsub("^.*\\.", "", table$Question)
  return(table)
}
