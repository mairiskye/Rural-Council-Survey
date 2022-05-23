extract_stats <- function(df, str) {
  df_slice <- df[, grepl(str , names(df))] #extract slice of master data which contain query string
  mtx <- matrix(ncol = 2, nrow = ncol(df_slice)) #create empty matrix
  last_col_index <- ncol(df_slice)
  row <- 1 #initialise row pointer variable for populating matrix
  current_col <- 1  #initialise pointer for columns of the df slice
  #LOOP: for each question in the slice of the masterdata, calculate the proportion who answered yes
  #and populate matrix with the question text and the proportion
  while (current_col <= last_col_index) {
    mtx[row, 2] <- yes_proportions(df_slice, current_col)
    mtx[row, 1] <- names(df_slice[current_col])
    row <- row + 1
    current_col <- current_col + 1
  }
  stats <- as.data.frame(mtx)
  names(stats) <- c("Question", "% who answered 'Yes'")
  stats$`% who answered 'Yes'` <- as.numeric(paste(stats$`% who answered 'Yes'`)) #convert second column from factor
  stats$Question <- as.character(paste(stats$Question)) #convert first column factor
  final_stats <- trim_questions(stats)
  return(final_stats)
}

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

trim_questions <- function(table) {
  table$Question <- gsub(pattern = "Q\\d+\\.\\d+\\.\\s","",table$Question)
  table$Question <- gsub ("\\(e.g..*","", table$Question)
  return(table)
}

trim_questions_old <- function(table) {
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
