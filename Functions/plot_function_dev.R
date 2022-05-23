source("functions/stats_function_dev.R")

plot_stats <- function(df, query, width, titles) {
  stats <- extract_stats(df, query) # custom function
  x_labels <- wrap_it(stats$Question, width) # custom function
  plot <- ggplot(stats, aes(x = reorder(x_labels, `% who answered 'Yes'`),  y = `% who answered 'Yes'`)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    xlab("")
  label <- dplyr::filter(titles, titles$question_numbers == query) %>%
    pull(root_questions)
  title_and_theme <- plot + theme_bw() + ggtitle(label)
  return(title_and_theme)
}

wrap_it <- function(strings, width) {
  sapply(strings,function(y) paste(strwrap(y, width),
                           collapse = "\n"),
         USE.NAMES = FALSE)
}
