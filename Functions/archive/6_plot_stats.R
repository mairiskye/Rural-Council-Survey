source("functions/stats_function_dev.R")
source("functions/5_wrap_labels.R")
#plot_stats() generates a horizontal bar plot of summary stats from generate_stats()
#params:  df = survey dataframe
#         query = search string ("Q10", )
#         width = width to wrap axis labels by
#         titles = reference table of questions and question numbers to generate table title NOT ALTERABLE
plot_stats <- function(df, query, width, titles) {
  stats <- generate_stats(df, query) # custom function
  x_labels <- wrap_labels(stats$Question, width) # custom function
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
