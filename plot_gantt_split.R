library(plotly)

gantt_df <- data.frame(
  project_name = c(
    "Project D",
    "Project E",
    "Project F",
    "Project G",
    "Project H",
    "Project I",
    "Project J",
    "Project K",
    "Project L",
    "Project M"
  ),
  start_date = as.Date(
    c(
      "2024-01-01",
      "2024-02-15",
      "2024-03-10",
      "2024-05-01",
      "2024-06-15",
      "2024-07-10",
      "2024-08-05",
      "2024-09-20",
      "2024-10-01",
      "2024-11-10"
    )
  ),
  end_date = as.Date(
    c(
      "2024-03-15",
      "2024-04-25",
      "2024-05-10",
      "2024-06-15",
      "2024-07-25",
      "2024-08-20",
      "2024-09-10",
      "2024-10-25",
      "2024-11-10",
      "2024-12-15"
    )
  ),
  link = c(
    "http://projectd.com",
    "http://projecte.com",
    "http://projectf.com",
    "http://projectg.com",
    "http://projecth.com",
    "http://projecti.com",
    "http://projectj.com",
    "http://projectk.com",
    "http://projectl.com",
    "http://projectm.com"
  ),
  category = c(
    "Category X",
    "Category Y",
    "Category Z",
    "Category X",
    "Category Y",
    "Category Z",
    "Category X",
    "Category Y",
    "Category Z",
    "Category X"
  )
)

source("plot_gantt.R")

plot_gantt_split <- function(
  gantt_df
) {
  gantt_split <- split(gantt_df, gantt_df$category)

  sub_gantts <- lapply(seq_along(gantt_split), function(i) {
    df <- gantt_split[[i]]
    today_line_length <- if (i == 1) 1 else 1.5
    # To remove the gaps between subplots
    plot_gantt(
      df,
      today_line_length = today_line_length
    )
  })

  subplot(
    sub_gantts,
    nrows = length(sub_gantts),
    shareX = TRUE,
    margin = 0.05,
    titleY = TRUE
  )
}

plot_gantt_split(gantt_df)
