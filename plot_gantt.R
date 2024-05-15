plot_gantt <- function(df) {

  df$duration <- df$end_date - df$start_date

  df$project_link <- glue::glue(
    "<a href='{df$link}'>{df$project_name}</a>   "
  )

  # Plotly throws a warning that spams the log when type is not specified
  # mode keeps the plot empty but also avoids another warning from plotly
  fig <- plot_ly(type = "scatter", mode = "none")

  if (nrow(df) > 0) {
    for (i in 1:(nrow(df))) {

      fig <- add_trace(
        fig,
        x = c(df$start_date[i], df$start_date[i] + df$duration[i]), # x0, x1
        y = c(i, i), # y0, y1
        mode = "lines",
        line = list(color = df$color[i], width = 20),
        showlegend = FALSE,
        hoverinfo = "text",
        text = paste(
          "Project: ",
          df$project_name[i],
          "<br>Duration: ",
          df$duration[i],
          "days<br>",
          "Category: ",
          df$category[i]
        ) # HTML means we can make things clickable
      )
    }

    fig <- layout(
      fig,
      xaxis = list(
        showgrid = FALSE,
        tickfont = list(color = "#333638")
      ),
      yaxis = list(
        showgrid = FALSE, tickfont = list(color = "#333638"),
        tickmode = "array", tickvals = 1:nrow(df),
        ticktext = paste(
          df$category,
          df$project_link,
          sep = ": "
        ),
        domain = c(0, 0.9),
        automargin = TRUE
      ),
      plot_bgcolor = "#faf9f6",
      paper_bgcolor = "#faf9f6",
      shapes = list(
        today_line()
      ),
      autosize = TRUE
    )
  }

  fig
}

today_line <- function(color = "red") {
  list(
    type = "line",
    y0 = 0,
    y1 = 0.9,
    yref = "paper",
    x0 = as.Date(Sys.Date()),
    x1 = as.Date(Sys.Date()),
    line = list(color = color)
  )
}
