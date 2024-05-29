plot_gantt <- function(df, today_line_length = 1) {

  df$duration <- df$end_date - df$start_date

  df$project_link <- glue::glue(
    "<a href='{df$link}'>{df$project_name}     </a>"
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
        line = list(color = df$color[i], width = 15),
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
        showgrid = FALSE,
        tickfont = list(color = "#333638"),
        tickmode = "array",
        tickvals = 1:nrow(df),
        ticktext = df$project_link,
        domain = c(0, 0.9),
        automargin = TRUE,
        align = "left"
      ),
      plot_bgcolor = "#f3eeec",
      paper_bgcolor = "#f3eeec",
      shapes = list(
        today_line(today_line_length = today_line_length)
      ),
      annotations = list(
        text = unique(df$category),
        x = -0.1,
        y = 6,
        xref = "paper",
        showarrow = FALSE,
        font = list(
          color = "#333638",
          size = 16
        )
      ),
      autosize = TRUE
    )
  }

  fig
}

today_line <- function(color = "red", today_line_length = 1) {
  list(
    type = "line",
    y0 = 0,
    y1 = today_line_length,
    yref = "paper",
    x0 = as.Date(Sys.Date()),
    x1 = as.Date(Sys.Date()),
    line = list(color = color)
  )
}
