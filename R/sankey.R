to_long <- function(x, cols = -1) {
  out <- tidyr::pivot_longer(x,
                             cols = {{ cols }},
                             names_to = "time",
                             values_to = "value")
  out$time <- as.integer(
    gsub("x", replacement = "", x = out$time, ignore.case = TRUE))
  names(out)[1] <- "gp"
  out
}

add_ribbon_columns <- function(x, gap) {
  x$ydown <- stats::ave(x$value, x$time, FUN = function(x) {
    c(0, cumsum(sort(x)) + gap)[rank(x, ties.method = "first")]
  })
  x$yup <- stats::ave(x$value, x$time, FUN = function(x) {
    cumsum(sort(x))[rank(x, ties.method = "first")]
  })
  x
}

#' @importFrom utils head tail
duplicate_lag <- function(x, timegap) {
  x2 <- x
  x2$time <- stats::ave(x2$time, x2[[1]], FUN = function(x) {
    c(tail(x, -1), tail(x, 1) + diff(tail(x, 2))) - timegap
  })
  x2
}

utils::globalVariables(c("gp", "time", "ypos", "value"))
get_labels <- function(x, x2) {
  out <- data.frame(
    gp = x[[1]],
    time = (x$time + x2$time) / 2,
    ypos = (x$ydown + x$yup) / 2,
    value = x$value
  )
  xlabels <- unique(x$time)
  xbreaks <- unique(out$time)
  ybreaks <- tapply(out$ypos, INDEX = out[[1]], FUN = function(x) x[1])
  list(label_df = out, xlabels = xlabels, xbreaks = xbreaks,
       ybreaks = ybreaks)
}

my_palette <- function(n) {
  cols <- c("#DA654F", "#D88F54", "#8ABA8F", "#4D6D94", "#7EACC5")
  if (n <= 5) {
    return(head(cols, n))
  } else {
    grDevices::colorRampPalette(cols)(n)
  }
}

utils::globalVariables(c("yup", "ydown", "value", "gp"))
#' Sankey Plot
#'
#' Create a Sankey plot using [ggplot2]
#'
#' This function creates a Sankey plot from a data frame with two columns
#' (x and y) and one column for the group (gp).
#'
#' @param dat A data frame in wide format with the first column being the
#'   different categories, and the remaining columns showing different time
#'   points.
#' @param title The title of the plot
#' @param timeslope The width of the slope between each time point (should be > 0)
#' @param hspace The thickness of the gap between the ribbons (the horizontal white spaces)
#' @param vspace Whether or not to include vertical white spaces
#' @param digits The number of decimal places to round the values to
#' @return A [`ggplot2::ggplot2`] object
#'
#' @importFrom ggplot2 element_blank element_text aes
#' @export
plot_sankey <- function(dat, title = NULL, timeslope = 1, hspace = .03,
                        vspace = TRUE, digits = 2) {
  # 1. Convert to long data, and add ribbon coordinates
  xlong <- add_ribbon_columns(
    to_long(dat),
    gap = hspace
  )
  # 2. A duplicated data set (for positioning the labels)
  x2 <- duplicate_lag(xlong, timegap = timeslope)
  # 3. Obtain labels
  labels <- get_labels(xlong, x2)
  # 4. Plot
  ggplot2::ggplot(rbind(xlong, x2), aes(x = time, fill = gp)) +
    ggplot2::geom_ribbon(aes(ymin = ydown, ymax = yup)) +
    ggplot2::geom_vline(aes(xintercept = time), color = "white",
                        alpha = as.integer(vspace)) +
    ggplot2::geom_label(data = labels$label_df,
                        aes(y = ypos,
                            label = formatC(value, digits = digits,
                                            format = "f")),
                        color = "white",
                        size = 3, label.size = NA) +
    ggplot2::ggtitle(paste0(title, "\n\n")) +
    ggplot2::scale_x_continuous(position = "top",
                                breaks = labels$xbreaks,
                                labels = labels$xlabels) +
    ggplot2::scale_y_continuous(position = "left",
                                breaks = labels$ybreaks,
                                labels = names(labels$ybreaks)) +
    ggplot2::scale_fill_manual(values = my_palette(length(labels$ybreaks))) +
    ggplot2::theme(plot.title = element_text(hjust = 0.4),
                   axis.ticks.x = element_blank(), # remove x axis ticks
                   axis.ticks.y = element_blank(),
                   axis.title = element_blank(), # remove axis titles
                   panel.background = element_blank(), # remove background
                   legend.position = "none"
    )
}
