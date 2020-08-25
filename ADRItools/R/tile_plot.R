#' Tile Plot over Multiple Columns
#'
#' @param d Data set. Must contain a column \code{col} to indicate which \code{facet} is placed in which column of the plot
#' @param x Column in \code{d} that is plotted on x axis
#' @param y Column in \code{d} that is plotted on y axis
#' @param fill Column in \code{d} that used for the fill shading
#' @param facets Column in \code{d} that is used to create facets
#' @param w Spacing of columns
#' @param y_label_free Allow the space of the y labels in each column to vary. Will impact on the tile plotting area, creating unequal widths. Set to \code{FALSE} by default.
#' @param ... Additional arguments passed to \code{facet_col()}
#'
#' @return A ggplot tile plot arranged into multiple columns
#' @export
#'
#' @examples
#' library(gapminder)
#' d <- gapminder %>%
#'   as_tibble() %>%
#'   mutate(col = as.numeric(continent),
#'          col = ifelse(test = continent == "Europe", yes = 2, no = col),
#'          col = ifelse(test = continent == "Oceania", yes = 3, no = col),
#'          # messing with label lengths to check code is working
#'          continent = fct_recode(continent, "North and South America" = "Americas"),
#'          country = as.character(country),
#'          country = ifelse(continent == "Africa", yes = abbreviate(country), country),
#'          country = fct_inorder(country),
#'          country = fct_rev(country))
#'
#' tile_plot(d = d, x = "year", y = "country", fill = "lifeExp", facets = "continent")
#' tile_plot(d = d, x = "year", y = "country", fill = "lifeExp", facets = "continent",
#'           labeller = label_wrap_gen(width = 15))
tile_plot <- function(d = NULL, x = NULL, y = NULL, fill = NULL, facets = NULL,
                      w = c(rep(0.94/max(d$col), max(d$col)), 0.06), y_label_free = FALSE,
                      ...){
  # x = "year"; y = "country"; fill = "lifeExp"; facets = "continent"
  # w = c(rep(0.94/max(d$col), max(d$col)), 0.06); y_label_free = FALSE
  if(!"col" %in% names(d))
    stop("must have a column in d with name col and numeric values to place each facet")
  g <- list()
  for(i in unique(d$col)){
    g[[i]] <- d %>%
      dplyr::filter(col == i) %>%
      ggplot2::ggplot(mapping = aes_string(x = x, y = y, fill = fill)) +
      ggplot2::geom_tile() +
      ggforce::facet_col(facets = facets, scales = "free_y", space = "free",# +
                         strip.position = "right", ...) +
      ggplot2::theme(strip.text.y = element_text(angle = 0)) +
      # aviod legends in every column
      ggplot2::guides(fill = FALSE) +
      ggplot2::labs(x = "", y = "")
  }
  g0 <- ggplot2::ggplot(data = d, mapping = aes_string(x = x, y = y, fill = fill)) +
    ggplot2::geom_tile()

  g1 <- g
  g1[[max(d$col) + 1]] <- cowplot::get_legend(g0)

  m <- d %>%
    dplyr::group_by(col) %>%
    dplyr::summarise(row = n_distinct(country)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(row = paste(1:row, collapse = ",")) %>%
    tidyr::separate_rows(row) %>%
    dplyr::mutate(row = as.numeric(row),
                  col = col,
                  p = col) %>%
    stats::xtabs(formula = p ~ row + col) %>%
    cbind(max(d$col) + 1) %>%
    ifelse(. == 0, NA, .)

  p <- gridExtra::grid.arrange(grobs = g1, layout_matrix = m, widths = w)

  g_width <- lapply(p$grobs[1:max(d$col)], function(gt) gt$widths) %>%
    do.call(grid::unit.pmax, .)

  s_grobs <- lapply(p$grobs, function(gt) gtable::gtable_filter(gt, pattern = "strip"))
  s_width <- lapply(s_grobs[1:max(d$col)], function(gt) gt$grobs[[1]]$widths) %>%
    do.call(grid::unit.pmax, .)

  for (i in 1:max(d$col)){
    if(y_label_free)
      p$grobs[[i]]$widths[-4] <- g_width[-4]
    if(!y_label_free)
      p$grobs[[i]]$widths <- g_width

    s <- stringr::str_which(string = p$grobs[[i]]$layout$name, pattern = "strip")
    for(j in s){
      p$grobs[[i]]$grobs[[j]]$widths <- s_width
    }
  }
  grid::grid.draw(p)
}


# d <- gapminder %>%
#   as_tibble() %>%
#   mutate(col = as.numeric(continent),
#          col = ifelse(test = continent == "Europe", yes = 2, no = col),
#          col = ifelse(test = continent == "Oceania", yes = 3, no = col),
#          # messing with label lengths to check code is working
#          continent = fct_recode(continent, "North and South America" = "Americas"),
#          country = as.character(country),
#          country = ifelse(continent == "Africa", yes = abbreviate(country), country),
#          country = fct_inorder(country))
#
#
# g <- list()
# for(i in unique(d$col)){
#   g[[i]] <- d %>%
#     filter(col == i) %>%
#     ggplot(mapping = aes(x = year, y = fct_rev(country), fill = lifeExp)) +
#     geom_tile() +
#     facet_col(facets = "continent", scales = "free_y", space = "free",# +
#               strip.position = "right") +
#     theme(strip.text.y = element_text(angle = 0)) +
#     # aviod legends in every column
#     guides(fill = FALSE) +
#     labs(x = "", y = "")
# }
#
#
# library(cowplot)
# gg <- ggplot(data = d, mapping = aes(x = year, y = country, fill = lifeExp)) +
#   geom_tile()
# leg <- get_legend(gg)
#
#
# m <-
#   d %>%
#   group_by(col) %>%
#   summarise(row = n_distinct(country)) %>%
#   rowwise() %>%
#   mutate(row = paste(1:row, collapse = ",")) %>%
#   separate_rows(row) %>%
#   mutate(row = as.numeric(row),
#          col = col,
#          p = col) %>%
#   xtabs(formula = p ~ row + col) %>%
#   cbind(max(d$col) + 1) %>%
#   ifelse(. == 0, NA, .)
#
# p <- grid.arrange(grobs = c(g), layout_matrix = m,
#                   widths = c(rep(0.94/5, 5)))
#                   # widths = c(0.32, 0.32, 0.32, 0.06))
# p
# grid::grid.draw(p)
#
# library(gtable)
# gtable_show_layout(p)
# gtable_show_layout(p$grobs[[1]])
# gtable_show_layout(p$grobs[[2]])
# grid::grid.draw(p$grobs[[1]])
# p$grobs[[2]]$heights
#
# grid::grid.draw(p$grobs[[1]]$grobs[[7]])
# p$grobs[[1]]$widths
# p$grobs[[2]]$widths
# p$grobs[[3]]$widths
#
#
# g_width <- grid::unit.pmax(p$grobs[[1]]$widths, p$grobs[[2]]$widths, p$grobs[[3]]$widths)
#
# # x axis label width all the same
# p$grobs[[1]]$widths[4] <- g_width[4]
# p$grobs[[2]]$widths[4] <- g_width[4]
# p$grobs[[3]]$widths[4] <- g_width[4]
#
# # p$grobs[[1]]$widths[5] <- unit(12, units = "null")
# # p$grobs[[2]]$widths[5] <- unit(12, units = "null")
# # p$grobs[[3]]$widths[5] <- unit(12, units = "null")
#
# # strip label width all the same
# p$grobs[[1]]$widths[6] <- g_width[6]
# p$grobs[[2]]$widths[6] <- g_width[6]
# p$grobs[[3]]$widths[6] <- g_width[6]
# grid::grid.draw(p)
#
#
# p$grobs[[1]]$grobs[[7]]$widths
# p$grobs[[2]]$grobs[[12]]$widths
# p$grobs[[2]]$grobs[[13]]$widths
# p$grobs[[3]]$grobs[[12]]$widths
# p$grobs[[3]]$grobs[[13]]$widths
#
# gtable_filter(p$grobs[[2]], pattern = "strip") %>%
#   gtable_width()
#
# # strip grey area width
# s_width <- grid::unit.pmax(
#   p$grobs[[1]]$grobs[[7]]$widths,
#   p$grobs[[2]]$grobs[[12]]$widths,
#   p$grobs[[2]]$grobs[[13]]$widths,
#   p$grobs[[3]]$grobs[[12]]$widths,
#   p$grobs[[3]]$grobs[[13]]$widths
# )
#
# p$grobs[[1]]$grobs[[7]]$widths <- s_width
# p$grobs[[2]]$grobs[[12]]$widths <- s_width
# p$grobs[[2]]$grobs[[13]]$widths <- s_width
# p$grobs[[3]]$grobs[[12]]$widths <- s_width
# p$grobs[[3]]$grobs[[13]]$widths <- s_width
# grid::grid.draw(p)
#
#
# # more general code to get to a function
# p <- grid.arrange(grobs = g, layout_matrix = m,
#                   widths = c(rep(0.94/3, 3)))
# # widths = c(0.32, 0.32, 0.32, 0.06))
# p
# grid::grid.draw(p)
#
# g_width <- lapply(p$grobs, function(gt) gt$widths) %>%
#   do.call(grid::unit.pmax, .)
#
# s_grobs <- lapply(p$grobs, function(gt) gtable_filter(gt, pattern = "strip"))
# s_width <- lapply(s_grobs, function(gt) gt$grobs[[1]]$widths) %>%
#   do.call(grid::unit.pmax, .)
#
# # s_grobs[[1]]$grobs[[1]]$widths
# # p$grobs[[1]]$grobs[[7]]$widths
# # p$grobs[[2]]$grobs[[7]]$widths
#
# y_label_free = FALSE
# # y_label_free = TRUE
# for (i in 1:length(p)){
#   # l <- str_which(string = p$grobs[[i]]$layout$name, pattern = "ylab-l")
#   if(y_label_free)
#     p$grobs[[i]]$widths[-4] <- g_width[-4]
#     # p$grobs[[i]]$widths[-l] <- g_width[-l]
#   if(!y_label_free)
#     p$grobs[[i]]$widths <- g_width
#
#   s <- str_which(string = p$grobs[[i]]$layout$name, pattern = "strip")
#   for(j in s){
#     p$grobs[[i]]$grobs[[j]]$widths <- s_width
#   }
# }
# grid::grid.draw(p)
