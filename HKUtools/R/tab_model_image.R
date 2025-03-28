#' Convert a tab_model to an image
#'
#' @param ... Passed to `sjPlot::tab_model`
#' @param file Character string. The file name of the output image. The file extension will be added automatically.
#' @param zoom Numeric. The zoom factor for the image. Default is 1.
#'
#' @return A table image saved as a PNG file.
#' @export
tab_model_image <- function(..., file = "./image-table/tab", zoom = 1) {
  sjPlot::tab_model(..., file = paste0(file, ".html")) |>
    print()
  webshot::webshot(url = paste0(file, ".html"),
                   file = paste0(file, ".png"),
                   selector = "table", zoom = zoom)
}
