#' Graphical display of a textual table
#'
#' @param matrix A matrix. If it is a simple vector or data.frame, it will be
#' converted to a one-column matrix.
#' @inheritDotParams ComplexHeatmap::Heatmap -matrix -rect_gp -layer_fun -show_heatmap_legend
#' @param text_gp A [gpar] objects define the text attributes.
#' @param band A string, "row" or "column", add row or column interval color
#' band. If `NULL`, no band will be added.
#' @param rect_gp Graphic parameters for drawing rectangles (for heatmap body).
#' The value should be specified by [gpar] and `fill` parameter is ignored.
#' Using `type = "none"` to draw nothing on the heatmap body.
#' @inheritParams layer_text
#' @inheritParams ComplexHeatmap::Heatmap
#' @export 
cheat_table <- function(matrix, ..., format = NULL, text_gp = gpar(), band = NULL, band_col = c("white", "#eff3f2"), rect_gp = gpar(type = "none"), layer_fun = NULL, show_heatmap_legend = FALSE) {
    matrix <- check_matrix(matrix)
    if (!is.null(layer_fun)) {
        if (!is.function(layer_fun)) {
            cli::cli_abort("{.arg layer_fun} must be a function")
        }
        text_layer_fun <- layer_fun
    } else {
        text_layer_fun <- layer_text(matrix, format = format, gp = text_gp)
    }
    if (!is.null(band)) {
        band_laryer_fun <- layer_band(band, band_col = band_col)
        layer_fun <- function(j, i, x, y, width, height, fill) {
            band_laryer_fun(j, i, x, y, width, height, fill)
            text_layer_fun(j, i, x, y, width, height, fill)
        }
    } else {
        layer_fun <- text_layer_fun
    }
    ComplexHeatmap::Heatmap(
        matrix = matrix,
        rect_gp = rect_gp,
        ...,
        layer_fun = layer_fun,
        show_heatmap_legend = show_heatmap_legend
    )
}
