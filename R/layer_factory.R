#' Function factories for `layer_fun` in ComplexHeatmap
#' 
#' @details 
#' - `layer_band`: draw color interval band.
#' - `layer_text`: draw text.
#' @param band A string, "row" or "column", add row or column interval color
#' band.
#' @param band_col Color define the band interval. Should be of length 2L.
#' @return A layer_fun function for the usage of
#' [Heatmap][ComplexHeatmap::Heatmap]. 
#' @export 
#' @aliases layer_band
#' @name layer-fun
layer_band <- function(band, band_col = c("white", "#eff3f2")) {
    band <- match.arg(band, c("row", "column"))
    band_col <- rep_len(band_col, 2L)
    function(j, i, x, y, width, height, fill) {
        mat <- switch(band,
            row = cbind(height, y),
            column = cbind(width, x)
        )
        mat <- unique(mat)
        y <- mat[, 2L, drop = TRUE]
        col <- band_col[(order(y) %% 2L) + 1L]
        # browser()
        cheat_grid(grid.rect,
            which = switch(band,
                row = "column",
                column = "row"
            ),
            y = y,
            height = mat[, 1L, drop = TRUE],
            gp = gpar(col = col, fill = col),
            default.units = "native"
        )
        grid.rect(gp = gpar(lwd = 2, fill = "transparent"))
    }
}

#' @param matrix Text matrix used to draw.
#' @param format Format strings, see [sprintf].
#' @param gp A [gpar] objects define the text attributes.
#' @export 
#' @rdname layer-fun
layer_text <- function(matrix, format = NULL, gp = gpar()) {
    force(matrix)
    force(format)
    force(gp)
    function(j, i, x, y, width, height, fill) {
        labels <- pindex(matrix, i, j)
        if (!is.null(format)) {
            labels <- sprintf(format, labels)
        }
        # since grid.text can also be vectorized
        grid.text(labels, x, y, gp = gp)
    }
}
