#' Intervals annotation: lines, crossbars & errorbars
#'
#' @param x A matrix with at leat 3 columns, regarded as the error bar point
#' estimate and confidence interval (lower and higher).
#' @param which Whether it is drawn as a column annotation or a row annotation?
#' @param width A [unit][grid::unit] object specifying the width of the tip
#' head.
#' @inheritParams grid::grid.points
#' @param point_gp A [gpar][grid::gpar] objects define the point attributes.
#' @param line_gp A [gpar][grid::gpar] objects define the line attributes.
#' @param band A bool, if `TRUE`, will add color band interval
#' @param band_col Color define the band interval. Should be of length 2L.
#' @export
#' @aliases anno_errorbar
#' @name anno-interval
anno_errorbar <- function(x, which = NULL, width = 0.9, pch = 19, size = 1, point_gp = gpar(), line_gp = gpar(), band = TRUE, band_col = c("white", "#eff3f2")) {
    x <- check_matrix(x)
    if (ncol(x) < 3L) {
        cli::cli_abort("{.arg x} must be matrix with 3 columns")
    }
    n <- nrow(x)
    which <- cheat_which(which)
    point_gp <- recycle_gp(point_gp, n)
    line_gp <- recycle_gp(line_gp, n)
    pch <- rep_len(pch, n)
    width <- unit(rep_len(width, n), "native")
    size <- unit(rep_len(size, n), "mm")
    limits <- range(c(x), na.rm = TRUE, finite = TRUE)
    chstat_anno_fn(
        fun = anno_errorbar_fn(
            x,
            which = which, pch, size, width,
            limits = limits,
            band, band_col, point_gp, line_gp
        ),
        n = n, subsettable = TRUE, height = unit(2L, "cm"),
        which = which, data_scale = limits
    )
}

anno_errorbar_fn <- function(x, which, pch, size, width, limits, band, band_col, point_gp, line_gp) {
    force(x)
    force(which)
    force(pch)
    force(size)
    force(width)
    force(limits)
    force(band)
    force(band_col)
    force(point_gp)
    force(line_gp)
    function(index, k, total) {
        n <- length(index)
        mat <- x[index, , drop = FALSE]
        na_row <- rowSums(is.na(mat)) > 0L
        cur_pch <- pch[index[na_row]]
        cur_size <- size[index[na_row]]
        cur_width <- width[index[na_row]]
        pushViewport(cheat_viewport(which,
            xscale = c(0.5, n + 0.5),
            yscale = limits
        ))
        coord_x <- cheat_x(n, which)
        if (isTRUE(band)) {
            add_band(n,
                which = which, x = coord_x,
                limits = limits, band_col = band_col
            )
        }
        grid.rect(gp = gpar(fill = 0L))
        cheat_grid(grid.points,
            which = which,
            gp = subset_gp(point_gp, index[na_row]),
            pch = cur_pch, size = cur_size,
            x = coord_x, y = mat[, 1L, drop = TRUE],
            default.units = "native"
        )
        cheat_grid(grid.segments,
            which = which,
            gp = subset_gp(line_gp, index[na_row]),
            arrow = arrow(angle = 90, ends = "both", length = cur_width),
            x0 = coord_x, x1 = coord_x,
            y0 = mat[, 2L, drop = TRUE],
            y1 = mat[, 3L, drop = TRUE],
            default.units = "native"
        )
        popViewport()
    }
}

#' @export
#' @rdname anno-interval
anno_linerange <- function(x, which = NULL, pch = 19, size = 1, point_gp = gpar(), line_gp = gpar(), band = TRUE, band_col = c("white", "#eff3f2")) {
    x <- check_matrix(x)
    if (ncol(x) < 3L) {
        cli::cli_abort("{.arg x} must be matrix with 3 columns")
    }
    n <- nrow(x)
    which <- cheat_which(which)
    point_gp <- recycle_gp(point_gp, n)
    line_gp <- recycle_gp(line_gp, n)
    pch <- rep_len(pch, n)
    size <- unit(rep_len(size, n), "mm")
    limits <- range(c(x), na.rm = TRUE, finite = TRUE)
    chstat_anno_fn(
        fun = anno_linerange_fn(
            x,
            which = which, pch, size, band,
            band_col, point_gp, line_gp
        ),
        n = n, subsettable = TRUE, height = unit(2L, "cm"),
        which = which, data_scale = limits
    )
}

anno_linerange_fn <- function(x, which, pch, size, limits, band, band_col, point_gp, line_gp) {
    force(x)
    force(which)
    force(pch)
    force(size)
    force(limits)
    force(band)
    force(band_col)
    force(point_gp)
    force(line_gp)
    function(index, k, total) {
        n <- length(index)
        mat <- x[index, , drop = FALSE]
        na_row <- rowSums(is.na(mat)) > 0L
        cur_pch <- pch[index[na_row]]
        cur_size <- size[index[na_row]]
        pushViewport(cheat_viewport(which,
            xscale = c(0.5, n + 0.5),
            yscale = limits
        ))
        coord_x <- cheat_x(n, which)
        if (isTRUE(band)) {
            add_band(n,
                which = which, x = coord_x,
                limits = limits, band_col = band_col
            )
        }
        grid.rect(gp = gpar(fill = 0L))
        cheat_grid(grid.points,
            which = which,
            gp = subset_gp(point_gp, index[na_row]),
            pch = cur_pch, size = cur_size,
            x = coord_x, y = mat[, 1L, drop = TRUE],
            default.units = "native"
        )
        cheat_grid(grid.segments,
            which = which,
            gp = subset_gp(line_gp, index[na_row]),
            x0 = coord_x, x1 = coord_x,
            y0 = mat[, 2L, drop = TRUE],
            y1 = mat[, 3L, drop = TRUE],
            default.units = "native"
        )
        popViewport()
    }
}

add_band <- function(n, which, x, limits, band_col = c("white", "#eff3f2")) {
    band_col <- rep_len(band_col, 2L)
    band_col <- band_col[(seq_len(n) %% 2L) + 1L]
    cheat_grid(grid.rect,
        which = which, x = x, y = mean(limits),
        width = 1L, height = unit(1, "npc"),
        gp = gpar(col = band_col, fill = band_col),
        default.units = "native"
    )
}
