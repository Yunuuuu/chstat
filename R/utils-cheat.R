# call grid function
# Always regard `y` as coord parallelly with heatmap and `y` as coord vertically
# with heatmap; x, x0, x1;
cheat_grid <- function(fn, which, ...) {
    params <- list(...)
    if (which == "row") params <- flip_coord(params)
    do.call(fn, params)
}

cheat_viewport <- function(which, xscale, yscale) {
    cheat_grid(viewport, which = which, xscale = xscale, yscale = yscale)
}

cheat_x <- function(n, which) {
    out <- seq_len(n)
    if (which == "row") out <- rev(out)
    out
}

#' @import grid
cheat_axis <- function(which, k, total, axis_param, data_scale) {
    if (axis_param$side %in% c("left", "top")) {
        if (k > 1L) {
            return()
        }
    } else if (k < total) {
        return()
    }
    grid.draw(ComplexHeatmap:::construct_axis_grob(
        axis_param,
        which = which, data_scale = data_scale
    ))
}

cheat_which <- function(which = NULL) {
    out <- cheat_env_get("current_annotation_which")
    if (is.null(out)) {
        out <- match.arg(which, c("column", "row"))
    }
    out
}

anno_width_and_height <- function(which, width = NULL, height = NULL, default = unit(10, "mm")) {
    # height must be absolute
    params <- list(width = width, height = height)
    if (which == "row") {
        params <- flip_coord(params)
        arg <- "width" # nolint
    } else {
        arg <- "height"
    }
    if (is.null(params$height)) {
        params$height <- default
    } else if (!ComplexHeatmap::is_abs_unit(params$height)) {
        cli::cli_abort(
            "{.arg height} of the annotation can only be an absolute unit."
        )
    }
    if (is.null(params$width)) {
        params$width <- unit(1L, "npc")
    }
    params
}

cheat_env_get <- function(name) {
    cheat_env()[[name]]
}

cheat_env <- function() {
    ComplexHeatmap:::.ENV
}
