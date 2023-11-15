`%||%` <- function(a, b) {
    if (!is.null(a)) a else b
}


#' Rename elements in a list, data.frame or vector
#'
#' This is akin to `dplyr::rename` and `plyr::rename`. It renames elements given
#' as names in the `replace` vector to the values in the `replace` vector
#' without touching elements not referenced.
#'
#' @param x A data.frame or a named vector or list
#' @param replace A named character vector. The names identifies the elements in
#' `x` that should be renamed and the values gives the new names.
#'
#' @return `x`, with new names according to `replace`
#'
#' @keywords internal
#' @noRd
rename <- function(x, replace) {
    current_names <- names(x)
    old_names <- names(replace)
    missing_names <- setdiff(old_names, current_names)
    if (length(missing_names) > 0) {
        replace <- replace[!old_names %in% missing_names]
        old_names <- names(replace)
    }
    names(x)[match(old_names, current_names)] <- as.vector(replace)
    x
}

flip_coord <- function(aesthetics) {
    x <- c("x", "x0", "x1", "width", "xscale")
    y <- c("y", "y0", "y1", "height", "yscale")
    rename(aesthetics, structure(c(x, y), names = c(y, x)))
}

subset_gp <- function(gp, i) {
    params <- lapply(gp, `[[`, i)
    do.call(grid::gpar, params)
}

recycle_gp <- function(gp, n) {
    n <- max(n, 1L)
    for (i in seq_along(gp)) {
        gp[[i]] <- rep_len(gp[[i]], n)
    }
    gp
}

reverse_trans <- function(x) {
    sum(range(x, na.rm = TRUE)) - x
}

check_matrix <- function(x) {
    if (inherits(x, "data.frame")) {
        x <- as.matrix(x)
    } else if (!is.matrix(x)) {
        if (is.atomic(x)) {
            cli::cli_alert_info("convert simple vector {.arg matrix} to one-column matrix")
            x <- matrix(x, ncol = 1L)
        } else {
            cli::cli_abort("{.arg {arg}} must be a {.cls matrix}, a simple vector, or a {.cls data.frame}.")
        }
    }
    x
}

pindex <- function(array, ...) {
    if (length(dim(array)) != ...length()) {
        stop("Indexing must have as many as the number of dimentions of array")
    }
    dots <- list(...)
    # all index must be atomic
    is_right <- vapply(dots, function(x) {
        is.atomic(x) && !is.null(x)
    }, logical(1L))
    if (!all(is_right)) {
        stop("All elements in ... must be atomic (`NULL` is not allowed)")
    }
    dots_len <- lengths(dots)
    if (any(dots_len == 0L)) {
        stop("Empty index is not allowed")
    }
    common_len <- max(dots_len)
    if (any(dots_len > 1L & dots_len < common_len)) {
        stop("Only index of length one are recycled")
    }
    if (common_len != 1L) {
        dots[dots_len == 1L] <- lapply(dots[dots_len == 1L], function(x) {
            rep_len(x, common_len)
        })
    }
    array[do.call("cbind", dots)]
}
