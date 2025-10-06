#' Load configuration
#'
# -------------------------------------------------------------------------
#' Load a user defined configuration from file. By default
#' (i.e. when `as_is = FALSE`), `load_config()` requires inputs to be given as
#' uniquely-named lists. It first parses the configuration file looking for a
#' 'default' entry. With no additional arguments this will be returned as a list
#' to the user. If the user specifies an additional list to consider (via the
#' `config` argument) then this list is layered on top
#' (using `utils::modifyList()`).
#'
# -------------------------------------------------------------------------
#' Configuration files can be specified using a reduced subset of base R.
#' By defauly this is restricted to the following operators and functions:
#'
#' - <-, =, +, -, *, :,
#' - $, [, [[,
#' - $<-, [<-, [[<-,
#' - c,
#' - as.Date,
#' - array, matrix,
#' - list, data.frame,
#' - Sys.Date, Sys.time,
#' - seq, sequence and seq_len.
#' - file.path
#'
#' We also enable a convenience function, `cc`, which automatically quotes input
#' to save typing.
#'
#' Users can also inject their own functions in to the evaluation environment
#' by supplying a list of [crates][carrier::crate()] as an additional argument.
#'
# -------------------------------------------------------------------------
#' @param filename
#'
#' Configuration file to read from.
#'
#' @param config
#'
#' Name of entry in configuration file to layer on top of 'default'.
#'
#' Not used if `as_is = TRUE`.
#'
#' @param crates
#'
#' A list of [carrier::crate] objects which are used to inject functions in to
#' the environment where the configuration file will be evaluated.
#'
#' @param as_is
#'
#' Should the configuration file be read in as is, without layering on top of
#' the `default` config?
#'
#' Defaults to `FALSE`.
#'
#' @param default
#'
#' The default configuration to use.
#'
#' @param ...
#'
#' Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' If `as_is = FALSE` (default) a list contain entries corresponding to the
#' chosen `config`. If `as_is = TRUE`, a list of all entries in the evaluated
#' configuration file.
#'
# -------------------------------------------------------------------------
#' @examples
#' # load the example configuration
#' file <- system.file("config.R", package = "ronfig")
#' cat(readChar(file, file.info(file)$size))
#'
#' # default configuration
#' str(load_config(file))
#'
#' # debug configuration
#' str(load_config(file, "debug"))
#'
#' # forecast configuration
#' str(load_config(file, "forecast"))
#'
#' # Injecting crated function
#' f <- tempfile()
#' cat("default <- list(a=mean(1:10))", file = f)
#'
#' # will fail as mean() not available
#' tryCatch(with(load_config(f), a), error = conditionMessage)
#'
#' # will work if we inject crated mean
#' crate <- carrier::crate(function(x) mean(x))
#' with(load_config(f, crates = list(mean = crate)), a)
#'
#' unlink(f)
#'
# -------------------------------------------------------------------------
#' @importFrom cli cli_abort
#' @export
load_config <- function(
    filename,
    config,
    crates,
    ...,
    as_is = FALSE,
    default = "default"
) {
    # Check the filename is valid
    if (!is.character(filename) || length(filename) != 1L || is.na(filename))
        .abort("{.arg filename} must be a string.")

    # check as_is is bool
    if (!is.logical(as_is) || length(as_is) != 1L || is.na(as_is))
        .abort("{.arg as_is} must be a boolean.")

    # check the config is valid
    if (!missing(config)) {
        if (as_is)
            .abort("{.arg config} can only be given when {.arg as_is} is FALSE.")

        if (!is.character(config) || length(config) != 1L || is.na(config))
            .abort("{.arg config} must be a string.")
    }

    # check the crates
    if (!missing(crates)) {
        if (!is.list(crates))
            .abort("{.arg crates} must be a named list of {.fun carrier::crate}.")

        if (length(crates)) {
            crate_names <- names(crates)
            if (is.null(crate_names) || !all(nzchar(crate_names)) || anyDuplicated(crate_names)) {
                .abort("{.arg crates} must be a uniquely-named list of {.fun carrier::crate}.")
            }
            idx <- vapply(crates, carrier::is_crate, TRUE)
            if (!all(idx)) {
                invalid <- match(FALSE, idx)
                .abort(
                    c(
                        "{.arg crates} must be a uniquely-named list of {.fun carrier::crate}.",
                        x = "Entry {.field {crate_names[invalid]}} has class {.class {class(crates[[invalid]])}}."
                    )

                )
            }
        }
    }

    # check the default is valid
    if (!as_is && (!is.character(default) || length(default) != 1L || is.na(default)))
        .abort("{.arg default} must be a string.")

    # Check the file exist
    if (file.access(filename, mode = 0))
        .abort("File {.file {filename}} does not exist")

    chkDots(...)

    # helper function for quoting input (saves writing speech marks)
    cc <- function(...) as.character(substitute(list(...))[-1L])

    # Insert only a few essential functions plus cc in to an empty environment
    # to use as the parent environment to will eventually source things
    allow_list <- list(
        c('<-', '=', '+', '-', '*', ':'),
        c('as.Date'),
        c('array', 'matrix'),
        c('list', 'data.frame'),
        c('c', 'cc'),
        c('[', '[[', '$'),
        c('$<-', '[<-'),
        c('Sys.Date', 'Sys.time'),
        c('seq','sequence','seq_len'),
        'file.path'
    )
    allowed <- unlist(allow_list)

    parent <- list2env(mget(allowed, inherits = TRUE), parent = emptyenv())

    # Add in any crates supplied by the user
    if (!missing(crates)) {
        idx <- crate_names %in% allowed
        if (any(idx)) {
            nm <- crate_names[idx[1L]]
            .abort("{.val {nm}} cannot be used as a crated function name.")
        }

        for (i in seq_along(crate_names)) {
            assign(crate_names[[i]], crates[[i]], envir = parent)
        }
    }

    # Now create an environment with the given parent
    envir <- new.env(parent = parent)

    # Capture the caller environment for error messaging
    call <- sys.call()[1L]

    # source the input file in to our minimal environment with custom error
    # handling
    withCallingHandlers(
        sys.source(file = filename, envir = envir, chdir = TRUE, keep.source = FALSE),
        error = function(cond) {
            # Add additional information to the error message when functions are
            # not available in the environment. Errors from sys.source are not
            # classed so we condition on the content of the associated message.
            cnd_msg <- conditionMessage(cond)
            if (startsWith(cnd_msg, "could not find function ")) {
                allowed_strings <- vapply(allow_list, toString, "")
                .abort(
                    c(
                        "Unable to load configuration file: {cnd_msg}",
                        i = "Only the following functions are available to use in rconfig files:",
                        `names<-`(allowed_strings, rep("*", length(allowed_strings)))
                    ),
                    parent = NA,
                    error = cond,
                    call = call
                )
            }

            .abort("Unable to load configuration file", parent = cond, call = call)
        }
    )

    if (as_is) {
        # return the environment as a list
        return(as.list(envir, all.names = TRUE, sorted = FALSE))
    }

    # Check that the 'default' configuration exists in the environment
    if (is.null(out <- get0(default, envir, inherits = FALSE)))
        .abort("Cannot find {.arg default} entry ({.val {default}}) in the rconfig file.")

    # Check that default is a list
    if (!is.list(out) || is.data.frame(out))
        .abort("{.arg default} entry must be a named list.")

    # Check that default is a named list
    if (!.all_named(out))
        .abort("{.arg default} entry ({.val {default}}) must be a non-empty and uniquely-named list.")

    if (missing(config))
        return(out)

    # Check that the 'config' configuration exists in the environment
    if (is.null(conf <- get0(config, envir, inherits = FALSE)))
        .abort("Cannot find {.arg config} entry ({.val {config}}) in the rconfig file.")

    # Check that config is a list
    if (!is.list(conf) || is.data.frame(conf))
        .abort("{.arg config} entry ({.val {config}}) must be a named list.")

    # Check that config is a named list
    if (!.all_named(conf))
        .abort("{.arg config} entry ({.val {config}}) must be a non-empty and uniquely-named list.")

    utils::modifyList(out, conf, keep.null = TRUE)
}

.all_named <- function(x) {
    names <- names(x)
    if (is.null(names) || !all(nzchar(names)) || anyDuplicated(names))
        return(FALSE)

    for (n in names) {
        xx <- x[[n]]
        if (is.list(xx) && !.all_named(xx))
            return(FALSE)
    }
    TRUE
}

.abort <- function(message, ..., call = .envir, .envir = parent.frame(), .frame = .envir) {
    cli_abort(
        message,
        ...,
        call = call,
        .envir = .envir,
        .frame = .frame,
        class = "ronfig_error"
    )
}
