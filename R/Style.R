
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' CSS style builder
#'
#' @docType class
#'
#' @usage NULL
#' @section Usage:
#' \preformatted{style <- Style$new()
#' style <- css_style()
#'
#' style$set_selector(".thing")
#' style$remove("color")
#' style$update(color = 'blue')
#' style$as_character()
#' style$as_inline()
#' new_style <- style$copy()
#'
#' }
#'
#' @format NULL
#'
#' @section Methods:
#'
#' \describe{
#'
#' \item{\code{$update(..., selector = NULL)}}{
#' Updates the list of properties associated with this style.
#' \tabular{ll}{
#'   \code{...} \tab name/value properties \cr
#'   \code{selector} \tab If given, then used as the the selector for this style. If
#'   not given, then the first unnamed argument in \code{...} is interpreted as the
#'   character string for the selector. \cr
#' }
#' }
#'
#' \item{\code{$set_selector(selector)}}{
#' Updates the selector associated with this style.
#' \tabular{ll}{
#'   \code{selector} \tab new selector. either a character string or a \code{Selector} object \cr
#' }
#' }
#'
#' \item{\code{$remove(...)}}{
#' Removes properties from this style by name
#' \tabular{ll}{
#'   \code{...} \tab names of properties to remove (character strings) \cr
#' }
#' }
#'
#' \item{\code{$as_character()}}{
#' Convert \code{Style} to a character string.
#' }
#'
#' \item{\code{$as_inline()}}{
#' Convert \code{Style} to a single character string without the selector,
#' suitable for use as an inline style.
#' }
#'
#' \item{\code{$print()}}{
#' Print \code{Style} to terminal.
#' }
#'
#' \item{\code{$copy()}}{
#' Copy \code{Style}.
#' }
#'
#'}
#'
#'
#' @examples
#' \dontrun{
#' css_style('#marsha')$
#'   update(marsha = 'marsha')$
#'   update(ptag$display$none, width = "100%")
#' }
#'
#' @import R6
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Style <- R6::R6Class(
  "Style",

  public = list(
    selector    = NULL,
    declarations = NULL,
    indent = NULL,

    initialize = function(..., selector = NULL, indent = 0) {
      self$indent <- indent
      self$set_selector(selector = selector)
      self$declarations <- list()
      self$update(...)

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update and return the selector.
    # internal the selector is stored as a 'Selector' object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set_selector = function(selector = NULL) {
      if (is.null(selector)) {
        # do nothing
      } else if (is.character(selector)) {
        self$selector <- Selector$new(selector)
      } else if (inherits(selector, "Selector")) {
        self$selector <- selector
      } else {
        stop("Style$set_selector(): Only understands character strings or Selector objects, not ", class(selector))
      }
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add declarations which consists of a property and a value
    # derived from the named arguments to this method.
    # If there any unnamed arguments, then the first one is considered the selector
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update = function(...) {
      varargs <- list(...)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # If caller is using the 'prop' helper then these results will contain
      # a list nested within the '...' argument.
      # So use unlist() to carefully remove one layer of listing
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      is_list_element <- vapply(varargs, is.list, logical(1))
      if (any(is_list_element)) {
        varargs <- unlist(varargs, recursive = FALSE, use.names = TRUE)
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # If there is a single unnamed argument, then assume it's the
      # selector name and remove it from varargs
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      unnamed_idx <- which(names(varargs) == "")
      if (is.null(names(varargs)) && length(varargs) == 1) {
        unnamed_idx <- 1
      }
      if (length(unnamed_idx) == 1) {
        self$set_selector(varargs[[unnamed_idx]])
        varargs[unnamed_idx] <- NULL
      }


      is_list_element <- vapply(varargs, is.list, logical(1))
      if (any(is_list_element)) {
        stop("Style$update(): only one level of list() elements is undone. i need a better error message", call. = FALSE)
      }

      if (!all_named(varargs)) {
        stop("All arguments to Style$update() must be named. Got: ", deparse(varargs))
      }
      prop_names <- names(varargs)

      # Fix the names for underscores and the spelling of 'colour'
      prop_names <- gsub('colour', 'color', prop_names)
      prop_names <- gsub("_", "-", prop_names)
      varargs    <- setNames(varargs, prop_names)

      # pull out the 'transform' and handle it separately
      trans <- varargs$transform
      varargs$transform <- NULL

      # all other declarations overwrite any existing value
      self$declarations <- modifyList(self$declarations, varargs)

      # but the 'transform' declaration appends
      if (!is.null(trans)) {
        self$declarations$transform <- paste0(c(self$declarations$transform, trans), collapse = " ")
      }

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Remove properties by name
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    remove = function(...) {
      self$declarations[as.character(list(...))] <- NULL
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Remove the transform
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    reset_transform = function() {
      self$declarations$transform <- NULL
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create character represenation.
    # If inline = TRUE, then print on a single line and exclude the selector
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character = function(inline = FALSE, ...) {
      style_strings <- c()

      if (inline || is.null(self$selector)) {
        indent   <- ''
        spacer   <- ''
        collapse <- "; "
      } else if (self$indent > 0) {
        indent   <- '    '
        spacer   <- '        '
        collapse <- ";\n"
      } else {
        indent   <- ''
        spacer   <- '    '
        collapse <- ";\n"
      }

      prop_names <- names(self$declarations)

      for (i in seq_along(self$declarations)) {
        prop_name     <- prop_names[i]
        value         <- self$declarations[[i]]
        this_string   <- paste0(spacer, prop_name, ": ", value)
        style_strings <- c(style_strings, this_string)
      }

      style_string <- paste(style_strings, collapse = collapse)

      if (nchar(style_string) > 0) {
        style_string <- paste0(style_string, ";")
      }


      if (inline || is.null(self$selector)) {
        style_string
      } else if (length(self$declarations) == 1) {
        paste0(indent, self$selector, " { ", trimws(style_string), " }")
      } else {
        paste0(indent, self$selector, " {\n", style_string, "\n", indent, "}")
      }
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # create an inline style string
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_inline = function(...) {
      self$as_character(inline = TRUE, ...)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Print
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print = function(...) {
      cat(self$as_character(...))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Deep copy needed as object contains R6 object (selector)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    copy = function() {
      self$clone(deep = TRUE)
    }
  ),

  private = list(
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # When called with `$clone(deep = TRUE)`, the 'deep_clone' function is
    # called for every name/value pair in the object.
    # See: https://r6.r-lib.org/articles/Introduction.html
    # Need special handling for:
    #   - selector
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    deep_clone = function(name, value) {
      if (name %in% c('selector')) {
        if (inherits(value, "R6")) value$clone(deep = TRUE) else value
      } else {
        value
      }
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Retrieve the character representation of a Style object
#'
#' @param x Style object
#' @param ... other arguments
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.Style <- function(x, ...) {
  x$as_character(...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Helper function for creating a Style object for a CSS style (requires selector)
#'
#' @param selector character string or object of Selector class
#' @param ... named list of properties for the style e.g. `color = "black"`
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_style <- function(..., selector = NULL) {
  invisible(Style$new(..., selector = selector))
}


