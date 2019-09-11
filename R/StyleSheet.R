

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' CSS style sheet builder
#'
#' @docType class
#'
#' @usage NULL
#' @section Usage:
#' \preformatted{sheet <- StyleSheet$new()
#' sheet <- css_stylesheet()
#'
#' sheet$append(style)
#' sheet$remove(3)
#' sheet$add('h1', color = 'blue')
#' sheet$as_character()
#' new_sheet <- sheet$copy()
#'
#' }
#'
#' @format NULL
#'
#' @section Methods:
#'
#' \describe{
#'
#' \item{\code{$new(...)}}{
#' Initialise a stylesheet
#' \tabular{ll}{
#'   \code{...} \tab all arguments treated as \code{Style} objects and added to the style sheet \cr
#' }
#'}
#'
#'
#' \item{\code{$append(...)}}{
#' Append \code{Style} objects to this StyleSheet
#' \tabular{ll}{
#'   \code{...} \tab all arguments treated as Style objects and added to the style sheet \cr
#' }
#' }
#'
#' \item{\code{$remove(indices)}}{
#' Remove \code{Style} objects from this StyleSheet by index.
#' \tabular{ll}{
#'   \code{indices} \tab indices of Style objects to remove \cr
#' }
#' }
#'
#' \item{\code{$add(..., selector = NULL)}}{
#' Creates a \code{Style} object and adds it to the StyleSheet.  Returns the newly
#' created \code{Style}.
#' \tabular{ll}{
#'   \code{...} \tab name/value properties \cr
#'   \code{selector} \tab If given, then used as the the selector for this style. If
#'   not given, then the first unnamed argument in \code{...} is interpreted as the
#'   character string for the selector. \cr
#' }
#' }
#'
#' \item{\code{$as_character()}}{
#' Convert \code{StyleSheet} to a character string.
#' }
#'
#' \item{\code{$print()}}{
#' Print \code{StyleSheet} to terminal.
#' }
#'
#' \item{\code{$copy()}}{
#' Copy \code{StyleSheet}.
#' }
#'
#'}
#'
#'
#' @examples
#' \dontrun{
#' marsha <- css_style('#marsha')$
#'   update(marsha = 'marsha')$
#'   update(ptag$display$none, width = "100%")
#'
#' sheet <- StyleSheet$new(marsha)
#' }
#'
#' @import R6
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
StyleSheet <- R6::R6Class(
  "StyleSheet",
  public = list(
    styles = NULL,
    initialize = function(...) {
      do.call(self$append, list(...))
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Append Style objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    append = function(...) {
      new_styles <- list(...)
      is_Style <- vapply(new_styles,
                         function(x) {
                           inherits(x, 'Style') || inherits(x, 'StyleSheet')
                          },
                         logical(1))
      if (!all(is_Style)) {
        stop("StyleSheet$append(): only accepts 'Style' or 'StyleSheet' objects")
      }
      self$styles <- append(self$styles, new_styles)

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Remove styles by giving the indices of selectors to remove.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    remove = function(indices) {
      self$styles[indices] <- NULL
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create a Style object, add it it to the current stylesheet and return it.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add = function(..., selector = NULL) {
      this_style <- Style$new(..., selector = selector)

      if (is.null(this_style$selector)) {
        stop("StyleSheet$add(): must nominate a selector")
      }

      do.call(self$append, list(this_style))

      invisible(this_style)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Character representation is a concetentation of all the Style objects
    # in self$styles
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character = function(...) {
      strings <- vapply(self$styles, function(x) {x$as_character()}, character(1))
      paste(strings, collapse = "\n")
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # print
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print = function() {
      cat(self$as_character(), "\n")
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Deep copy needed as 'styles' is a list of R6 objects
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
    #   - 'styles' is a list of R6 objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    deep_clone = function(name, value) {
      if (name %in% c('styles')) {
        lapply(value, function(x) {if (inherits(x, "R6")) x$clone(deep = TRUE) else x})
      } else {
        value
      }
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Retrieve the character representation of a SStyleSheettyle object
#' @param x StyleSheet object
#' @param ... other arguments
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.StyleSheet <- function(x, ...) {
  x$as_character(...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname StyleSheet
#' @usage NULL
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_stylesheet <- function(...) {
  StyleSheet$new(...)
}


