

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a single keyframe for a style at a moment in time.
#'
#' This is identical to the \code{Style} class except the first argument is a
#' time specification (either 'from', 'to', or a percentage), instead of a name
#'
#'
#' @examples
#' \dontrun{
#' Keyframe$new('from', color = '#000000')
#' Keyframe$new('to'  , color = '#ffffff')
#' }
#'
#' @import R6
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Keyframe <- R6::R6Class(
  "Keyframe", inherit = Style,

  public = list(
    initialize = function(time, ...) {
      if (!time %in% c('from', 'to') && !endsWith(time, '%')) {
        if (!(is.numeric(time) && time >= 0 && time <= 100)) {
          stop("Keyframe$new(): 'time' must be 'from', 'to' or a number between 0 and 100")
        }
        time <- paste0(time, '%')
      }

      super$initialize(selector = time, ..., indent = 1)
    }
  )
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a named Keyframes style suitable to insert in a StyleSheet
#'
#' @import R6
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Keyframes <- R6::R6Class(
  "Keyframes", inherit = StyleSheet,

  public = list(
    name = NULL,

    initialize = function(name, ...) {
      if (!is.character(name)) {
        stop("Keyframes$new(): the first argument must be a character string representing a name, not: ", class(name))
      }
      self$name <- name
      super$initialize(...)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create a Style object, add it it to the current stylesheet and return it.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add = function(time, ...) {
      this_keyframe <- Keyframe$new(time = time, ...)
      do.call(self$append, list(this_keyframe))
      invisible(this_keyframe)
    },

    as_character = function(...) {
      strings <- vapply(self$styles, function(x) {x$as_character()}, character(1))
      children <- paste(strings, collapse = "\n")
      paste0("@keyframes ", self$name, " {\n", children, "\n}")
    }
  )
)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @usage NULL
#' @rdname Keyframe
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_keyframe <- function(time, ...) {
  Keyframe$new(time = time, ...)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @usage NULL
#' @rdname Keyframes
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_keyframes <- function(name, ...) {
  Keyframes$new(name = name, ...)
}




if (FALSE) {
  library(htmltools)
  library(minicss)

  # Construct individual frames
  k1 <- css_keyframe('from', color = '#ffffff')
  k2 <- css_keyframe('to'  , color = '#123456',
                     css_prop$transform$translateX(40))

  # Combine frames into @keyframes
  kf <- css_keyframes('ex1', k1, k2)

  # Create style to attach @keyframes to the 'h1' type
  h1 <- css_style('h1',
                  css_anim('ex1', duration = 1, direction = 'alternate')
  )

  ## Create a style sheet
  ss <- css_stylesheet(h1, kf)

  # Create some html using this style sheet
  html <- glue::glue("<html><head>
  <style>{ss}</style>
  </head>
  <body>
  <h1> Hello #RStats </h1>
  </body></html>")

  html_print(HTML(html))
}
























