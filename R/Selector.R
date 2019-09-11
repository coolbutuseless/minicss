

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Collapse a name value pair into a string for a pseudo class or pseudo element
#
# either   name:value  for pseudo classes
#    or    name::value for pseudo elements
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
collapse_pseudo <- function(name, value, prefix = ":") {
  if (isTRUE(value)) {
    paste0(prefix, name)
  } else {
    paste0(prefix, name, "(", value, ")")
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' CSS selector builder
#'
#' Based upon a naive reading of \url{https://developer.mozilla.org/en-US/docs/Web/CSS/Reference#Selectors}.
#'
#' @docType class
#'
#' @usage NULL
#' @section Usage:
#' \preformatted{selector <- Selector$new()
#' selector <- css_sel()
#'
#' selector$type(type_name)
#' selector$class(...)
#' selector$id(id_name)
#'
#' new_selector <- selector$copy()
#'
#' }
#'
#' @format NULL
#'
#' @section Methods:
#'
#' \describe{
#'
#' \item{\code{$type(type_name)}}{
#' Set the type for the selector
#' \tabular{ll}{
#'   \code{type_name} \tab type name \cr
#' }
#' }
#'
#' \item{\code{$id(id_name)}}{
#' Set the id for the selector
#' \tabular{ll}{
#'   \code{id_name} \tab id name \cr
#' }
#' }
#'
#' \item{\code{$class(...)}}{
#' Add to the class list for for the selector
#' \tabular{ll}{
#'   \code{...} \tab class names \cr
#' }
#' }
#'
#' \item{\code{$attr(..., modifier = c('exact', 'choice', 'prefix', 'suffix', 'contains'))}}{
#' Add attribute matching to the selector. See \url{https://developer.mozilla.org/en-US/docs/Web/CSS/Attribute_selectors}
#' \tabular{ll}{
#'   \code{...} \tab name/value pairs for the atributes \cr
#'   \code{modifer} \tab by default 'exact' attribute matching, but can select from among the different types of matching  \cr
#' }
#' }
#'
#' \item{\code{$pseudo_class(...)}}{
#' Set pseudo classes on the selector.
#' \tabular{ll}{
#'   \code{...} \tab Use \code{name = TRUE} to set pseudo-classes \cr
#' }
#' }
#'
#' \item{\code{$pseudo_element(...)}}{
#' Set pseudo element on the selector.
#' \tabular{ll}{
#'   \code{...} \tab Use \code{name = TRUE} to set pseudo-elements \cr
#' }
#' }
#'
#'
#'
#' \item{\code{$descendant_of(selector), $parent_of(selector), $child_of(selector), $prior_adjacent(selector), $prior_sibling(selector), $and(selector)}}{
#' Set relationship with another selector
#' \tabular{ll}{
#'   \code{selector} \tab Other selector \cr
#' }
#' }
#'
#' \item{\code{$as_character()}}{
#' Convert \code{Selector} to a character string.
#' }
#'
#' \item{\code{$print()}}{
#' Print \code{Selector} to terminal.
#' }
#'
#' \item{\code{$copy()}}{
#' Copy \code{Selector}.
#' }
#'
#' }
#'
#'
#'
#' @examples
#' \dontrun{
#' selector <- css_sel(".marsha")$child_of('carol')
#' }
#'
#' @import R6
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Selector <- R6::R6Class(
  "Selector",


  public = list(

    .type           = NULL,
    .id             = NULL,
    .classes        = NULL,
    .child          = NULL,
    .parent         = NULL,
    .sibling        = NULL,
    .adjacent       = NULL,
    .additional     = NULL,
    .attr           = NULL,
    .pseudo_class   = NULL,
    .pseudo_element = NULL,

    initialize = function(selector = NULL) {
      self$.classes    <- character(0)
      self$.sibling    <- list()
      self$.additional <- list()
      self$.attr       <- list()

      if (is.null(selector)) {
        # do nothing
      } else if (inherits(selector, 'Selector')) {
        for (name in names(Selector$public_fields)) {
          self[[name]] <- selector[[name]]
        }
      } else if (startsWith(selector, ".")) {
        self$class(selector)
      } else if (startsWith(selector, '#')) {
        self$id(selector)
      } else {
        self$type(selector)
      }

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Type - singular
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    type = function(type_name) {
      stopifnot(is.character(type_name))
      self$.type <- type_name
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # classes - multiple
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    class = function(...) {
      class_names <- as.character(list(...))
      has_dot <- startsWith(class_names, ".")
      class_names[!has_dot] <- paste0(".", class_names[!has_dot])

      if (any(substr(class_names, 2, 2) %in% 0:9)) {
        stop("Selector$class(): Class names cannot start with a digit")
      }

      self$.classes <- c(self$.classes, class_names)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ID - singular
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    id = function(id_name) {
      stopifnot(is.character(id_name))
      if (!startsWith(id_name, "#")) {
        id_name <- paste0("#", id_name)
      }

      if (any(substr(id_name, 2, 2) %in% 0:9)) {
        stop("Selector$id(): ID name cannot start with a digit")
      }

      self$.id <- id_name
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # selector attributes
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    attr = function(..., modifier = c('exact', 'choice', 'prefix', 'suffix', 'contains')) {
      named_value <- list(...)
      stopifnot(length(named_value) == 1)
      if (is.null(names(named_value))) {
        self$.attr <- c(self$.attr, named_value)
      } else {

        modifier  <- match.arg(modifier)
        eq <- switch(
          modifier,
          exact  = '=',
          choice = '~=',
          prefix = '^=',
          suffix = '$=',
        )

        new_attr <- paste0(names(named_value), eq, '"', unname(named_value), '"')
        self$.attr <- c(self$.attr, new_attr)

      }
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Pseudo class
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pseudo_class = function(active, any_link, blank, checked, default, defined, dir, disabled, empty, enabled, first, first_child, first_of_type, fullscreen, focus, focus_visible, focus_within, has, host, host_context, hover, indeterminate, in_range, invalid, is, lang, last_child, last_of_type, left, link, not, nth_child, nth_col, nth_last_child, nth_last_col, nth_last_of_type, nth_of_type, only_child, only_of_type, optional, out_of_range, read_only, read_write, required, right, root, scope, target, valid, visited, ...) {
      pcs <- find_args(...)

      pc_names <- names(pcs)
      pc_names <- gsub("_", "-", pc_names)

      pcs <- mapply(collapse_pseudo, pc_names, unname(pcs), MoreArgs = list(prefix = ':'), USE.NAMES = FALSE)

      self$.pseudo_class <- c(self$.pseudo_class, pcs)

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Pseudo Element
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pseudo_element = function(after, before, cue, first_letter, first_line, selection, slotted, ...) {
      pes <- find_args(...)

      pe_names <- names(pes)
      pe_names <- gsub("_", "-", pe_names)


      pes <- mapply(collapse_pseudo, names(pes), unname(pes), MoreArgs = list(prefix = '::'), USE.NAMES = FALSE)

      self$.pseudo_element <- c(self$.pseudo_element, pes)

      invisible(self)
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Combinators
    # descendant:         A B
    # child:              A > B
    # adjacent sibling;   A + B
    # general sibling:    A ~ B
    #
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    descendant_of = function(selector) {
      self$.descendent <- selector
      invisible(self)
    },

    parent_of = function(selector) {
      self$.child <- selector
      invisible(self)
    },

    child_of = function(selector) {
      self$.parent <- selector
      invisible(self)
    },

    prior_adjacent = function(selector) {
      self$.adjacent <- append(self$.adjacent, selector)
      invisible(self)
    },

    prior_sibling = function(selector) {
      self$.sibling <- append(self$.sibling, selector, after = 0)
      self
    },

    and = function(selector) {
      self$.additional <- append(self$.additional, selector)
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Convert to character representation
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character = function(...) {
      id_text    <- self$.id
      class_text <- paste(self$.classes, collapse="")

      if (!is.null(self$.child)) {
        child_text <- as.character(self$.child)
        child_text <- paste0(" > ", child_text)
      } else {
        child_text <- NULL
      }

      if (!is.null(self$.parent)) {
        parent_text <- as.character(self$.parent)
        parent_text <- paste0(parent_text, " > ")
      } else {
        parent_text <- NULL
      }

      if (length(self$.additional) > 0) {
        additional <- vapply(self$.additional, as.character, character(1))
        additional <- paste0(additional, collapse = ", ")
        additional <- paste0(", ", additional)
      } else {
        additional <- NULL
      }

      if (length(self$.attr) > 0) {
        attr_text <- paste0('[', self$.attr, ']', sep = '', collapse = '')
      } else {
        attr_text <- NULL
      }

      if (length(self$.pseudo_class) > 0) {
        pseudo_class_text <- paste0(self$.pseudo_class, collapse = "")
      } else {
        pseudo_class_text <- NULL
      }

      if (length(self$.pseudo_element) > 0) {
        pseudo_element_text <- paste0(self$.pseudo_element, collapse = "")
      } else {
        pseudo_element_text <- NULL
      }


      paste0(parent_text, self$.type, id_text, class_text, pseudo_class_text,
             pseudo_element_text, attr_text, child_text, additional)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Print
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print = function(...) {
      cat(self$as_character(...))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Selector objects don't contain any R6 objects, so fine to just do
    # a shallow ocpy
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    copy = function(...) {
      self$clone(...)
    }

  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Retrieve the character representation of a Selector object
#'
#' @param x Selector object
#' @param ... other arguments
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.Selector <- function(x, ...) {
  x$as_character(...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Helper for creating a Selector object
#'
#' @param selector string or another Selector object to clone
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_sel <- function(selector = NULL) {
  Selector$new(selector)
}

