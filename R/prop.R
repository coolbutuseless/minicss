



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a function with the given arguments that simply returns the first
# argument value. all args are included for auto-complete during coding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_first_arg_func <- function(name) {
  fstring <- glue("function(value) {{
     res <- setNames(list(value), name)
     class(res) <- c('css_property')
     res
  }}")

  eval(parse(text = fstring))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Some magic to create the property helper
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_inner_inner <- function(value, name) {
  res <- setNames(list(value), name)
  class(res) <- c('css_property')
  res
}

create_inner <- function(prop) {
  res <- lapply(
    prop$values,
    create_inner_inner, prop$name
  )

  res <- setNames(res, prop$values)
  res$set <- create_first_arg_func(prop$name)

  res
}



prop_names <- vapply(properties, function(x) {x$name}, character(1))
inners     <- lapply(properties, create_inner)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' CSS property helper
#'
#' Uses autocomplete to help write some standard propertys
#'
#' @importFrom stats setNames
#' @import glue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_prop <- setNames(inners, prop_names)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a CSS 'property' object to a string
#'
#' @param x property object
#' @param ... other arguments
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.css_property <- function(x, ...) {
  paste0(names(x), ": ", unname(unlist(x)), ";")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a CSS 'property' object
#'
#' @param x property object
#' @param ... other arguments
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.css_property <- function(x, ...) {
  cat(as.character(x), "\n", sep = "")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Helper to manually create new "!important" properies
#'
#' @param ... a name/value pair
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
imp <- function(...) {

  ll <- list(...)

  if (inherits(ll[[1]], 'css_property')) {
    res <- ll[[1]]
    res[[1]] <- paste(res[[1]], "!important")
    return(res)
  }

  value <- unname(ll[[1]])
  name  <- names(ll)[1]
  value <- paste(value, "!important")
  res <- setNames(list(value), name)
  class(res) <- c('css_property')
  res
}
















