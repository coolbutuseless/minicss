
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a helper method to the 'Ruleset' class
#'
#' @param property_name the name of the CSS property. will also be used as the property name
#' @param values character vector of argument names
#'
#' @importFrom glue glue
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_method <- function(property_name, values) {

  property_name <- gsub("-", "_", property_name)
  values          <- gsub('-', '_', values)

  if (length(values) == 0) {
    arg_text <- "..."
  } else {
    arg_text <- glue("..., {paste('`', values, '`', sep = '', collapse = ', ')}")
  }

  func_text <- glue("function({arg_text}) {{
  res <- as.list(sys.call())
  if (length(res) != 2) stop('${property_name}(): Must have just a single argument')
  res <- res[2]
  if (is.null(names(res))) {{
    value <- as.character(res)
  }} else {{
    value <- names(res)
  }}
  do.call(self$attr, list({property_name} = value))
  invisible(self)
}}")

  func <- eval(parse(text = func_text))

  Style$set("public", property_name, func)
  func_text
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dynamically add all the element creation methods to 'Style' class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  for (prop in properties) {
    add_method(prop$name, prop$values)
  }
}

