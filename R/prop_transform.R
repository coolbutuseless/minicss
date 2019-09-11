

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The properties for transform objects are different from other properties.
#
# Most other properties are simple name/value pairs e.g. "color: #000", but
# transforms are more like name/function-call pairs.
#
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a function to create a transform property
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_transform_func <- function(spec) {
  args <- spec$args
  name <- spec$name

  if (length(args) == 0) {
    inner_args <- ''
  } else if (name == 'rotate3d') {
    inner_args <- paste0("({x} {y} {z} {angle}{unit})")
  } else if (!is.null(spec$units)) {
    inner_args <- paste0(paste0("{", args, "}"), "{unit}", collapse = " ")
    inner_args <- paste0("(", inner_args, ")")
  } else {
    inner_args <- paste0(paste0("{", args, "}"), collapse = " ")
    inner_args <- paste0("(", inner_args, ")")
  }

  if (!is.null(spec$units)) {
    outer_args <- c(args, glue("unit='{spec$units}'"))
  } else {
    outer_args <- args
  }

  outer_args <- paste0(outer_args, collapse = ", ")
  func_string <- glue("function({outer_args}) {{
  res <- list(transform = glue('{name}{inner_args}'))
  class(res) <- 'css_property'
  res
}}")

  eval(parse(text = func_string))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create all the transform-property creator functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transform_names <- lapply(transform_info, function(x) x$name)
css_prop$transform <- setNames(lapply(transform_info, create_transform_func), transform_names)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a function to create a transform property
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_transform_method <- function(spec) {
  args <- spec$args
  name <- spec$name

  if (length(args) == 0) {
    inner_args <- ''
  } else if (name == 'rotate3d') {
    inner_args <- paste0("({x} {y} {z} {angle}{unit})")
  } else if (!is.null(spec$units)) {
    inner_args <- paste0(paste0("{", args, "}"), "{unit}", collapse = " ")
    inner_args <- paste0("(", inner_args, ")")
  } else {
    inner_args <- paste0(paste0("{", args, "}"), collapse = " ")
    inner_args <- paste0("(", inner_args, ")")
  }

  if (!is.null(spec$units)) {
    outer_args <- c(args, glue("unit='{spec$units}'"))
  } else {
    outer_args <- args
  }

  self <- NULL

  outer_args <- paste0(outer_args, collapse = ", ")
  func_string <- glue("function({outer_args}) {{
  self$update(transform = glue('{name}{inner_args}'))
  invisible(self)
}}")

  eval(parse(text = func_string))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add a method for each transform to the style object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (spec in transform_info) {

  meth <- create_transform_method(spec)

  Style$set("public", spec$name, meth)
}





