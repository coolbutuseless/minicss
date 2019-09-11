

library(here)
library(dplyr)
library(jsonlite)
library(purrr)
library(tidyr)

# JSON source file
# https://github.com/adobe/brackets/blob/master/src/extensions/default/CSSCodeHints/CSSProperties.json
json_file <- here::here("data-raw", "CSSProperties.json")

json <- jsonlite::read_json(json_file, simplifyVector = TRUE)

options(useFancyQuotes = FALSE)


sink(here::here("R/Css_methods.R"))

# converting to a 'pretty' list implementation for inclusion in package
# as a source file
cat("css_properties <- list(\n")
for (i in seq_along(json)) {
  args <- deparse(json[[i]]$values, width.cutoff = 500)
  if (args == 'list()') {
    args <- "c()"
  }
  z <- sprintf("  list(name = %-28s, args = %s)", dQuote(names(json)[i]), args)
  if (i == length(json)) {
    cat(z, "\n", sep = "")
  } else {
    cat(z, ",\n", sep = "")
  }
}
cat(")\n")
sink()

