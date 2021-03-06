% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Style.R
\docType{class}
\name{Style}
\alias{Style}
\title{CSS style builder}
\description{
CSS style builder
}
\section{Usage}{

\preformatted{style <- Style$new()
style <- css_style()

style$set_selector(".thing")
style$remove("color")
style$update(color = 'blue')
style$as_character()
style$as_inline()
new_style <- style$copy()

}
}

\section{Methods}{


\describe{

\item{\code{$update(..., selector = NULL)}}{
Updates the list of properties associated with this style.
\tabular{ll}{
  \code{...} \tab name/value properties \cr
  \code{selector} \tab If given, then used as the the selector for this style. If
  not given, then the first unnamed argument in \code{...} is interpreted as the
  character string for the selector. \cr
}
}

\item{\code{$set_selector(selector)}}{
Updates the selector associated with this style.
\tabular{ll}{
  \code{selector} \tab new selector. either a character string or a \code{Selector} object \cr
}
}

\item{\code{$remove(...)}}{
Removes properties from this style by name
\tabular{ll}{
  \code{...} \tab names of properties to remove (character strings) \cr
}
}

\item{\code{$as_character()}}{
Convert \code{Style} to a character string.
}

\item{\code{$as_inline()}}{
Convert \code{Style} to a single character string without the selector,
suitable for use as an inline style.
}

\item{\code{$print()}}{
Print \code{Style} to terminal.
}

\item{\code{$copy()}}{
Copy \code{Style}.
}

}
}

\examples{
\dontrun{
css_style('#marsha')$
  update(marsha = 'marsha')$
  update(ptag$display$none, width = "100\%")
}

}
\keyword{datasets}
