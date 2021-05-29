% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Labels.R
\name{use_labels}
\alias{use_labels}
\title{Utiliza las etiquetas de datos de una variable}
\usage{
use_labels(tbl, vars = NULL, dict = NULL)
}
\arguments{
\item{tbl}{\link{data.frame}: Conexión a base de datos o dataframe con los datos}

\item{vars}{\link{character}: Si especificado, solo se asignaran las etiquetas a esas variables.}

\item{dict}{\link{data.frame}: Diccionario con todas las etiquetas de datos a utilizar
si aún no han sido asignadas. Vea \code{Details}.}
}
\value{
Los datos suministrados en el argumento \code{tbl}, pero en lugar de
valores utilizando las etiquetas de datos correspondientes
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\details{
Esta función asume que las etiquetas de datos han sido asignadas a
los datos con anterioridad, a menos que se suministre un \code{dict}, en cuyo
caso se utilizará este último para asignar y utilizar las etiquetas de datos.
En tal sentido, es recomendable utilizar las funciones equivalentes en los
paquetes ENDOMER que vienen con un diccionario
}
\examples{
\dontrun{
  dict <- list(
    S2_P8 = list(
      lab = "Â¿Tiene esta vivienda instalaciÃ³n para agua corriente por tuberÃ­a
       conectada a la red pÃºblica?",
      labs = c("Si" = 1, "No" = 2)
    )
   ) # enftr::dict
  enft <- data.frame(S2_P8 = c(1, 2))
  enft
  use_labels(enft, dict = dict)
}
}
\seealso{
Etiquetas de datos \code{vignette("labels", package = "endomer")}
}