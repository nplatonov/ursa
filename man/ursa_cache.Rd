\name{ursa_cache}
\alias{ursa_cache}
\title{
Cache management of ursa package
}
\description{
This help topic is about how cache is managed in the package.
}
\usage{
ursa_cache()
}  
\details{
Users, who want to keep cache files between R sessions, should define \link[base:options]{option} with name \code{ursaCacheDir} and value of the path for storage of cache files. This setting can be specified if \code{~/.Rprofile} file, or be in your code. If you specify permanent cache directory as sub-directory of \code{\link[base:tempfile]{tempdir()}} (see example), it will be removed after finishing of R session.
}
\value{
NULL
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
There is no neccessary to call this function. It just defines this help topic.
}

\examples{
\donttest{ ## internet connection is required
options(ursaCacheDir=file.path(tempdir(),".ursaCacheDir"))
print(c(tempdir=tempdir(),ursaCacheDir=getOption("ursaCacheDir")))
glance("Mount Eden",place="park")
dir(getOption("ursaCacheDir"))
}}
\keyword{environment}
