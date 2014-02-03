\name{sju.wordwrap}
\alias{sju.wordwrap}
\title{Insert line breaks in long labels}
\usage{
sju.wordwrap(labels, wrap)
}
\arguments{
  \item{labels}{The label(s) (i.e. character string). You
  can also pass several strings as vector (e.g.
  \code{labels=c("first long string", "second long
  string")})}

  \item{wrap}{The amount of chars per line (i.e. line
  length)}
}
\value{
New label(s) with line breaks inserted at every
\code{wrap}'s position.
}
\description{
Insert line breaks in long character strings. Useful if you
want to wordwrap plot labels.
}
\examples{
sju.wordwrap(c("A very long string", "And another even longer string!"), 10)
}

