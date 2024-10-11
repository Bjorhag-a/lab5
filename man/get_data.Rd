\name{get_data}
\alias{get_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_data(kpi, municipality, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{kpi}{
%%     ~~Describe \code{kpi} here~~
}
  \item{municipality}{
%%     ~~Describe \code{municipality} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (kpi, municipality, year) 
{
    m_id_res <- GET("http://api.kolada.se/v2/municipality", query = list(title = municipality))
    m_id_data <- fromJSON(rawToChar(m_id_res$content))
    m_id <- m_id_data$values$id
    print(m_id)
    res <- GET(paste("http://api.kolada.se/v2/data/kpi/", kpi, 
        "/municipality/", m_id, "/year/", year, sep = ""))
    data <- fromJSON(rawToChar(res$content))
    values <- data$values$values
    return(values)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
