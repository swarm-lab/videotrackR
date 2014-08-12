\name{make_background}
\alias{make_background}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes background image from video. 
}
\description{
make_backround 
}
\usage{
make_background(video, n = 10, type = "mean")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{video}{
%%     ~~Describe \code{video} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (video, n = 10, type = "mean") 
{
    frames <- seq(1, video$length(), length.out = n)
    if (type == "mean") {
        mat <- array(0, dim = c(video$dim(), 3))
        print("Loading images:")
        pb <- startpb(0, n - 1)
        for (i in 1:length(frames)) {
            mat <- mat + video$get_frame(frames[i])
            setpb(pb, i)
        }
        closepb(pb)
        print("Computing average image.")
        print("Done.")
        return(mat/n)
    }
    else if (type == "median") {
        mat <- array(0, dim = c(video$dim(), 3))
        mat.r <- array(NA, dim = c(video$dim(), n))
        mat.g <- array(NA, dim = c(video$dim(), n))
        mat.b <- array(NA, dim = c(video$dim(), n))
        print("Loading images:")
        pb <- startpb(0, n - 1)
        for (i in 1:length(frames)) {
            mat <- video$get_frame(frames[i])
            mat.r[, , i] <- mat[, , 1]
            mat.g[, , i] <- mat[, , 2]
            mat.b[, , i] <- mat[, , 3]
            setpb(pb, i)
        }
        closepb(pb)
        print("Computing median image. This is a slow process, please be patient.")
        print("   Median red:")
        mat[, , 1] <- pbapply(mat.r, c(1, 2), median.default)
        print("   Median green:")
        mat[, , 2] <- pbapply(mat.g, c(1, 2), median.default)
        print("   Median blue:")
        mat[, , 3] <- pbapply(mat.b, c(1, 2), median.default)
        print("Done.")
        return(mat)
    }
    else {
        stop("'type' should be 'mean' or 'median'")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
