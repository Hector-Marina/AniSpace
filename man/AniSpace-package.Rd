\name{AniSpace-package}
\alias{AniSpace-package}
\alias{AniSpace}
\docType{package}

\title{
  R package for analysing the high-resolution position data of livestock and wildlife.
}

\description{
  \pkg{AniSpace} provides tools to clean, transform, and analyse
  high-resolution position (GPS/RTLS) data from livestock and wildlife.
  Core features include movement metrics (speed, step length, turning angles),
  indoor/area usage summaries, efficient data filtering, and workflows to
  replicate analyses from the CSI:DT project (social network analyses,
  behavioural analyses, and area utilisation)
}

\details{
  The package emphasises minimal dependencies and reproducible pipelines for
  processing large streams of position fixes.

  \strong{Key functionality:}
  \itemize{
    \item Movement summaries: \code{\link{movement.AniSpace}}
    \item Polygon inclusion tests and filtering: e.g. \code{\link{filter_positions}}
    \item Interpolation / gap filling: e.g. \code{\link{interp_positions}}
    \item Turning-angle calculations and plots: e.g. \code{\link{plot_turning_angles}}
    \item Object checks/validation: e.g. \code{\link{validate_AniObj}}
    \item Example dataset: \code{\link{cows}}
  }

  \strong{Typical workflow:}
  \enumerate{
    \item Load example data with \code{data("cows")} or your own positions.
    \item Compute movement metrics with \code{movement.AniSpace()}.
    \item Optionally filter by areas/polygons and interpolate missing fixes.
    \item Summarise or visualise space use and movement behaviour.
  }

  \strong{Documentation and examples:}
  See the package vignettes (e.g., \emph{AniSpace examples}, \emph{CSI:DT replication})
  via \code{browseVignettes("AniSpace")} or the package website if available.
}

\author{
  Hector Marina (Maintainer)
  Keni Ren
  Ida Hansson
  Lars Rönnegård

  Maintainer: Hector Marina <hector.marina@slu.se>
}

\references{
  CSI:DT project materials and associated analyses.
  Additional references may be added in package vignettes.
}

\keyword{ Spatial temporal data package }

\seealso{
  \code{\link{movement.AniSpace}},
  \code{\link{cows}},
  \code{\link[utils]{browseVignettes}}
}

\examples{
  \dontrun{
    library(AniSpace)

    ## Load example data bundled with the package
    data("cows")
    str(cows$positions)

    df=load.Space(cows$positions)
    df=load.Info(df, InfObj=cows$animals)
    df=load.Area(AniObj=df, square2poly(cows$areas))

    ## Plot position data of one individual
    plot.AniSpace(df,IDs="2417195")

    # Filter position data
    df.ID.filt=filter.AniSpace(AniObj=df,NIDs=c(12:17))

    ## Compute movement metrics (example function)
    df.mov=movement.AniSpace(df.ID.filt)
    head(df.mov)

    ## Compute social networks
    AdjMat.obj=spatialint.AniSpace(AniObj=df.ID.filt)
    head(AdjMat)
  }
}
