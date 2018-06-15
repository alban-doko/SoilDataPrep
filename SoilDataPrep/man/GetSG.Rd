\name{GetSG}
\alias{GetSG}
\title{Download soil properties.}

\description{Function to download 72 raster files describing different soil characteristics for several depth with a resolution of 250 m for the required catchment/study area.}

\usage{GetSG(catch)}

\details{catch: shapefile of the catchment/study area, different projections are accepted. Read the shapefile with readOGR().}

\value{
Returns 72 files in a new folder "SoilGrids" in your working directory: 
\item{TAXNWRB}{soil class due to WRB}

The following characteristics are available for 7 depth: 0, 5, 15, 30, 60, 100, 200 cm (sl1 - sl7):
\item{BLDFIE}{Bulk density (fine earth) in kg / cubic-meter}
\item{CECSOL}{Cation exchange capacity of soil in cmolc/kg}
\item{CLYPPT}{Clay content (0-2 micro meter) mass fraction in percent}
\item{CRFVOL}{Coarse fragments volumetric in percent}
\item{ORCDRC}{Soil organic carbon content (fine earth fraction) in g per kg}
\item{PHIHOX}{Soil pH x 10 in H2O}
\item{SLTPPT}{Silt content (2-50 micro meter) mass fraction in percent}
\item{SNDPPT}{Sand content (50-2000 micro meter) mass fraction in percent}
\item{WWP}{Available soil water capacity (volumetric fraction) until wilting point}
\item{AWCtS}{Saturated water content (volumetric fraction) for tS}

}
\references{
soilgrids.org
}
\author{Sophia Dobkowitz <dobkowit@uni-potsdam.de>}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#Create a spatial polygon and download soil characteristics from SoilGrids for the extent of this polygon:
coords = matrix(c(-44,-10,    -41,-9,
                  -40,-13,    -45,-14), 
                ncol = 2, byrow = TRUE)
p <- Polygon(coords)
catch <- SpatialPolygons(list(Polygons(list(p), ID = "a")))
#plot(catch, axes = TRUE, col="yellow")
GetSG(catch)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
