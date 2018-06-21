\name{SoilDataPrep-package}
\alias{SoilDataPrep-package}
\alias{SoilDataPrep}
\docType{package}
\title{
\packageTitle{SoilDataPrep}
}
\description{
\packageDescription{SoilDataPrep}
}

\details{
\packageDESCRIPTION{SoilDataPrep}
\packageIndices{
GetDTB(catch)
GetDTB(catch)
SoilParams(catch, DEM, c=1000)
SoilParamsCont(catch, DEM, c=1000)
}}

\author{
\packageAuthor{SoilDataPrep}
\packageMaintainer{SoilDataPrep}
}

\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
#Create a spatial polygon
coords = matrix(c(-44,-10,    -41,-9,
                  -40,-13,    -45,-14), 
                ncol = 2, byrow = TRUE)
p <- Polygon(coords)
catch <- SpatialPolygons(list(Polygons(list(p), ID = "a")))

#Download soil characteristics from SoilGrids for the extent of this polygon:
GetSG(catch)

#Download depth to bedrock from Pelletier et al. for the extent of this polygon:
GetDTB(catch)

#Apply pedotransfer functions to the downloaded soil characteristics and prepare soil input tables for WASA:
DEM<-??
c<-500
SoiParams(catch, DEM, c, res_DEM)
}
