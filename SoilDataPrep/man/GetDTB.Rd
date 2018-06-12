\name{GetDTB}
\alias{GetDTB}
\title{Download depth to bedrock.}

\description{Function to download 2 global 1-km grids of depth to bedrock for the required catchment/study area.}

\usage{GetDTB(catch)}

\details{catch: shapefile of the catchment/study area, geographic coordinate system WGS1984 longlat, no projection. Read the shapefile with readOGR().}

\value{
Returns two files in a new folder "Pelletier_DTB" in your working directory: 

\item{depth_5}{upland hill-slope soil thickness in m}
\item{depth_6}{upland valley-bottom and lowland sedimentary deposit thickness}
}

\references{
Pelletier, J.D. et al. 2016, A gridded global data set of soil, immobile regolith, and sedimentary deposit thicknesses for regional and global land surface modeling, Journal of Advances in Modeling Earth Systems, 8.

https://webmap.ornl.gov/wcsdown/dataset.jsp?ds_id=1304
}

\author{
Sophia Dobkowitz <dobkowit@uni-potsdam.de>
}

\examples{
#Create a spatial polygon and download depth to bedrock from Pelletier et al. for the extent of this polygon:
coords = matrix(c(-44,-10,
                  -41,-9,
                  -40,-13,
                  -45,-14), 
                ncol = 2, byrow = TRUE)

p <- Polygon(coords)
catch <- SpatialPolygons(list(Polygons(list(P1), ID = "a")))
#plot(sp, axes = TRUE, col="yellow")
GetDTB(catch)
}
