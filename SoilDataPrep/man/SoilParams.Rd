\name{SoilParams}
\alias{SoilParams}
\title{Extract soil parameters using pedotransfer functions}

\description{
Function to treat the study area tile by tile. Slope is derivated from the DEM to choose between the two depth to bedrock values. The other soil characteristics are from SoilGrids. Then, pedotransfer functions are applied to calculate the soil parameters and prepare the input tables for WASA.}

\usage{SoilParams(catch, DEM, c, res_DEM)}

\details{
catch: shapefile of the catchment/study area, geographic coordinate system (gcs) WGS1984 longlat, no projection. Read the shapefile with readOGR().
DEM: digital elevation model of the study area (raster), gcs WGS1984 longlat, no projection. Read the raster file with raster().
c: maximum number of cells in x and y direction for single tiles, e.g.1000
res_DEM: resolution of the DEM in m, e.g. 90}

\value{

\item{last_tile}{Indicates which was the last treated tile to allow continue a not-finished run from this tile with SoilParamsCont}
\item{soil_sum_recent}{Shows the results of the last calculated tile}
\item{soil_sum_collected}{Table where the results of all tiles are put together}
\item{soil_sum_weighted}{The final result of all tiles containing mean values for each soil_id, weighted due to the number of occurrence}

Input tables for WASA:
\item{particle_classes}{Definition of particle classes}
\item{r_soil_contains_particles}{Fraction of each particle class for each soil_id, only topsoil is considered}
\item{horizons}{Describes each horizon of each soil_id}
\item{soils}{Bedrock, alluvium and organic matter content for each soil_id}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sophia Dobkowitz <dobkowitz@uni-potsdam.de>

Till Francke <francke@uni-potsdam.de>

Tobias Pilz <tpilz@uni-potsdam.de>}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
ptf.rawls, euptf
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
}

