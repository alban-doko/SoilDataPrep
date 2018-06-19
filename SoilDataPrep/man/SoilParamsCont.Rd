\name{SoilParamsCont}
\alias{SoilParamsCont}
\title{Continue to extract soil parameters using pedotransfer functions}

\description{
Function to continue the extraction of soil parameters started with SoilParams at the tile where the process stopped.}

\usage{SoilParamsCont(catch, DEM, c)}

\details{
catch: shapefile of the catchment/study area, different projections are accepted. Read the shapefile with readOGR().
DEM: digital elevation model of the study area (raster), gcs WGS1984 longlat, no projection. Different resolutions are accepted. Read the raster file with raster().
c: maximum number of cells in x and y direction for single tiles, default is set to 500}

\value{

\item{last_tile}{Indicates which was the last treated tile, allowing to continue a not-finished run from this tile with SoilParamsCont}
\item{soil_sum_recent}{Shows the results of the last calculated tile}
\item{soil_sum_collected}{Results of all treated tiles together}
\item{soil_sum_weighted}{The final result of all tiles: mean values for each soil_id, weighted due to the number of occurrence}
\item{Mapsoils/soils_catchment.tif}{Map with the new soil ids, to ids with alluvium 1000 was added}

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

