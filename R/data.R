#' Substrate Classifications From Side-Scan Sonar Imagery and Visual Surveys
#' 
#' Substrate classifications from the Cuyahoga River, Ohio, United States Of
#' America. Side-scan sonar was used to map the lower 68 kilometers of the river
#' and the resulting imagery was manually digitized as polygons and classified 
#' by substrate type, using the Wentworth scale (bedrock, boulder, cobble, 
#' pebble, sand, and silt/clay). An other patch type was used to document other 
#' bottom types (e.g., schools of fish, debris, and large woody debris). Gravel 
#' was not classified from sonar imagery, due to difficulty distinguishing it 
#' from both pebble and cobble in the imagery. Ground-truthing data was
#' collected at discrete points through visual surveys of the river bottom and 
#' from existing data sources at deeper locations that could not be surveyed
#' visually. Gravel was able to be distinguished in visual assessments and was
#' documented in the ground-truthing surveys.
#' 
#' @format A list of three simple features objects:
#' \itemize{
#'   \item{\strong{gtDom_sf:} A point shapefile of dominant substrates at ground-truthing
#'   locations.}
#'   \itemize{
#'    \item{\emph{samplID:} Unique identifier for each ground-truthing observation.}
#'     \item{\emph{trnscID:} Unique identifier for each cross-section of ground-truthing 
#'    observations. Three observations were made at each cross-section (left 
#'    bank, right bank, and middle of the channel).}
#'    \item{\emph{domSub:} Dominant substrate (composed over 60% of substrate 
#'    composition) at survey location. Substrate types are based on the Wentworth
#'    scale and include bedrock, boulder, cobble, pebble, gravel, sand, and
#'    silt/clay.}
#'    \item{\emph{prcntCD:} Percent of the substrate composition composed by the dominant substrate.}
#'    \item{\emph{mix:} Is the substrate at a ground-truthing point a mixture
#'    of multiple types (i.e., no one substrate type composes over 60% of the 
#'    composition)}
#'    \item{\emph{mixComp:} The substrate types that compose the substrate mix at a 
#'    point , if mix is yes.}
#'    \item{\emph{geometry:} The global positioning system coordinates recorded at a 
#'    point, as UTM X and Y for WGS 84 / UTM zone 17N.}
#'   }
#'   \item{\strong{sonarClass:} A polygon shapefile of substrate patches digitized and 
#'   classified from side-scan sonar imagery.}
#'   \itemize{
#'    \item{\emph{id:} Unique identifier for each classified polygon. Polygon substrate
#'    classifications were manually digitized and classified from side-scan sonar
#'    imagery.}
#'    \item{\emph{substrate:} Dominant substrate type within a digitized polygon. 
#'    Substrates classified were boulder, cobble, pebble, sand, silt/clay, 
#'    and other (e.g., tires, schools of fish, large woody debris).
#'    silt/clay, sand, pebble, cobble, boulder, bedrock, large woody debris, 
#'    unknown, and other unique objects (e.g., tires and schools of fish). Gravel 
#'    was not classified, due to the difficulty distinguishing gravel from sand 
#'    and pebble.}
#'    \item{\emph{area_m2:} The area of a polygon in square meters.}
#'     \item{\emph{notes:} Comments pertaining to the classification and observations of 
#'     the sonar imagery within each polygon.}
#'     \item{\emph{rkm:} The river kilometer of each polygon. The rivermouth is at rkm 0 
#'    and rkm increases in the upriver direction.}
#'    \item{\emph{rkmZone:} The 5 km section of river a polygon is within. This field is
#'     used to group polygons for summary and visualization of the classified and
#'    reclassified substrate types.}
#'     \item{\emph{geometry:} The global positioning system coordinates recorded at a 
#'     point, as UTM X and Y for WGS 84 / UTM zone 17N.}
#'   }
#'   \item{\strong{gtSSS:} A point shapefile of ground-truthing data (gtDom_sf), with 
#'   spatial uncertainty propagated, that is spatially joined with the 
#'   side-scan sonar polygon layer (sonarClass).}
#'   \itemize{
#'    \item{\emph{sampleID:} Unique identifier for each ground-truthing observation.}
#'     \item{\emph{drawID:} Unique identifier for each set of ground-truthing point 
#'     location realizations, drawn from the global positioning system error 
#'     distribution.}
#'     \item{\emph{gtPatch:} Dominant substrate (composed over 60% of substrate 
#'     composition) at survey location. Substrate types are based on the Wentworth
#'     scale and include bedrock, boulder, cobble, pebble, gravel, sand, and
#'     silt/clay.}
#'    \item{\emph{sssSampleID:} Unique identifier for each classified polygon. Polygon 
#'     substrate classifications were manually digitized and classified from 
#'    side-scan sonar imagery.}
#'    \item{\emph{sssPatch:} Dominant substrate type within a digitized polygon. 
#'     Substrates classified were boulder, cobble, pebble, sand, silt/clay, 
#'     and other (e.g., tires, schools of fish, large woody debris). Gravel 
#'     was not classified, due to the difficulty distinguishing gravel from sand 
#'     and pebble.}
#'     \item{\emph{area_m2:} The area of a polygon in square meters.}
#'     \item{\emph{notes:} Comments pertaining to the classification and observations of 
#'     the sonar imagery within each polygon.}
#'     \item{\emph{rkm:} The river kilometer of each polygon. The rivermouth is at rkm 0 
#'     and rkm increases in the upriver direction.}
#'     \item{\emph{rkmZone:} The 5 km section of river a polygon is within. This field is
#'     used to group polygons for summary and visualization of the classified and
#'     reclassified substrate types.}
#'     \item{\emph{geometry:} The global positioning system coordinates recorded at a 
#'     point, as UTM X and Y for WGS 84 / UTM zone 17N.}
#'   }
#' }
"substrateLayers"