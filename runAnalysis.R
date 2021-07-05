processObsData <- T
processSimData <- T

# Building the folder structure
# Change the dir name based on your simulation run
simDesc <- "10pct_noMc_bicycle_v1"
outputDir <- paste0("./calibrationOutputs/", simDesc,"/")
if (!dir.exists(outputDir)) dir.create(outputDir)

# Running the observation data processing
# Set prcessObsData to false if this step is not required
# If skipping, existing processed obs data joined to the network will be used
# If skipping, make sure to have the joined data in observationsJoined dir
if(processObsData) rmarkdown::render("observation-preparation.Rmd",
                                    output_dir=outputDir, quiet = F)

# Running the Simulation data processing
# Set processSimData to false if this step is not required
# If skipping, existing processed sim data joined to the network will be used
# If skipping, make sure to have the joined data in simDataJoined dir
if(processSimData) rmarkdown::render("simOutputPorcessing.Rmd",
                                     output_dir=outputDir, quiet = F,
                                     params = list(parseXML=FALSE))

# Runing the comparison for road traffic
# This code requires both observation and simulation data processed and
# placed in the correct directory
if(roadTrafficComp) rmarkdown::render("road-traffic-comparison.Rmd",
                                     output_dir=outputDir, quiet = F)



