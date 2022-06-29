# run scenarios and visualize
pacman::p_load(tidyverse, lubridate, ncdf4, GLMr, glmtools)
sim_folder <- getwd()

# Make sure the most recent version of glm3.nml is copied to glm4.nml
file.copy('glm3.nml', 'glm4.nml', overwrite = TRUE)


# scenario 1: double P inputs in 500's watersheds

# Copy scenario 1 nml to glm3.nml
file.copy('scenario_nmls/glm3_i500.nml', 'glm3.nml', overwrite = TRUE)

system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")


# scenario 2: double P inputs in 600's watersheds
file.copy('scenario_nmls/glm3_i600.nml', 'glm3.nml', overwrite = TRUE)

system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")


# scenario 3: double P inputs in 700's watersheds
file.copy('scenario_nmls/glm3_i700.nml', 'glm3.nml', overwrite = TRUE)

system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")


# scenario 4: double P inputs in 800's watersheds
file.copy('scenario_nmls/glm3_i800.nml', 'glm3.nml', overwrite = TRUE)

system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")


# write nml file back to base nml 
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)


