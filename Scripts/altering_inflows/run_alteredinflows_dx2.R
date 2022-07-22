# run scenarios and visualize
rm(list = ls())
pacman::p_load(tidyverse, lubridate, ncdf4, GLMr, glmtools)
sim_folder <- getwd()

setwd('/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/')
# Make sure the most recent version of glm3.nml is copied to glm4.nml
file.copy('glm3.nml', 'glm4.nml', overwrite = TRUE)

#setwd("/Volumes/G-DRIVE SSD/scenario_output")

getwd()

file.copy('discharge_scenario_nml/glm3_i505_dx2.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('discharge_scenario_nml/glm3_i510_dx2.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('discharge_scenario_nml/glm3_i540_dx2.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")


file.copy('discharge_scenario_nml/glm3_i665_dx2.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")


file.copy('discharge_scenario_nml/glm3_i760_dx2.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('discharge_scenario_nml/glm3_i788_dx2.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('discharge_scenario_nml/glm3_i790_dx2.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('discharge_scenario_nml/glm3_i800_dx2.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('discharge_scenario_nml/glm3_i805_dx2.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('discharge_scenario_nml/glm3_i830_dx2.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('discharge_scenario_nml/glm3_i835_dx2.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")


file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)


