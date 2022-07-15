# run scenarios and visualize
rm(list = ls())
pacman::p_load(tidyverse, lubridate, ncdf4, GLMr, glmtools)
sim_folder <- getwd()

setwd('Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/')
# Make sure the most recent version of glm3.nml is copied to glm4.nml
file.copy('glm3.nml', 'glm4.nml', overwrite = TRUE)

#setwd("/Volumes/G-DRIVE SSD/scenario_output")

getwd()

file.copy('scenario_nmls/glm3_i505.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('scenario_nmls/glm3_i510.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('scenario_nmls/glm3_i540.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")


file.copy('scenario_nmls/glm3_i665.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")


file.copy('scenario_nmls/glm3_i760.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('scenario_nmls/glm3_i788.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('scenario_nmls/glm3_i790.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('scenario_nmls/glm3_i800.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('scenario_nmls/glm3_i805.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('scenario_nmls/glm3_i830.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")

file.copy('scenario_nmls/glm3_i835.nml', 'glm3.nml', overwrite = TRUE)
system2("/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS/glm", stdout = TRUE, stderr = TRUE, env = "DYLD_LIBRARY_PATH=/Users/jacobwynne/Dropbox/SUNP-GLMv3.3-JHW/glm.app/Contents/MacOS")


file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)


