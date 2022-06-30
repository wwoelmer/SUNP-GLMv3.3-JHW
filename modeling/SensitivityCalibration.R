#*****************************************************************                                                           *
#* TITLE:   Sensitivity and numerical optimization calibration script
#*           for GLM-AED
#* AUTHORS:  R. Ladwig and C.C. Carey                    
#* DATE:   Originally developed by R. Ladwig in 2018 and given to CCC in 2019; 
#*         Last modified by CCC in 9 Sept 2021                            
#* NOTES:  CCC modified the original script in 2019 for FCR modeling, 
#*        with subsequent tweaks to annotation in summer 2021. 
#*****************************************************************


#scripts for formatting create_noon_oxy_file_obs

rm(list = ls()) #let's clean up that workspace!

setwd("~/Dropbox/SUNP-GLMv3.3-JHW/")
#setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta") #if pulling from github, sets it to proper wd
source('modeling/functions-glm.R') #source the helper functions
read.packages() 

# RUN GLM AND READ RESULTS  ---------------------------
filename = 'SNP'
out = 'output/output.nc' 
sim_vars(out)

sim_folder<-getwd()

#run_glm('Compiled') #using Cayelan's GLM-AED dynamic libraries in this repo


plot_temp(out, col_lim = c(0,30))

plot_var(file=out,"OXY_sat",reference="surface")

# GET FIELD DATA FOR CALIBRATION AND VALIDATION  ---------------------------
# WTR AND OXY DATA
field_temp<-read.csv("data/formatted-data/field_temp_noon_obs.csv", header=T)
field_oxy <-read.csv("data/formatted-data/manual_buoy_oxy.csv", header=T)
field_temp$DateTime <-as.POSIXct(strptime(field_temp$DateTime, "%Y-%m-%d", tz="EST"))
field_oxy$DateTime <-as.POSIXct(strptime(field_oxy$DateTime, "%Y-%m-%d", tz="EST"))

# CHEMISTRY: Only have TP for Sunapee
chem <- read.csv('data/formatted-data/field_obs_TP.csv', header=T)
chem$DateTime <-as.POSIXct(strptime(chem$DateTime, "%Y-%m-%d", tz="EST"))



#######################################################
# RUN SENSITIVITY ANALYSIS  ---------------------------
# 1) water temperature
#first, copy & paste your glm3.nml and aed2.nml within their respective directories
# and rename as glm4.nml and aed4.nml; these 4.nml versions are going to be rewritten
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed4.nml', 'aed/aed.nml', overwrite = TRUE)
var = 'temp'

#build a matrix with all potential parameters for sensitivity analysis here
calib <- matrix(c('par', 'lb', 'ub', 'x0', 
                  'wind_factor', 0.75, 1.25, 1,
                  'sw_factor', 0.75, 1.25, 1,
                  'lw_factor', 0.75, 1.25, 1,
                  'coef_mix_conv', 0.1, 0.5, 0.2,
                  'coef_wind_stir', 0.1, 0.5, 0.23,
                  'coef_mix_shear', 0.1, 0.6, 0.3,
                  'coef_mix_turb', 0.2, 0.8, 0.51,
                  'coef_mix_KH', 0.1, 0.6, 0.3,
                  'coef_mix_hyp', 0.2, 0.8, 0.5,
                  'ce', 0.0005, 0.002, 0.0013,
                  'ch', 0.0005, 0.002, 0.0013,
                  'cd', 0.0005, 0.002, 0.0013,
                  'zone_heights', 0.1,9.5,5,
                  'zone_heights', 0.1,9.5,9,
                  'rain_factor', 0.75, 1.25, 1,
                  'at_factor', 0.75, 1.25, 1,
                  'rh_factor', 0.75, 1.25, 1,
                  'sed_temp_mean',3,20,11,
                  'sed_temp_mean',3,20,17,
                  'sed_temp_amplitude',2,12,6,
                  'sed_temp_amplitude',2,12,6,
                  'sed_temp_peak_doy',250,280,272,
                  'sed_temp_peak_doy',250,280,272), nrow = 24,ncol = 4, byrow = TRUE) 
#Be sure to edit the nrow value if you decrease the number of rows in matrix above
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs("data/formatted-data/field_temp_noon_obs.csv", var)
obs$Depth <- round(obs$Depth)
nml_file = 'glm3.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)

# 2) dissolved oxygen
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed4.nml', 'aed/aed.nml', overwrite = TRUE)
var = 'OXY_oxy'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Fsed_oxy', -2, -40, -21,
                  'Fsed_oxy', -2, -40, -21,
                  'Ksed_oxy', 1, 15, 7,
                  'theta_sed_oxy', 0.8, 1.2, 1.08,
                  'Rdom_minerl', 0.0001, 0.10, 0.001,
                  'Rpom_hydrol', 0.0001, 0.10, 0.03,
                  'Rdomr_minerl', 0.00001,0.10,0.0001,
                  'Rcpom_bdown', 0.0001, 0.10, 0.001), nrow = 9, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
#not sure what field_FCR.csv contains, assuming temp for now  
#obs <- read_field_obs('field_data/field_FCR.csv', var)
obs <- read_field_obs("data/formatted-data/field_temp_oxy_noon_obs.csv", var)
nml_file = 'aed/aed.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)



# 6) phosphorus
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed4.nml', 'aed/aed.nml', overwrite = TRUE)
var = 'PHS_frp'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Fsed_frp', 0.0001,0.5, 0.001,
                  'Ksed_frp', 1, 150, 10,
                  'theta_sed_frp', 1, 1.15, 1.08), nrow = 4, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_chem.csv', var)
obs <- completeFun(obs, 'PHS_frp')
nml_file = 'aed/aed.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)



# 8) chlorophyll a
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed4.nml', 'aed/aed.nml', overwrite = TRUE)
var = 'PHY_TCHLA'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'pd%w_p', -0.1, 0.10, -0.001,
                  'pd%Xcc', 30, 500, 100,
                  'pd%R_growth', 0.3, 2.0, 1.1,
                  'pd%theta_growth', 1.02, 1.2, 1.08, #causes it to bomb
                  'pd%T_std', 15, 35, 20, #causes it to bomb
                  'pd%T_opt', 15, 35, 27, #causes it to bomb
                  'pd%T_max', 25, 42, 37, #causes it to bomb
                  'pd%I_K', 10, 400, 50, #causes it to bomb
                  'pd%I_S', 10, 500, 250,
                  'pd%KePHY', 0.001, 0.1, 0.0045, #causes it to bomb
                  'pd%f_pr', 0.001, 0.95, 0.05,
                  'pd%R_resp', 0.001, 0.5, 0.02,
                  'pd%theta_resp', 1.01, 1.15, 1.1, #causes it to bomb
                  'pd%k_fres', 0.01, 1, 0.8,
                  'pd%k_fdom', 0.01, 0.9, 0.1,
                  'pd%K_N', 0.001, 10, 1.5, #causes it to bomb
                  'pd%X_ncon', 0.01, 1, 0.035,
                  'pd%X_nmin', 0, 1, 0.07,
                  'pd%X_nmax', 0.01, 1, 0.2,
                  'pd%R_nuptake', 0.001, 1, 0.06,
                  'pd%K_P', 0.001, 1.5, 0.6,
                  'pd%X_pcon', 0.0001, 0.1, 0.0015,
                  'pd%X_pmin', 0, 0.1, 0.002,
                  'pd%X_pmax', 0.001, 1, 0.05,
                  'pd%R_puptake', 0.0001, 0.1, 0.004), nrow =26, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_obs_chla.csv', var)
obs <- completeFun(obs, 'PHY_TCHLA')
nml_file = 'aed/aed.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)



# START CALIBRATION  ---------------------------
# 1) water temperature
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed4.nml', 'aed/aed.nml', overwrite = TRUE)
var = 'temp'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (c(1,4,5,7,11,13,14,265,275,275) - lb) *10 /(ub-lb) # NEEDS TO BE UPDATED WITH STARTING VALUES FROM YOUR CALIBRATION FILE
obs <- read_field_obs('field_data/field_temp_noon_obs.csv', var)  
# obs1 <- obs %>% 
#   filter(DateTime> as_date("2015-07-06"),
#          DateTime< as_date("2020-01-01"))
#   Sys.setenv(TZ = “EST”)
method = 'cmaes'
calib.metric = 'RMSE'
os = 'Compiled' 
target_fit = -Inf#1.55
target_iter = 1000 
nml_file = 'glm3.nml'
run_calibvalid(var, var_unit = 'degreesC', var_seq = seq(-5,35,1), cal_pars, pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c()) #var_seq is contour color plot range


#to visualize how parameters are optimized during calibration
# par(mfrow=c(3,4))
# data<-read.csv("results/calib_results_RMSE_temp.csv", header=T)
# temp<-seq(1,length(data$DateTime),1)
# for(i in 1:length(init.val)){
#   plot(temp,data[,i+1],type="l",xlab="Iteration", ylab=(colnames(data[i+1])))
# }


# 2) dissolved oxygen
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed4.nml', 'aed/aed.nml', overwrite = TRUE)
var = 'OXY_oxy'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (c(-30,-10,-1) - lb) *10 /(ub-lb) # Paul's values
#obs <- read_field_obs('field_data/field_FCR.csv', var)
obs <- read_field_obs('field_data/manual_buoy_oxy.csv',var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled" 
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed/aed.nml'
run_calibvalid(var, var_unit = 'mmol/m3', var_seq = seq(0,600,50), cal_pars, pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 6) phosphate
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed4.nml', 'aed/aed.nml', overwrite = TRUE)
var = 'PHS_frp'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(0.011, 0.2, 25) - lb) *10 /(ub-lb) 
obs <- read_field_obs('field_data/field_obs_TP.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed/aed.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,0.5,0.05), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())



# 8a) Secchi
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
nml_file = 'aed/aed.nml'
var = 'extc_coef'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'KeDOM',0.00001,0.5,0.0025,
                  'KePOM',0.00001,0.5,0.0015,
                  'KeDOMR', 0.00001, 0.5, 0.01), nrow = 4, ncol = 4, byrow = TRUE)
cal_pars = data.frame(calib)
colnames(cal_pars) <- as.character(unlist(cal_pars[1,]))
cal_pars <- cal_pars[-1,]
#Reload ub, lb for calibration
pars <- as.character(cal_pars$par)
ub <- as.numeric(levels(cal_pars$ub))[cal_pars$ub] 
lb <- as.numeric(levels(cal_pars$lb))[cal_pars$lb] 
cal_pars$x0  <- as.numeric(levels(cal_pars$x0))[cal_pars$x0] 
#Create initial files
init.val <- (cal_pars$x0 - lb) *10 /(ub-lb)
obs <- read_field_obs('field_data/field_obs_secchi.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed/aed.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,1,10), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 9) chlorophyll- my attempt!
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed4.nml', 'aed/aed.nml', overwrite = TRUE)
var = 'PHY_TCHLA'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(0.01) - lb) *10 /(ub-lb) 
obs <- read_field_obs('field_data/field_obs_chla.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed/aed.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,100,10), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())

