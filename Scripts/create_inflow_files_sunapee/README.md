#### README for create_inflow_files_sunapee

This folder contains scripts that create inflow data for Lake Sunapee. This includes inflow discharge, stream water temperature, and nutrient chemistry for all stream sites. The scripts should be processed in this order:

1. *inflow_model_sunapee.R* this file creates modeled output of stream discharge and water temperature at all stream sites
2. *boostratp_TP.R* this script creates daily estimates of nutrient chemistry at all stream sites by bootstrapping from available nutrient chemistry at approximately XXX frequency
3. *NtoPratio.R* this script 
