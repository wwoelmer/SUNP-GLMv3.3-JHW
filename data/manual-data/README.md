# Lake Sunapee Protective Association's Longterm Monitoring Program data repository

This repository contains the [Lake Sunapee Protective Association (LSPA)](https://www.lakesunapee.org/) Longterm Monitoring Program (LMP) [historical raw data](https://github.com/Lake-Sunapee-Protective-Association/LMP/tree/main/raw%20data%20files), [collation and QAQC scripts](https://github.com/Lake-Sunapee-Protective-Association/LMP/tree/main/collation%20code), as well as the harmonized post-QAQC [master files](https://github.com/Lake-Sunapee-Protective-Association/LMP/tree/main/master%20files) for all data collected by the LSPA in the watershed 1986-2020 as reported in the LMP files. The raw data were made available by the LSPA (lspa@lakesunapee.org) and are archived by the New Hampshire Volunteer Lake Assessment Program (NH VLAP). The foci of this repository are the collation of and the QAQC of the raw LMP data. QAQC in this context only includes the recoding of obviously errant data to *NA* and the addition of use flags to the dataset. The resulting 'master files' are meant for use by researchers who collaborate in the Lake Sunapee Watershed.

These data use the ODC Open Database License v1.0, see *[ODC license.txt](https://github.com/Lake-Sunapee-Protective-Association/LMP/blob/main/odc%20license.txt)* for details on use and reuse. 

Please cite these data using the Zenodo DOI associated with this repository.

#### Contacts: 

code and repository questions: B. Steele - steeleb@caryinstitute.org, Kathleen Weathers -weathersk@caryinstitute.org

data and sampling methodology questions: Lake Suanpee Protective Association - lspa@lakesunapee.org

This repository is maintained by B. Steele, Weathers Lab, Cary Institute of Ecosystem Studies (steeleb@caryinstitute.org). 

***

# Lake Sunapee Watershed and Sampling sites

Below is a map of Lake Sunapee, the lake's watershed (shaded green), and the sampling sites referenced in these data. Other waterbodies within the watershed are identified, as well. 

![Lake Sunapee Watershed and Sampling sites](https://github.com/Lake-Sunapee-Protective-Association/LMP/blob/main/master%20files/LMP%20sampling%20map%20v2.jpg)

The location details, as well as summaries of the available data, are listed in the [master files](https://github.com/Lake-Sunapee-Protective-Association/LMP/tree/main/master%20files) folder.

***

# Data organization

How files are organized within this repository


## [collation code](https://github.com/Lake-Sunapee-Protective-Association/LMP/tree/main/collation%20code)

This folder contains the code used to collate and QAQC the data (where needed) to recode obviously errant data. Data from multiple files in the 'raw data files' folder are collated in this script.

## [master files](https://github.com/Lake-Sunapee-Protective-Association/LMP/tree/main/master%20files)

These are the harmonized, collated data that should be used by other researchers. Proper citation is required. See folder for additional description of files available.

## [raw data files](https://github.com/Lake-Sunapee-Protective-Association/LMP/tree/main/raw%20data%20files)

This folder contains the original uncollated data files from the LSPA that are organized for integration into the NH Volunteer Lake Assessment Program (VLAP) database. Files have come directly from the LSPA and may contain QAQC errors. These data are stored here for the purposes of transparency and processing, please use the files in the [master files](https://github.com/Lake-Sunapee-Protective-Association/LMP/tree/main/master%20files) folder instead of these. For questions about the QAQC process from the raw files, please contact Bethel Steele (steeleb@caryinstitute.org) for information. All QAQC of these files are performed in the R scripts found in the 'collation code' folder.

These data were obtained by request from the LSPA and are archived at the New Hampshire Department of Environmental Services (NHDES) Environmental Monitoring Database (data are also available upon request from the NHDES).

***

# Data status

The monitoring and data acquisition is ongoing, this repostitory will be updated approximately annually with additional data.
