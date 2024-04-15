#######################################
# Function: 4_Create_additional_files-LDNDC.R
# Author: Ellen Maas
# Date: Sept 23, 2022
# Description: Composes XML files with information to direct LDNDC which
# models to use for internal processing and the names of input files with
# site-specific data.
#######################################
# Output
# Setup file: setup.xml: Names models to use within LDNDC for soil
#             system processing. Also directs which output sets to
#             provide and the timestep.
# Project file: *.ldndc: Names the input files for each scenario.
# Batch file:
#######################################
# Audit Log
# 9/23/2022: Created file.
# 2/6/2023: Modified with updates to the general processing regime with
# scenario-named files. Also removed experimental-period-only sections
# so that there is just one process that goes through 2100. This is 
# in line with all other models (except Daycent).
#######################################

print("Starting 4_Create_additional_files-LDNDC.R")

library(xml2)

dndc_setup_filename <- paste0(dndc_path,"setup.xml")
dndc_project_filename <- paste0(dndc_path,site_name,"_",scenario_name2,".ldndc")

print(dndc_project_filename)

dndc_speciesparams_filename <- paste0(dndc_path,"speciesparameters.xml")
dndc_airchem_filename <- paste0(dndc_path,"airchem.txt")
dndc_batch_filename <- paste0(dndc_path,site_name,"_",scenario_name2,".bat")
dndc_shell_filename <- paste0(dndc_path,site_name,"_",scenario_name2,".sh")
dndc_callshell_filename <- paste0(dndc_path,"callsh_",site_name,"_",scenario_name2,".sh")


#*************************************************************
# setup file --------------------------------------------------------------

unlink(dndc_setup_filename)

doc_setup <- read_xml(paste0(
  "<?xml version=\"1.0\" ?>",
  "<ldndcsetup>",
  "<setup id=\"",site_id,"\" name=\"",site_name,"\" >",
  "<location elevation=\"",elevation_m,"\" latitude=\"",latitude,"\" longitude=\"",longitude,"\" />",
  "<models>",
  "<model id=\"_MoBiLE\" />",
  "</models>",
  "<mobile>",
  "<modulelist>",
  "<module id=\"microclimate:canopyecm\" timemode=\"subdaily\" />",
  "<module id=\"watercycle:watercycledndc\" timemode=\"subdaily\" />",
  "<module id=\"airchemistry:airchemistrydndc\" timemode=\"daily\" />",
  "<module id=\"physiology:plamox\" timemode=\"subdaily\" />",
  "<module id=\"soilchemistry:metrx\" timemode=\"subdaily\" />",
  "<!-- outputs -->",
  "<module id=\"output:microclimate:daily\" />",
  "<module id=\"output:watercycle:daily\" />",
  "<module id=\"output:physiology:daily\" />",
  "<module id=\"output:soilchemistry:daily\" />",
  "<module id=\"output:soilchemistry:yearly\" />",
  "<module id=\"output:report:arable:harvest\" timemode=\"subdaily\" />",
  "</modulelist>",
  "</mobile>",
  "</setup>",
  "</ldndcsetup>"
))

write_xml(doc_setup,file=dndc_setup_filename)


#*************************************************************
# project file (.ldndc) --------------------------------------------------------------

unlink(dndc_project_filename)

# set the treatment details (soil and management) the scenario is based on
# base_treatment <- ifelse(mgmt_scenario_grp %in% c(1,4,5,6),1,mgmt_scenario_grp)

doc_proj <- read_xml(paste0("<?xml version=\"1.0\" ?><ldndcproject PackageMinimumVersionRequired=\"1.35.2\">",
                 paste0('<schedule time=\"',experiment_start_date,'/24 -> ',max_fut_period_year,'-12-31\" />'),
                 #paste0('<schedule time=\"1989-01-01/24 -> 2075-12-31\" />'),
                 "<input>",
                 paste0('<sources sourceprefix=\"',site_name,'/" >'), 
                 "<setup source=\"setup.xml\" />",
                 paste0('<site source=\"', site_name, "_soil",'.xml\" />'), # includes soil characteristics, changed from "base_treatment"
                 "<airchemistry source=\"airchem.txt\" format=\"txt\" />",
                 paste0('<climate source=\"climate_',clim_scenario_num,'.txt\" />'),
                 paste0('<event source=\"mana_',scenario_name2,'.xml\" />'),
                 "<speciesparameters source=\"speciesparameters.xml\" />",
                 "</sources>",
                 '<attributes use=\"0\" endless=\"0\" >',
                 "<airchemistry endless=\"yes\" />",
                 "<climate endless=\"no\" />",
                 "</attributes>",
                  "</input>",
                  "<output>",
                 paste0('<sinks sinkprefix=\"',site_name,'/',site_name,'_output/" >'),
                 paste0("<soilchemistrydaily sink=\"soil_chem_daily_",scenario_name2,".csv\" format=\"txt\" delimiter=\",\" />"),
                 paste0("<soilchemistryyearly sink=\"soil_chem_yearly_",scenario_name2,".csv\" format=\"txt\" delimiter=\",\" />"),
                 paste0("<microclimatedaily sink=\"soil_temp_daily_",scenario_name2,".csv\" format=\"txt\" delimiter=\",\" />"),
                 paste0("<physiologydaily sink=\"physiology_daily_",scenario_name2,".csv\" format=\"txt\" delimiter=\",\" />"),
                 paste0("<arablereportharvest sink=\"harvest_",scenario_name2,".csv\" format=\"txt\" delimiter=\",\" />"),
                 paste0("<watercycledaily sink=\"soil_water_daily_",scenario_name2,".csv\" format=\"txt\" delimiter=\",\" />"),
                 paste0("<metrxdaily sink=\"metrx-daily_",scenario_name2,".csv\" format=\"txt\" delimiter=\",\" />"),
                 "</sinks>",
                  "</output>",
                 "</ldndcproject>"))

#print(x)

write_xml(doc_proj,file=dndc_project_filename)
          

#*************************************************************
#*## air chemistry file

unlink(dndc_airchem_filename)

airchem_txt <- c("nh4\t no3\t ch4\t co2\t o3\t nh3\t no2\t no",
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t"),
                 paste0("0.3\t 0.3\t 0.883\t 353.0\t 0.0\t 0.0\t 0.0\t 0.0\t")
)

writeLines(airchem_txt,dndc_airchem_filename)


#*************************************************************
# species parameters --------------------------------------------------------------

unlink(dndc_speciesparams_filename)

if(identical(crop, "Soybean")){
  doc_specparam <- read_xml(paste0(
    "<ldndcspeciesparameters>",
    "<speciesparameters>",
    "<species group=\"crop\" mnemonic=\"soyb\">",
    "<par name=\"gdd_base_temperature\" value=\"7\" />", #7
    "<par name=\"gdd_maturity\" value=\"1819\" />", #default=1819
    "<par name=\"gdd_grain_filling\" value=\"1000\" />", #default=est. at 1000 # AD changed from -1
    "<par name=\"fraction_foliage\" value=\"0.43\" />", #default=0.43
    "<par name=\"fraction_fruit\" value=\"0.4\" />", #default=0.4
    "<par name=\"fraction_root\" value=\"0.06\" />", #default=0.06
    "<par name=\"fyield\" value=\"0.25\" />", #default=0.25 # AD changed from .16
    "<par name=\"slamax\" value=\"20\" />", #default=20
    "<par name=\"vcmax25\" value=\"125.6\" />", #default=125.6
    "<par name=\"h2oref_a\" value=\"0.5\" />", #default=0.5
    "</species>",
    "</speciesparameters>",
    "</ldndcspeciesparameters>"
  ))
}

if(identical(crop, "Maize")){
  print("Maize")
  
  doc_specparam <- read_xml(paste0(
    "<ldndcspeciesparameters>",
    "<speciesparameters>",
    "<species group=\"crop\" mnemonic=\"foco\">",
    "<par name=\"gdd_base_temperature\" value=\"7\" />", #7
    "<par name=\"gdd_maturity\" value=\"1819\" />", #default=1819
    "<par name=\"gdd_grain_filling\" value=\"1000\" />", #default=est. at 1000 # AD changed from -1
    "<par name=\"fraction_foliage\" value=\"0.43\" />", #default=0.43
    "<par name=\"fraction_fruit\" value=\"0.4\" />", #default=0.4
    "<par name=\"fraction_root\" value=\"0.06\" />", #default=0.06
    "<par name=\"fyield\" value=\"0.25\" />", #default=0.25 # AD changed from .18
    "<par name=\"slamax\" value=\"20\" />", #default=20
    "<par name=\"vcmax25\" value=\"125.6\" />", #default=125.6
    "<par name=\"h2oref_a\" value=\"0.5\" />", #default=0.5
    "</species>",
    "</speciesparameters>",
    "</ldndcspeciesparameters>"
  ))
}

if(identical(crop, "Wheat")){
  doc_specparam <- read_xml(paste0(
    "<ldndcspeciesparameters>",
    "<speciesparameters>",
    "<species group=\"crop\" mnemonic=\"wiwh\">",
    "<par name=\"gdd_base_temperature\" value=\"7\" />", #7
    "<par name=\"gdd_maturity\" value=\"1819\" />", #default=1819
    "<par name=\"gdd_grain_filling\" value=\"1000\" />", #default=est. at 1000 # AD changed from -1
    "<par name=\"fraction_foliage\" value=\"0.43\" />", #default=0.43
    "<par name=\"fraction_fruit\" value=\"0.4\" />", #default=0.4
    "<par name=\"fraction_root\" value=\"0.06\" />", #default=0.06
    "<par name=\"fyield\" value=\"0.25\" />", #default=0.25 # AD changed from .18
    "<par name=\"slamax\" value=\"20\" />", #default=20
    "<par name=\"vcmax25\" value=\"125.6\" />", #default=125.6
    "<par name=\"h2oref_a\" value=\"0.5\" />", #default=0.5
    "</species>",
    "</speciesparameters>",
    "</ldndcspeciesparameters>"
  ))
}


if(identical(crop, "Cotton")){
  doc_specparam <- read_xml(paste0(
    "<ldndcspeciesparameters>",
    "<speciesparameters>",
    "<species group=\"crop\" mnemonic=\"cott\">",
    "<par name=\"gdd_base_temperature\" value=\"7\" />", #7
    "<par name=\"gdd_maturity\" value=\"1819\" />", #default=1819
    "<par name=\"gdd_grain_filling\" value=\"1000\" />", #default=est. at 1000 # AD changed from -1
    "<par name=\"fraction_foliage\" value=\"0.43\" />", #default=0.43
    "<par name=\"fraction_fruit\" value=\"0.4\" />", #default=0.4
    "<par name=\"fraction_root\" value=\"0.06\" />", #default=0.06
    "<par name=\"fyield\" value=\"0.25\" />", #default=0.25 # AD changed from .18
    "<par name=\"slamax\" value=\"20\" />", #default=20
    "<par name=\"vcmax25\" value=\"125.6\" />", #default=125.6
    "<par name=\"h2oref_a\" value=\"0.5\" />", #default=0.5
    "</species>",
    "</speciesparameters>",
    "</ldndcspeciesparameters>"
  ))
}

if(identical(crop, "Rotation")){
  doc_specparam <- read_xml(paste0(
    "<ldndcspeciesparameters>",
    "<speciesparameters>",
    "<species group=\"crop\" mnemonic=\"foco\">",
    "<par name=\"gdd_base_temperature\" value=\"7\" />", #7
    "<par name=\"gdd_maturity\" value=\"1819\" />", #default=1819
    "<par name=\"gdd_grain_filling\" value=\"1000\" />", #default=est. at 1000 # AD changed from -1
    "<par name=\"fraction_foliage\" value=\"0.43\" />", #default=0.43
    "<par name=\"fraction_fruit\" value=\"0.4\" />", #default=0.4
    "<par name=\"fraction_root\" value=\"0.06\" />", #default=0.06
    "<par name=\"fyield\" value=\"0.25\" />", #default=0.25 # AD changed from .18
    "<par name=\"slamax\" value=\"20\" />", #default=20
    "<par name=\"vcmax25\" value=\"125.6\" />", #default=125.6
    "<par name=\"h2oref_a\" value=\"0.5\" />", #default=0.5
    "</species>",
    "<species group=\"crop\" mnemonic=\"soyb\">",
    "<par name=\"gdd_base_temperature\" value=\"7\" />", #7
    "<par name=\"gdd_maturity\" value=\"1819\" />", #default=1819
    "<par name=\"gdd_grain_filling\" value=\"1000\" />", #default=est. at 1000 # AD changed from -1
    "<par name=\"fraction_foliage\" value=\"0.43\" />", #default=0.43
    "<par name=\"fraction_fruit\" value=\"0.4\" />", #default=0.4
    "<par name=\"fraction_root\" value=\"0.06\" />", #default=0.06
    "<par name=\"fyield\" value=\"0.25\" />", #default=0.25 # AD changed from .18
    "<par name=\"slamax\" value=\"20\" />", #default=20
    "<par name=\"vcmax25\" value=\"125.6\" />", #default=125.6
    "<par name=\"h2oref_a\" value=\"0.5\" />", #default=0.5
    "</species>",
    "</speciesparameters>",
    "</ldndcspeciesparameters>"
  ))
}


write_xml(doc_specparam,file=dndc_speciesparams_filename)

#*************************************************************
# write batch and shell files --------------------------------------------------------------

unlink(dndc_batch_filename)
unlink(dndc_shell_filename)
unlink(dndc_callshell_filename)

## write Windows batch and Linux shell files

# batch file
batch_txt <- paste0("%cd%\\..\\..\\bin\\ldndc.exe ",site_name,"_",mgmt_scenario_num,".ldndc")
                    
writeLines(batch_txt,dndc_batch_filename)

#-------------------
  
# shell file (runs LDNDC)
shell_txt <- c("#!/bin/bash",
  "",
  "if [ -e $HOME/.ldndc ]; ",
  "then",
  "#LandscapeDNDC program",
  "ldndc=\"../../bin/ldndc\"",
  "",
  "#Target project",
  paste0("project=\"./", site_name, "_", scenario_name2,".ldndc\""),
  "",
  "#Run target project",
  "$ldndc $project",
  "else",
  "printf \"Directory \\\"~/.ldndc\\\" missing!\n\\",
  "Did you install LandscapeDNDC via \\\"install.sh\\\"?\n\"",
  "fi")

writeLines(shell_txt,dndc_shell_filename)

# make the file executable
system(paste0("chmod u+x ",dndc_shell_filename))

#------------------
  
# callsh shell file (calls the shell file above (which runs LDNDC) and captures
# the output)
callsh_txt <- paste0("./", site_name, "_", scenario_name2,".sh > mylog.txt 2>&1")

writeLines(callsh_txt,dndc_callshell_filename)

# make the file executable
system(paste0("chmod u+x ",dndc_callshell_filename))




#*************************************************************
# Clean up ----------------------------------------------------------------

rm(dndc_setup_filename,dndc_project_filename,#dndc_speciesparams_filename,
   dndc_airchem_filename,dndc_batch_filename,dndc_shell_filename,
   dndc_callshell_filename,doc_setup,doc_proj,doc_specparam, # AD removed base_treatment
   batch_txt,shell_txt,callsh_txt)


