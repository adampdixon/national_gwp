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

dndc_setup_filename <- paste0(dndc_path,"/setup.xml")
dndc_project_filename <- paste0(dndc_path,"/",site_name,".ldndc")
dndc_airchem_filename <- paste0(dndc_path,"/airchem.txt")
dndc_batch_filename <- paste0(dndc_path,"/",site_name,".bat")


#*************************************************************
## setup file

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
  "<module id=\"airchemistry:airchemistrydndc\" timemode=\"daily\" />",
  "<module id=\"microclimate:canopyecm\" timemode=\"daily\" />",
  "<module id=\"physiology:arabledndc\" timemode=\"daily\" />",
  "<module id=\"soilchemistry:metrx\" timemode=\"daily\" />",
  "<module id=\"watercycle:watercycledndc\" timemode=\"daily\" />",
  "<!-- outputs -->",
  "<module id=\"output:soilchemistry-layer:daily\" />",
  "<module id=\"output:soilchemistry:yearly\" />",
  "<module id=\"output:microclimate:daily\" />",
  "<module id=\"output:physiology:daily\" />",
  "<module id=\"output:watercycle:daily\" />",
  "<module id=\"output:report:arable:harvest\" timemode=\"daily\" />",
  "</modulelist>",
  "</mobile>",
  "</setup>",
  "</ldndcsetup>"
))

write_xml(doc_setup,file=dndc_setup_filename)


#*************************************************************
## project file (.ldndc)

# set the treatment details (soil and management) the scenario is based on
base_treatment <- ifelse(mgmt_scenario_grp %in% c(1,4,5,6),1,mgmt_scenario_grp)

doc_proj <- read_xml(paste0("<?xml version=\"1.0\" ?><ldndcproject PackageMinimumVersionRequired=\"1.35.2\">",
                 paste0('<schedule time=\"',experiment_start_date,'/1 -> ',experiment_end_date,'\" />'),
                  "<input>",
                 paste0('<sources sourceprefix=\"',site_name,'/" >'),
                 "<setup source=\"setup.xml\" />",
                 paste0('<site source=\"site_',base_treatment,'.xml\" />'),
                 "<airchemistry source=\"airchem.txt\" format=\"txt\" />",
                 paste0('<climate source=\"climate_',clim_scenario_num,'.txt\" />'),
                 paste0('<event source=\"mana_',base_treatment,'.xml\" />'),
                 "</sources>",
                  "</input>",
                  "<output>",
                 paste0('<sinks sinkprefix=\"',site_name,'/',site_name,'_output/" >'),
                 "<soilchemistrydaily sink=\"soil-chem.txt\" format=\"txt\" />",
                 "<microclimatedaily sink=\"soil-temp.txt\" format=\"txt\" />",
                 "<physiologydaily sink=\"physiology.txt\" format=\"txt\" />",
                 "<harvest sink=\"harvest.txt\" format=\"txt\" />",
                 "<watercycledaily sink=\"soil-water.txt\" format=\"txt\" />",
                 "</sinks>",
                  "</output>",
                 "</ldndcproject>"))

#print(x)

write_xml(doc_proj,file=dndc_project_filename)
          

#*************************************************************
## batch file


# contents of batch file
batch_txt <- paste0("%cd%\\..\\..\\bin\\ldndc.exe ",site_name,".ldndc")
                    
writeLines(batch_txt,dndc_batch_filename)



#*************************************************************
## air chemistry file


# # output header data
# 
# DNDC_airchem_file <- paste0(dndc_path,site_name,"/",site_name,"_airchem.txt")
# 
# airchem_txt <- c("%global",
#                 paste0("        time = \"",start_date,"/1\"\n"),
#                 "%airchemistry",
#                 paste0("        id = \"",as.character(site_id),"\""),
#                 "%attributes",
#                 paste0("        co2 = \"353\"") #,
# #                "\n",
# #                "%data",
# #                "*\t *\t prec\t tavg\t tmax\t tmin\t grad\t wind"
# )
# 
# writeLines(airchem_txt,dndc_airchem_filename)
# 
# # # add data
# # write.table(DNDC_basic,sep="\t",
# #             file=DNDC_wth_file,
# #             append=TRUE,
# #             row.names = F,
# #             col.names = F)
