######################################################
#  Delete all the input data folders as they are not needed and troublesome during model reruns
# Comment out results files when they are needed!!!
# for Daycent, LDNDC, and Millennial
# BE CAREFUL !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# A Dixon
# Apr 24, 2024
###########################

if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    daycent_input_folder<-'/home/ap/Documents/GitHub/national_gwp/Daycent'
    ldndc_input_folder<-'/home/ap/Documents/GitHub/national_gwp/LDNDC/ldndc-1.36.linux64/projects'
    mill_input_folder<-'/home/ap/Documents/GitHub/national_gwp/Millennial/R/simulation'
    results_folder<-'/home/ap/Documents/national_gwp_results' # commenting out results for safety
    Glade=FALSE
    print("************************************")
    print("*****Using linux mint *********")
  } else {
    daycent_input_folder<-'/glade/derecho/scratch/apdixon/national_gwp/Daycent'
    ldndc_input_folder<-'/glade/derecho/scratch/apdixon/national_gwp/LDNDC/ldndc-1.36.linux64/projects'
    mill_input_folder<-'/glade/derecho/scratch/apdixon/national_gwp/Millennial/R/simulation'
    results_folder<-'/glade/derecho/scratch/apdixon/national_gwp_results'
    Glade=TRUE
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
  }
}


ld_results_data<-list.files(results_folder, pattern = 'LDNDC', recursive = T, full.names = T)
# only delete mgmt scenarios 2 to 5 for both clim scenarios
ld_results_data<-results_data[grepl("1_2_Soybean|1_3_Soybean|1_4_Soybean|1_5_Soybean|2_2_Soybean|2_3_Soybean|2_4_Soybean|2_5_Soybean", results_data)]

##################################################
# CAREFUL WITH THIS
##################################################
#results_data
delete_data<-c(ld_results_data)

##################################################
# CAREFUL WITH THIS
##################################################

print("***** DELETING FILES *********")

for (i in 1:length(delete_data)){
  if (file.exists(delete_data[i])) {
    file.remove(delete_data[i])
    cat(paste0(delete_data[i], " has been deleted\n"))
  }
}
# Double check
input_data<-list.dirs(daycent_input_folder)
input_data<-input_data[grepl("GEOID", input_data)]
print(input_data)
# results_data<-results_data<-list.dirs(results_folder)
# results_data<-results_data[grepl("Results_GEOID_", results_data)]
# print(results_data)