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
    results_folder<-'/home/ap/Documents/national_gwp_results'
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

daycent_input_data<-list.dirs(daycent_input_folder)
daycent_input_data<-daycent_input_data[grepl("GEOID", daycent_input_data)]

ldndc_input_data<-list.dirs(ldndc_input_folder)
ldndc_input_data<-ldndc_input_data[grepl("GEOID", ldndc_input_data)]

mill_input_data<-list.dirs(mill_input_folder)
mill_input_data<-mill_input_data[grepl("GEOID", mill_input_data)]

results_data<-results_data<-list.dirs(results_folder)
results_data<-results_data[grepl("Results_GEOID_", results_data)]

##################################################
# CAREFUL WITH THIS
##################################################
delete_data<-c(results_data, daycent_input_data, ldndc_input_data, mill_input_data)
# delete_data<-c(daycent_input_data) 
##################################################
# CAREFUL WITH THIS
##################################################

print("***** DELETING FILES *********")

for (i in 1:length(delete_data)){
  if (file.exists(delete_data[i])) {
    unlink(delete_data[i],recursive = TRUE)
    cat(paste0(delete_data[i], " has been deleted\n"))
  }
}
# Double check
input_data<-list.dirs(daycent_input_folder)
input_data<-input_data[grepl("GEOID", input_data)]
print(input_data)
results_data<-results_data<-list.dirs(results_folder)
results_data<-results_data[grepl("Results_GEOID_", results_data)]
print(results_data)