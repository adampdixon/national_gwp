######################################################
#  Delete all the input data folders as they are not needed and troublesome during model reruns
# for Daycent 
# A Dixon
# Jan 7, 2024
###########################

daycent_input_data<-list.dirs('/home/ap/Documents/GitHub/national_gwp/Daycent')
daycent_input_data<-daycent_input_data[grepl("GEOID", daycent_input_data)]

results_data<-results_data<-list.dirs('/home/ap/Documents/GitHub/national_gwp')
results_data<-results_data[grepl("Results_GEOID_", results_data)]
# CAREFUL WITH THIS
delete_data<-c(daycent_input_data, results_data)
# delete_data<-c(daycent_input_data)
# CAREFUL WITH THIS

for (i in 1:length(delete_data)){
  if (file.exists(delete_data[i])) {
    unlink(delete_data[i],recursive = TRUE)
    cat(paste0(delete_data[i], " has been deleted\n"))
  }
}
# Double check
input_data<-list.dirs('/home/ap/Documents/GitHub/national_gwp/Daycent')
input_data<-input_data[grepl("GEOID", input_data)]
print(input_data)
results_data<-results_data<-list.dirs('/home/ap/Documents/GitHub/national_gwp')
results_data<-results_data[grepl("Results_GEOID_", results_data)]
print(results_data)