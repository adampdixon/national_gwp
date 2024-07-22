
# Set workspace
if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    master_path<-'/home/ap/Documents/GitHub/national_gwp'
    results_folder<-'/home/ap/Documents/national_gwp_results'
    county_number<-1
    Test <- TRUE # if TRUE, only run county, filtered below
    # crop<- "Maize"   #Maize #Soybeans", "Wheat", "Cotton
    Glade=FALSE
    print("************************************")
    print("*****Using linux mint *********")
    cat("date and time are ")
    print(Sys.time())
  } else {
    master_path<-'/glade/derecho/scratch/apdixon/national_gwp'
    results_folder<-'/glade/derecho/scratch/apdixon/national_gwp_results'
    Test <- FALSE # if TRUE, only run county, filtered below
    county_number<-args[2]
    Glade=TRUE
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
    cat("date and time are ")
    print(Sys.time())
  }
}

library(dplyr)
library(data.table)

r<-dir(results_folder, full.names = T)

county_data<-read.csv(file.path(master_path, 'Data', 'County_start', 'county_centroids_elevation_crops.csv'))

GEOIDS<-county_data$GEOID

results_data<-data.frame()

for (i in GEOIDS[1:3]){
  county_data_1<-filter(county_data, GEOID==i)
  
  county_geoid<-county_data_1$GEOID 
  county_name<-county_data_1$NAMELSAD
  state_name<-county_data_1$State_Name
  Maize_ha<-county_data_1$Maize_ha
  Cotton_ha<-county_data_1$Cotton_ha
  Soybean_ha<-county_data_1$Soybean_ha
  Wheat_ha<-county_data_1$Wheat_ha
  
  
  tryCatch({
    get_county_results_folder<-r[grep(paste0("_", county_geoid, "_"), r)]
    
    daycent_results<-list.files(get_county_results_folder, pattern = 'Daycent_', full.names = T)
    ldndc_results<-list.files(get_county_results_folder, pattern = 'LDNDC_', full.names = T)
    mill_results<-list.files(get_county_results_folder, pattern = 'Millennial_', full.names = T)
    
    day_files<-length(daycent_results)
    ldndc_files<-length(ldndc_results)
    mill_files<-length(mill_results)
    
    d_rows<-nrow(fread(daycent_results[1]))
    ld_rows<-nrow(fread(ldndc_results[2]))
    mill_rows<-nrow(fread(mill_results[6]))
    
  }, warning = function(war) {
    
  }, error = function(err) {
    
    day_files=0
    ldndc_files=0
    mill_files=0
    d_rows=0
    ld_rows=0
    mill_rows=0
    
  }, finally = {
    
    # NOTE:  Finally is evaluated in the context of of the inital
    # NOTE:  tryCatch block and 'e' will not exist if a warning
    # NOTE:  or error occurred.
    #print(paste("e =",e))
  }) # END tryCatch
  
  
  results_data<-rbind(results_data, 
                    data.frame(county_geoid, county_name, state_name, Maize_ha, Cotton_ha, Soybean_ha, 
                               Wheat_ha, day_files, ldndc_files, mill_files,
                               d_rows, ld_rows, mill_rows))
  

}

  write.csv(results_data, file.path(results_folder, 'results_data.csv'))
