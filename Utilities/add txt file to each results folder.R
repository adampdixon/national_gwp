
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

for (i in GEOIDS){
  county_data_1<-filter(county_data, GEOID==i)
  
  county_geoid<-county_data_1$GEOID 
  county_name<-county_data_1$NAMELSAD
  state_name<-county_data_1$State_Name
  Maize_ha<-county_data_1$Maize_ha
  Cotton_ha<-county_data_1$Cotton_ha
  Soybean_ha<-county_data_1$Soybean_ha
  Wheat_ha<-county_data_1$Wheat_ha
  
  get_county_results_folder<-r[grep(paste0("_", county_geoid, "_"), r)]
  
  results<-list.files(get_county_results_folder, pattern = '.csv', full.names = T)
  
  csv_files<-length(results)
  
  random_numbs<-sample(1:csv_files, 3)
  nrows_random_1<-nrow(fread(results[random_numbs[1]]))
  nrows_random_2<-nrow(fread(results[random_numbs[2]]))
  nrows_random_3<-nrow(fread(results[random_numbs[3]]))
  
  results_data<-rbind(results_data, 
                      data.frame(county_geoid, county_name, state_name, maize_ha, cotton_ha, soybean_ha, 
                                 wheat_ha, csv_files, nrows_random_1, nrows_random_2, nrows_random_3))
  
  for (c in c("Maize_ha", "Cotton_ha", "Soybean_ha", "Wheat_ha")){
    crop_amount<-eval(c)
    if (crop_amount<1){
      print(paste0("*************** creating text file. no ", c, " in county **********************************"))
      file.create(file.path(get_county_results_folder, paste0("1 note - there is no ", c, " in county ", county_geoid,".txt")))
    }
  }
}


write.csv(results_data, file.path(results_folder, 'results_data.csv'))
