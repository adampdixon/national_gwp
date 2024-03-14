# rename files to include climate code

# Set workspace
if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    geo_link_dir<-'/home/ap/Documents/GitHub/national_gwp/Data/County_start'
    output_dir<-'/home/ap/Scratch'
    
    county_number = 1
    
    cmip6_dir<-'/home/ap/Documents/GitHub/national_gwp/cmip6_climate'
    print("************************************")
    print("*****Using linux mint *********")
  } else {
    # county data to link
    geo_link_dir<-'/glade/u/home/apdixon/Documents/national_gwp/Data/County_start'
    output_dir<-'/glade/work/apdixon/climate'
    
    cmip6_dir<-'/glade/work/apdixon/Output_climate' # say output climate, but it's Zhuonan's output folder when he was processing
    
    county_number = args[1]
    
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
  }
}

cmip_126<-list.files(output_dir, full.names = TRUE, pattern = 'cmip6.csv')

for(i in cmip_126){
  new_file_name<-paste0(substr(i, 1, (nchar(i)-4)), '_ssp126.csv')
  file.rename(from = i, to= new_file_name)
}


