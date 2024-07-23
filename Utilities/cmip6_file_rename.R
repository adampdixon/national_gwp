# rename files to include climate code

if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    source('/home/ap/Documents/GitHub/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  } else {
    source('/glade/derecho/scratch/apdixon/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  }
}

# cmip_126<-list.files(output_dir, full.names = TRUE, pattern = 'cmip6.csv')
# 
# for(i in cmip_126){
#   new_file_name<-paste0(substr(i, 1, (nchar(i)-4)), '_ssp126.csv')
#   file.rename(from = i, to= new_file_name)
# }


# Change name of CMIP6 Connecticut GEOIDs to match old style county ids and not the new reigonal planning ones

              
# f2<-f[grepl(c('_9110_|_9130_|_9150_|_9160_|_9170_|_9180_|_9190_'), f)]

# GEOIDS to switch to
# 9110	9003
# 9130	9007
# 9150	9015
# 9160	9005
# 9170	9009
# 9180	9011
# 9190	9001

# f<-list.files('/glade/work/apdixon/climate', full.names=T)
f<-list.files(climate_data_path, full.names=T)

old_geoids<-c('_9110_', '_9130_', '_9150_', '_9160_', '_9170_', '_9180_', '_9190_')
new_geoids<-c('_9003_', '_9007_', '_9015_', '_9005_', '_9009_', '_9011_', '_9001_')


for(i in 1:length(old_geoids)){
  
  f2<-f[grepl(c(old_geoids[i]), f)]
  
  for (l in 1:length(f2)){
    
    old_file_name<-f2[l]
    new_file_name<-gsub(old_geoids[i], new_geoids[i], old_file_name)
    
    file.copy(from = old_file_name, to= new_file_name)
    
    print('copied over')
    print(new_file_name)
    
  }
  

}

# f3<-f[grepl(c('_9110_|_9130_|_9150_|_9160_|_9170_|_9180_|_9190_'), f)]
# f3
# 
# search<-paste(new_geoids, collapse = '|')
# 
# f3<-f[grepl(search, f)]
# f3
# 
# f[grepl("_9003_|_9007_|_9015_|_9005_|_9009_|_9011_|_9001_", f)]
