#######################################
# Function: "Create_soil_data"
# Author: "Ellen Maas"
# Date: "July 11, 2022"
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates files in the appropriate folder for each model.
# Description: "This procedure generates soil data for the site."
#######################################


###########################
#Daycent
###########################

#soils.in
###########################

# collect: upper depth of soil layer (cm), lower depth of soil layer (cm), bulk density (g/cc),
# field capacity (volumetric), wilting point (volumetric), 

## cannot name with scenario numbers
write.table(soil_df,  
            file=file.path(daycent_path2,"soils.in"),
            row.names=F, quote=F, col.names=F, sep=' ')

# I don't know why this won't run without the extra soils and site files
write.table(soil_df,
            file=file.path(daycent_path2,"soils_1.in"),
            row.names=F, quote=F, col.names=F, sep=' ')


write.table(soil_df,
            file=file.path(daycent_path2,"soils_2.in"),
            row.names=F, quote=F, col.names=F, sep=' ')


write.table(soil_df,
            file=file.path(daycent_path2,"soils_3.in"),
            row.names=F, quote=F, col.names=F, sep=' ')


print("soil data written to soils.in")

# phaq_value_avg, bdfiod_value_avg, #These were updated in the setup file


#site.100
###########################
site100_file<-c(site_1, site_2, lat, long, elev, site_2, epnfa_2_0, site_3)
output_site<-file.path(daycent_path2 , 'site.100')
writeLines(site100_file, output_site) # site file name
#site_1.100
###########################
site100_1_file<-c(site_1, lat, long, elev, site_2, epnfa_2_1, site_3)
output_site<-file.path(daycent_path2 , 'site_1.100')
writeLines(site100_1_file, output_site) # site file name
#site_2.100
###########################
site100_2_file<-c(site_1, lat, long, elev, site_2, epnfa_2_2, site_3)
output_site<-file.path(daycent_path2 , 'site_2.100')
writeLines(site100_2_file, output_site) # site file name
#site_3.100
###########################
site100_3_file<-c(site_1, lat, long, elev, site_2, epnfa_2_3, site_3)
output_site<-file.path(daycent_path2 , 'site_3.100')
writeLines(site100_3_file, output_site) # site file name

print("site.100 files written to daycent folder")

