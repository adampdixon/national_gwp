#######################################
# Script: 1_Create_management_schedule_AD_.R
# Author: Adam Dixon
# Date: December 16, 2023
# Output: .sch file
# Description: Generates DayCent schedule file specifically in the format that
# Daycent needs
# https://www2.nrel.colostate.edu/projects/irc/public/Documents/Software/Century5/Reference/html/CMI/mgmt-sch-file.htm
#######################################
# Called by:
# 
#######################################
# Audit Log:
# Dec 2023 - Adpated from Maas scripts and colo state reference
#######################################

# # ---------------------------------------------------------------
# #  File: c3grs.sch
# #  DayCent5 management file for a C3 grassland.
# #  (The Century5 file would be the same but exclude "Daily output" lines.)
# #
# #  Runs for about 2005 years for a spinup w/grazing.
# #  Last 2 years do not have grazing.
# #  Prints monthly results for the last 3 years.
# #  Prints daily results for the last 2 years for DayCent5.
# #
# #  This schedule can be used with Century5 and DayCent5.
# # ---------------------------------------------------------------
# # Simulation Configuration
# 1           Starting year
# 2005        Last year
# not-used    Site file name
# 0           Labeling type
# 0           Labeling year
# -1          Microcosm
# 0           CO2 Systems
# 1           Initial system
# CPR         Initial crop
# Initial tree
# 
# Year Month Option
# 1           Block #   Northern Hemisphere C3 Dominated
# 2003        Last year
# 1           Repeats # years
# 2003        Output starting year
# 1           Output month
# 0.0833      Output interval
# M           Weather choice
# 1   1 CROP  CPR
# 1   1 FRST
# 1   5 GRAZ  GM
# 1   6 GRAZ  GM
# 1   7 GRAZ  GM
# 1   8 GRAZ  GM
# 1   9 GRAZ  GM
# 1  11 SENM
# 1  12 LAST
# END
# 
# 2           Block #   no grazing
# 2005        Last year
# 1           Repeats # years
# 2004        Output starting year
# 1           Output month
# 0.0833      Output interval
# 2004        Daily output starting year
# 1           Daily output month
# M           Weather choice
# 1   1 CROP  CPR
# 1   1 FRST
# 1  11 SENM
# 1  12 LAST
# END

print('*********Creating managment schedule file for Daycent*************')

schedule_file_maize <- file.path(daycent_path2 , paste0("sched_base_", scenario_name, "_Maize.sch"))
schedule_file_cotton <- file.path(daycent_path2 , paste0("sched_base_", scenario_name, "_Cotton.sch"))
schedule_file_soybeans <- file.path(daycent_path2 , paste0("sched_base_", scenario_name, "_Soybean.sch"))
schedule_file_wheat <- file.path(daycent_path2 , paste0("sched_base_", scenario_name, "_Wheat.sch"))

MAIZE <- c(
          '1850          Starting year ## start with assumed ground-breaking for agriculture until intensification',
          '2050          Last year',
          'site.100  Site file name',
          '0             Labeling type ## all defaults turned off',
          '-1            Labeling year',
          '-1.00         Microcosm',
          '-1            CO2 Systems',
          '-1            pH effect',
          '-1            Soil warming',
          '0             N input scalar option (0 or 1)',
          '0             OMAD scalar option (0 or 1)',
          '0             Climate scalar option',
          '1             Initial system',
          'C1            Initial crop ## low-yield corn',
          '              Initial tree',
          '    ',
          'Year Month Option',
          '1       Block ## Corn, low yield, no fertilizer',
          '1875    Last year',
          '1       Repeats # of years',
          '1850    Output starting year',
          '12       Output month',
          '1  Output interval',
          'F       Weather choice',
          'basic_1.wth',
          '1 111 CULT K     ## April 21 Moldboard plow',
          '1 121 CULT H     ## May 1 Disk',
          '1 121 CULT D     ## May 1 Cultivate',
          '1 124 CROP C1    ## May 4',
          '1 124 PLTM       ## May 4 Plant corn',
          '1 177 FERT (0.75N)	## June 26',
          '1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw',
          '-999 -999 X',
          '2       Block ## Higher-yielding corn with fertilizer',
          '1900    Last year',
          '1       Repeats # of years',
          '1876    Output starting year',
          '12       Output month',
          '1  Output interval',
          'C       Weather choice ## Continue',
          '1 111 CULT K     ## April 21 Moldboard plow',
          '1 121 CULT H     ## May 1 Disk',
          '1 121 CULT D     ## May 1 Cultivate',
          '1 124 CROP C3    ## May 4',
          '1 124 PLTM       ## May 4',
          '1 177 FERT (1.5N)	## June 26',
          '1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw',
          '-999 -999 X',
          '3       Block ## Higher-yielding corn with fertilizer',
          '1920    Last year',
          '1       Repeats # of years',
          '1901    Output starting year',
          '12       Output month',
          '1  Output interval',
          'C       Weather choice ## Continue',
          '1 111 CULT K     ## April 21 Moldboard plow',
          '1 121 CULT H     ## May 1 Disk',
          '1 121 CULT D     ## May 1 Cultivate',
          '1 124 CROP C3    ## May 4',
          '1 124 PLTM       ## May 4',
          '1 177 FERT (2.2N)	## June 26',
          '1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw',
          '-999 -999 X',
          '4       Block ## Higher-yielding corn with fertilizer',
          '2050    Last year',
          '1       Repeats # of years',
          '1921    Output starting year',
          '12       Output month',
          '1  Output interval',
          'C       Weather choice ## Continue',
          '1 111 CULT K     ## April 21 Moldboard plow',
          '1 121 CULT H     ## May 1 Disk',
          '1 121 CULT D     ## May 1 Cultivate',
          '1 124 CROP C4    ## May 4',
          '1 124 PLTM       ## May 4',
          '1 177 FERT (2.2N)	## June 26',
          '1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw',
          '-999 -999 X')




COTTON <- c('1850          Starting year ## start with assumed ground-breaking for agriculture until intensification',
            '2050          Last year',
            'site.100  Site file name',
            '0             Labeling type ## all defaults turned off',
            '-1            Labeling year',
            '-1.00         Microcosm',
            '-1            CO2 Systems',
            '-1            pH effect',
            '-1            Soil warming',
            '0             N input scalar option (0 or 1)',
            '0             OMAD scalar option (0 or 1)',
            '0             Climate scalar option',
            '1             Initial system',
            'GI5          Initial crop ## sorghum',
            '              Initial tree',
            '    ',
            'Year Month Option',
            '1       Block ## COTTON, low yield, no fertilizer',
            '1959    Last year',
            '1       Repeats # of years',
            '1850    Output starting year',
            '12      Output month',
            '1       Output interval',
            'F       Weather choice',
            'basic_eq.wth',
            '1 89 CULT K			## Mar 30',
            '1 118 CULT K  ## Apr 28',
            '1 140 CULT ROW  ## May 20',
            '1 140 CROP COT ## May 20',
            '1 161 PLTM 			## Jun 10',
            '1 290 HARV G90S  ## Oct 17',
            '-999 -999 X',
            '2       Block ## Switch to cotton',
            '2050    Last year',
            '1       Repeats # of years',
            '1960    Output starting year',
            '12      Output month',
            '1       Output interval',
            'C       Weather choice ## Continue',
            '1 118 CULT K  ## Apr 28',
            '1 140 CULT ROW  ## May 20',
            '1 140 CROP COT ## May 20',
            '1 140 PLTM  ## May 20',
            '1 290 HARV G90S  ## Oct 17',
            '-999 -999 X')



SOYBEANS <- c(
          '1850          Starting year ## start with assumed ground-breaking for agriculture until intensification',
          '2050          Last year',
          'site.100  Site file name',
          '0             Labeling type ## all defaults turned off',
          '-1            Labeling year',
          '-1.00         Microcosm',
          '-1            CO2 Systems',
          '-1            pH effect',
          '-1            Soil warming',
          '0             N input scalar option (0 or 1)',
          '0             OMAD scalar option (0 or 1)',
          '0             Climate scalar option',
          '1             Initial system',
          'C1            Initial crop ## low-yield corn',
          '              Initial tree',
          '    ',
          'Year Month Option',
          '1       Block ## Corn, low yield, no fertilizer',
          '1875    Last year',
          '1       Repeats # of years',
          '1850    Output starting year',
          '12       Output month',
          '1  Output interval',
          'F       Weather choice',
          'basic_1.wth',
          '1 111 CULT K     ## April 21 Moldboard plow',
          '1 121 CULT H     ## May 1 Disk',
          '1 121 CULT D     ## May 1 Cultivate',
          '1 124 CROP C1    ## May 4',
          '1 124 PLTM       ## May 4 Plant corn',
          '1 177 FERT (0.75N)	## June 26',
          '1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw',
          '-999 -999 X',
          '2       Block ## Higher-yielding corn with fertilizer',
          '1900    Last year',
          '1       Repeats # of years',
          '1876    Output starting year',
          '12       Output month',
          '1  Output interval',
          'C       Weather choice ## Continue',
          '1 111 CULT K     ## April 21 Moldboard plow',
          '1 121 CULT H     ## May 1 Disk',
          '1 121 CULT D     ## May 1 Cultivate',
          '1 124 CROP C3    ## May 4',
          '1 124 PLTM       ## May 4',
          '1 177 FERT (1.5N)	## June 26',
          '1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw',
          '-999 -999 X',
          '3       Block ## Higher-yielding corn with fertilizer',
          '1920    Last year',
          '1       Repeats # of years',
          '1901    Output starting year',
          '12       Output month',
          '1  Output interval',
          'C       Weather choice ## Continue',
          '1 138 CULT K     ## May 18 Moldboard plow',
          '1 145 CULT H     ## May 25 Disc',
          '1 145 CULT D     ## May 25 Cultivate',
          '1 149 CROP SYBN3 ## May 29',
          '1 149 PLTM       ## May 29 Plant soybean',
          '1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw',
          '-999 -999 X',
          '4       Block ## Higher-yielding corn with fertilizer',
          '2050    Last year',
          '1       Repeats # of years',
          '1921    Output starting year',
          '12       Output month',
          '1  Output interval',
          'C       Weather choice ## Continue',
          '1 138 CULT K     ## May 18 Moldboard plow',
          '1 145 CULT H     ## May 25 Disc',
          '1 145 CULT D     ## May 25 Cultivate',
          '1 149 CROP SYBN3 ## May 29',
          '1 149 PLTM       ## May 29 Plant soybean',
          '1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw',
          '-999 -999 X')


WHEAT <- c(
            '1850          Starting year ## start with assumed ground-breaking for agriculture until intensification',
            '2050          Last year',
            'site.100  Site file name',
            '0             Labeling type ## all defaults turned off',
            '-1            Labeling year',
            '-1.00         Microcosm',
            '-1            CO2 Systems',
            '-1            pH effect',
            '-1            Soil warming',
            '0             N input scalar option (0 or 1)',
            '0             OMAD scalar option (0 or 1)',
            '0             Climate scalar option',
            '1             Initial system',
            'GI5          Initial crop ## sorghum',
            '              Initial tree',
            '    ',
            'Year Month Option',
            '1       Block ## Wheat W4EG',
            '1959    Last year',
            '1       Repeats # of years',
            '1850    Output starting year',
            '12      Output month',
            '1       Output interval',
            'F       Weather choice',
            'basic_eq.wth',
            '1 89 CULT K			## Mar 30',
            '1 118 CULT K  ## Apr 28',
            '1 140 CULT ROW  ## May 20',
            '1 140 CROP W4EG ## May 20',
            '1 161 PLTM 			## Jun 10',
           '1 225 HARV G90S	## Oct 23 - Harvest grains and 90% straw',
            '-999 -999 X',
            '2       Block ## Switch to cotton',
            '2050    Last year',
            '1       Repeats # of years',
            '1960    Output starting year',
            '12      Output month',
            '1       Output interval',
            'C       Weather choice ## Continue',
            '1 118 CULT K  ## Apr 28',
            '1 140 CULT ROW  ## May 20',
            '1 140 CROP W4EG ## May 20',
            '1 140 PLTM  ## May 20',
           '1 225 HARV G90S	## Oct 23 - Harvest grains and 90% straw',
            '-999 -999 X')


writeLines(WHEAT, schedule_file_wheat)
writeLines(SOYBEANS, schedule_file_soybeans)
writeLines(COTTON, schedule_file_cotton)
writeLines(MAIZE, schedule_file_maize)


# block_txt <- c(header_txt, ops_txt, footer_txt)
# writeLines(block_txt,schedule_file_2100)

# if(identical(crop, "Maize")){
#   writeLines(MAIZE, schedule_file)
# }
# if(identical(crop, "Soybeans")){
#   writeLines(SOYBEANS, schedule_file)
# }
# if(identical(crop, "Wheat")){
#   writeLines(WHEAT, schedule_file)
# }
# if(identical(crop, "Cotton")){
#   writeLines(COTTON, schedule_file)
# }



# Spinup equilibrium schedule file
sp_eq_schedule_file <- file.path(daycent_path2 , paste0("sched_eq.sch"))

sp_eq<-c('1             Starting year',
         '4000          Last year',
         'site.100  Site file name',
         '0             Labeling type',
         '-1            Labeling year',
         '-1.00         Microcosm',
         '-1            CO2 Systems',
         '-1            pH effect',
         '-1            Soil warming',
         '0             N input scalar option (0 or 1)',
         '0             OMAD scalar option (0 or 1)',
         '0             Climate scalar option',
         '1             Initial system',
         'GI3           Initial crop',
         '              Initial tree',
         '   ',
         'Year Month Option',
         '1            Block #   Eq_KBS',
         '3999         Last year',
         '1            Repeats # years',
         '1            Output starting year',
         '12           Output month',
         '100.0        Output interval',
         'F            Weather choice',
         'basic_1.wth',
         '  1   75 CROP GI3',
         '  1   75 FRST',
         '  1  106 GRAZ GM',
         '  1  136 GRAZ GM',
         '  1  167 GRAZ GM',
         '  1  197 GRAZ GM',
         '  1  228 GRAZ GM',
         '  1  259 GRAZ GM',
         '  1  289 LAST',
         '  1  289 SENM',
         '-999 -999 X',
         '2            Block #   Eq_KBS',
         '4000         Last year',
         '1            Repeats # years',
         '4000         Output starting year',
         '12           Output month',
         '10.0         Output interval',
         'C            Weather choice',
         '  1   75 CROP GI3',
         '  1   75 FRST',
         '  1  106 GRAZ GM',
         '  1  136 GRAZ GM',
         '  1  167 GRAZ GM',
         '  1  197 GRAZ GM',
         '  1  228 GRAZ GM',
         '  1  259 GRAZ GM',
         '  1  289 LAST',
         '  1  289 SENM',
         '-999 -999 X')


writeLines(sp_eq, sp_eq_schedule_file)



         




