
# Soybean schedule vectors


soybean_1 <- c(
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
  'C1            Initial crop ## low-yield corn', # Corn until soybeans arrive in the 1900s
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
  '1 115 CULT H     ## April 25 Disk',
  '1 120 FERT (0.75N)	## April 30',
  '1 121 CULT D     ## May 1 Cultivate',
  '1 124 CROP C1    ## May 4',
  '1 124 PLTM       ## May 4 Plant corn',
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
  '1 115 CULT H     ## April 25 Disk',
  '1 120 FERT (0.75N)	## April 30',
  '1 121 CULT D     ## May 1 Cultivate',
  '1 124 CROP C3    ## May 4',
  '1 124 PLTM       ## May 4',
  '1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw',
  '-999 -999 X',
  '3       Block ##  ',
  '1920    Last year',
  '1       Repeats # of years',
  '1901    Output starting year',
  '12       Output month',
  '1  Output interval',
  'C       Weather choice ## Continue',
  '1 111 CULT K     ## April 21 Moldboard plow',
  '1 115 CULT H     ## April 25 Disk',
  '1 120 FERT (2.2N)	## April 30',
  '1 121 CULT D     ## May 1 Cultivate',
  '1 124 CROP C3    ## May 4',
  '1 124 PLTM       ## May 4',
  '1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw',
  '-999 -999 X',
  '4       Block ## ',
  '2021    Last year',
  '1       Repeats # of years',
  '1921    Output starting year',
  '12       Output month',
  '1  Output interval',
  'C       Weather choice ## Continue',
  '1 138 CULT K     ## May 18 Moldboard plow',
  '1 145 CULT H     ## May 25 Disc',
  '1 145 CULT D     ## May 25 Cultivate',
  '1 149 CROP SYBN2 ## May 29',
  '1 149 PLTM       ## May 29 Plant soybean',
  '1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw',
  '-999 -999 X')

# # Scenario 1 - no change, monoculture
soybean_scenario_1<-c(
  '5       Block ## ',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12       Output month',
  '1  Output interval',
  'C       Weather choice ## Continue',
  '1 138 CULT K     ## May 18 Moldboard plow',
  '1 145 CULT H     ## May 25 Disc',
  '1 145 CULT D     ## May 25 Cultivate',
  '1 149 CROP SYBN3 ## May 29',
  '1 149 PLTM       ## May 29 Plant soybean',
  '1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw',
  '-999 -999 X'
  )

# # Scenario 2- no-till
soybean_scenario_2<-c(
  '5       Block ## ',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12       Output month',
  '1  Output interval',
  'C       Weather choice ## Continue',
  '1 124 CULT NDRIL ## May 4', # No-till drill
  '1 124 CROP SYBN3 ## May 29', # Plant soybean
  '1 124 PLTM       ## ',
  '1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw',
  '-999 -999 X'
)


# Scenario 3 - cover crop mix
soybean_scenario_3<-c(
  '5       Block ## ',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12       Output month',
  '1  Output interval',
  'C       Weather choice ## Continue',
  '1 138 CULT K     ## May 18 Moldboard plow',
  '1 145 CULT H     ## May 25 Disc',
  '1 145 CULT D     ## May 25 Cultivate',
  '1 149 CROP SYBN3 ## May 29',
  '1 149 PLTM       ## May 29 Plant soybean',
  '1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw',
  '1 305 CROP RYELG ## Nov 1, plant rye, legume, grass mix cover crop',
  '1 305 PLTM       ## Nov 1',
  '-999 -999 X'
)
  
# Scenario 4 - cover crop cereal
soybean_scenario_4<-c(
  '5       Block ',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12       Output month',
  '1  Output interval',
  'C       Weather choice ## Continue',
  '1 138 CULT K     ## May 18 Moldboard plow',
  '1 145 CULT H     ## May 25 Disc',
  '1 145 CULT D     ## May 25 Cultivate',
  '1 149 CROP SYBN3 ## May 29',
  '1 149 PLTM       ## May 29 Plant soybean',
  '1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw',
  '1 305 CROP RYE ## Nov 1, plant rye cover crop',
  '1 305 PLTM       ## Nov 1',
  '-999 -999 X'
)


# Scenario 5 - cover crop legume (vetch)
soybean_scenario_5<-c(
  '5       Block ',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12       Output month',
  '1  Output interval',
  'C       Weather choice ## Continue',
  '1 138 CULT K     ## May 18 Moldboard plow',
  '1 145 CULT H     ## May 25 Disc',
  '1 145 CULT D     ## May 25 Cultivate',
  '1 149 CROP SYBN3 ## May 29',
  '1 149 PLTM       ## May 29 Plant soybean',
  '1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw',
  '1 305 CROP VETCH ## Nov 1, plant vetch cover crop',
  '1 305 PLTM       ## Nov 1',
  '-999 -999 X'
)

# Scenario 6 - notill + cover crop
soybean_scenario_6<-c(
  '5       Block ## ',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12       Output month',
  '1  Output interval',
  'C       Weather choice ## Continue',
  '1 149 CULT NDRIL      ## May 4', # No-till drill
  '1 149 CROP SYBN3 ## May 29', # Plant soybean
  '1 149 PLTM       ## ',
  '1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw',
  '1 305 CULT NDRIL ## Nov 1',
  '1 305 CROP RYELG ## Nov 1, plant rye, legume, grass mix cover crop',
  '1 305 PLTM       ## ',
  '-999 -999 X'
)