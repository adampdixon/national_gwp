
# Cotton schedule vectors
# https://extension.okstate.edu/fact-sheets/cotton-yield-goal-nitrogen-rate-recommendation.html
# using N fert rates same as LRF


cotton_1 <- c(
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
  '1       Block ## COTTON, low yield, no fertilizer',
  '1949    Last year',
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
  '2021    Last year',
  '1       Repeats # of years',
  '1950    Output starting year',
  '12      Output month',
  '1       Output interval',
  'C       Weather choice ## Continue',
  '1 118 CULT K  ## Apr 28',
  '1 120 FERT (2.8N)  ## May 20',
  '1 140 CULT ROW  ## May 20',
  '1 140 CROP COT ## May 20',
  '1 140 PLTM  ## May 20',
  '1 290 HARV G90S  ## Oct 17',
  '-999 -999 X')

# # Scenario 1 - no change, monoculture
cotton_scenario_1<-c(
  '3       Block ## Switch to cotton',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12      Output month',
  '1       Output interval',
  'C       Weather choice ## Continue',
  '1 118 CULT K  ## Apr 28',
  '1 120 FERT (2.8N)  ## May 20',
  '1 140 CULT ROW  ## May 20',
  '1 140 CROP COT ## May 20',
  '1 140 PLTM  ## May 20',
  '1 290 HARV G90S  ## Oct 17',
  '-999 -999 X'
  )

# # Scenario 2- no-till
cotton_scenario_2<-c(
  '3       Block ## Switch to cotton',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12      Output month',
  '1       Output interval',
  'C       Weather choice ## Continue',
  '1 120 FERT (2.8N)  ## May 20',
  '1 140 CULT NDRIL      ## May 4', # No-till drill
  '1 140 CROP COT ## May 20',
  '1 140 PLTM  ## May 20',
  '1 290 HARV G90S  ## Oct 17',
  '-999 -999 X'
)

# Scenario 3 - cover crop mix
cotton_scenario_3<-c(
  '3       Block ## Switch to cotton',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12      Output month',
  '1       Output interval',
  'C       Weather choice ## Continue',
  '1 118 CULT K  ## Apr 28',
  '1 120 FERT (2.8N)  ## May 20',
  '1 140 CULT ROW  ## May 20',
  '1 140 CROP COT ## May 20',
  '1 140 PLTM  ## May 20',
  '1 290 HARV G90S  ## Oct 17',
  '1 305 CROP RYELG ## Nov 1, plant rye, legume, grass mix cover crop',
  '1 305 PLTM       ## Nov 1',
  '-999 -999 X'
)


# Scenario 4 - cover crop cereal
cotton_scenario_4<-c(
  '3       Block ## Switch to cotton',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12      Output month',
  '1       Output interval',
  'C       Weather choice ## Continue',
  '1 118 CULT K  ## Apr 28',
  '1 120 FERT (2.8N)  ## May 20',
  '1 140 CULT ROW  ## May 20',
  '1 140 CROP COT ## May 20',
  '1 140 PLTM  ## May 20',
  '1 290 HARV G90S  ## Oct 17',
  '1 305 CROP RYE ## Nov 1, plant rye cover crop',
  '1 305 PLTM       ## Nov 1',
  '-999 -999 X'
)


# Scenario 5 - cover crop legume (vetch)
cotton_scenario_5<-c(
  '3       Block ## Switch to cotton',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12      Output month',
  '1       Output interval',
  'C       Weather choice ## Continue',
  '1 118 CULT K  ## Apr 28',
  '1 120 FERT (2.8N)  ## May 20',
  '1 140 CULT ROW  ## May 20',
  '1 140 CROP COT ## May 20',
  '1 140 PLTM  ## May 20',
  '1 290 HARV G90S  ## Oct 17',
  '1 305 CROP VETCH ## Nov 1, plant vetch cover crop',
  '1 305 PLTM       ## Nov 1',
  '-999 -999 X'
)

# Scenario 6 - notill + cover crop
cotton_scenario_6<-c(
  '3       Block ## Switch to cotton',
  '2050    Last year',
  '1       Repeats # of years',
  '2022    Output starting year',
  '12      Output month',
  '1       Output interval',
  'C       Weather choice ## Continue',
  '1 120 FERT (2.8N)  ## May 20',
  '1 140 CULT NDRIL      ## May 4', # No-till drill
  '1 140 CROP COT ## May 20',
  '1 140 PLTM       ## ',
  '1 290 HARV G90S  ## Oct 17',
  '1 305 CULT NDRIL      ## Nov 1', # No-till drill
  '1 305 CROP RYELG ## Nov 1, plant rye, legume, grass mix cover crop',
  '1 305 PLTM       ## Nov 1',
  '-999 -999 X'
)
