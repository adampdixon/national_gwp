1850          Starting year ## start with assumed ground-breaking for agriculture until intensification
2050          Last year
site.100  Site file name
0             Labeling type ## all defaults turned off
-1            Labeling year
-1.00         Microcosm
-1            CO2 Systems
-1            pH effect
-1            Soil warming
0             N input scalar option (0 or 1)
0             OMAD scalar option (0 or 1)
0             Climate scalar option
1             Initial system
GI5          Initial crop ## sorghum
              Initial tree
    
Year Month Option
1       Block ## COTTON, low yield, no fertilizer
1959    Last year
1       Repeats # of years
1850    Output starting year
12      Output month
1       Output interval
F       Weather choice
basic_eq.wth
1 89 CULT K			## Mar 30
1 118 CULT K  ## Apr 28
1 140 CULT ROW  ## May 20
1 140 CROP COT ## May 20
1 161 PLTM 			## Jun 10
1 290 HARV G90S  ## Oct 17
-999 -999 X
2       Block ## Switch to cotton
2021    Last year
1       Repeats # of years
1960    Output starting year
12      Output month
1       Output interval
C       Weather choice ## Continue
1 118 CULT K  ## Apr 28
1 120 FERT (2.8N)  ## May 20
1 140 CULT ROW  ## May 20
1 140 CROP COT ## May 20
1 140 PLTM  ## May 20
1 290 HARV G90S  ## Oct 17
-999 -999 X
3       Block ## Switch to cotton
2050    Last year
1       Repeats # of years
2022    Output starting year
12      Output month
1       Output interval
C       Weather choice ## Continue
1 118 CULT K  ## Apr 28
1 120 FERT (2.8N)  ## May 20
1 140 CULT ROW  ## May 20
1 140 CROP COT ## May 20
1 140 PLTM  ## May 20
1 290 HARV G90S  ## Oct 17
1 305 CROP RYELG ## Nov 1, plant rye, legume, grass mix cover crop
1 305 PLTM       ## Nov 1
-999 -999 X
/home/ap/Documents/GitHub/national_gwp/Daycent/GEOID_42053_Pennsylvania/sched_base_1_3_Cotton.sch
