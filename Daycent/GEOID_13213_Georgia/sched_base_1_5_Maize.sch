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
C1            Initial crop ## low-yield corn
              Initial tree
    
Year Month Option
1       Block ## Corn, low yield, no fertilizer
1875    Last year
1       Repeats # of years
1850    Output starting year
12       Output month
1  Output interval
F       Weather choice
basic_1.wth
1 111 CULT K     ## April 21 Moldboard plow
1 121 CULT H     ## May 1 Disk
1 121 CULT D     ## May 1 Cultivate
1 124 CROP C1    ## May 4
1 124 PLTM       ## May 4 Plant corn
1 177 FERT (0.75N)	## June 26
1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
-999 -999 X
2       Block ## Higher-yielding corn with fertilizer
1900    Last year
1       Repeats # of years
1876    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue
1 111 CULT K     ## April 21 Moldboard plow
1 121 CULT H     ## May 1 Disk
1 121 CULT D     ## May 1 Cultivate
1 124 CROP C3    ## May 4
1 124 PLTM       ## May 4
1 177 FERT (1.5N)	## June 26
1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
-999 -999 X
3       Block ## Higher-yielding corn with fertilizer
1920    Last year
1       Repeats # of years
1901    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue
1 111 CULT K     	 ##April 21 Moldboard plow
1 121 CULT H     	 ##May1 Disk
1 125 FERT (2.2N)	 ##May 5
1 126 CULT D     	 ##May 6 Cultivate
1 126 CROP C3    	 ##May 6
1 126 PLTM       	 ##May 6
1 296 HARV G90S	   ## Oct 23 - Harvest grains and 90% straw
-999 -999 X
4       Block ## Higher-yielding corn with fertilizer
1970    Last year
1       Repeats # of years
1921    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue
1 121 CULT H     ## May 1 Disk
1 121 CULT D     ## May 1 Cultivate
1 124 CROP C4    ## May 4
1 124 PLTM       ## May 4
1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
1 305 FERT (2.2N)	## Oct 31
-999 -999 X
5       Block ## Higher-yielding corn with fertilizer
2021    Last year
1       Repeats # of years
1971    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue
1 121 CULT H     ## May 1 Disk
1 121 CULT D     ## May 1 Cultivate
1 124 CROP C4    ## May 4
1 124 PLTM       ## May 4
1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
1 305 FERT (12.0N)	## Oct 31
-999 -999 X
6       Block ## Higher-yielding corn with fertilizer
2050    Last year
1       Repeats # of years
2022    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue
1 111 FERT (16.8N)	  ##April 21 Fertilize
1 121 CULT H     	  ##May1 Disk
1 126 CULT D     	  ##May 6 Cultivate
1 126 CROP C6 	      ##May 6
1 126 PLTM       	  ##May 6
1 296 HARV G90S	    ##Oct 23 - Harvest grains and 90% straw
1 302 CULT H     	  ##Oct 28 Disk
1 303 CULT D     	  ##Oct 29 Cultivate
1 305 CROP VETCH 	  ##Nov 1, plant vetch cover crop
1 305 PLTM       	  ##Nov 1
-999 -999 X
/home/ap/Documents/GitHub/national_gwp/Daycent/GEOID_13213_Georgia/sched_base_1_5_Maize.sch
