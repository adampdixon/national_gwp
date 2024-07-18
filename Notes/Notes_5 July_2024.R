Questions for Debjani

-How should we be summarizing at annual level for main outputs?
  -SOC, N2O, CH4, CO2, yield

-How to calculate SOC for LDNDC daily? see LDNDC_metrx_day

-check Millennial SOC output conversions for SOC


check out why ldndc has 2022-2050 events twice

is LDNDC outputting NA for SOC in 2050?

make models same colors in plots
get national vis graphs and maps working

-figure out why yield is showing error in yearly plots
-get national graphs and maps working

make updates as said in email

how models are saved in the data

readme files for each script

add notes about what you were confused about

files and folder system


Things unsure about
-How to calculate SOC for Daycent daily? See Daycent output results script
-CH4 outputs seemed to be always negative, so plots were written that way, see All_models_county_only_yearly_plots.R
-line 154 Daycent results:   mutate(CH4_net_gChad = -(CH4_oxid_gChad)* 0.2, # make it negative  !! Let Debjani know