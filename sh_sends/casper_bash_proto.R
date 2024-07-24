# Test script for Casper PBS

con <- file(file.path(figs_input_data, paste0("/glade/derecho/scratch/apdixon/national_gwp/sh_sends/test_R_console_out.log")))
sink(con)

# for pulling in county number from batch script
args <- commandArgs(trailingOnly = TRUE)

args=(commandArgs(TRUE))


county_number = args[2]

try(print("county_number"))
try(print(county_number))
try(print(args[1]))
try(print(args[2]))
try(print(args[3]))

sink()