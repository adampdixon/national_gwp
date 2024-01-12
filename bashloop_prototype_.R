######


args <- commandArgs(trailingOnly = TRUE)

cat("********************************\n")
cat("********************************\n")
# Open a file connection
log_con <- file("bash_proto.log", open = "a")

print(args)
cat("********************************\n")
my_var<-args[2]
cat("********************************\n")
# Use the variable
cat(paste0("The value of my_var is: ", my_var, "\n"), file = log_con)
cat(paste0("The time is: ", Sys.time(), "\n"), file = log_con)


# Close the file connection
close(log_con)
cat("********************************\n")
cat("********************************\n")