


args <- commandArgs(trailingOnly = TRUE)

cat("********************************\n")
print(args)
my_var<-args[2]
# Use the variable
cat(paste0("The value of my_var is: ", my_var, "\n"))
cat("********************************\n")
