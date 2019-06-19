## correlation analysis of World Clim variabls
rows <- ncol(output) 

for(name in output_names) {
  cors <- cor(subset(output[complete.cases(output),], select = c(3:21)))
}

# find variables that definitely need to be included and then remove other variables
# with high correlations
temp2 <- output %>% select("x", "y", "bio1", "bio2") 
temp3 <- temp2[complete.cases(temp2),]
temp3 <- cor(temp3[,3], temp3[,4])
?cor
