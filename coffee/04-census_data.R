# pre processing
library(readxl)    
read_excel_allsheets <- function(filename, tibble = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# cleaning historic census data for all regions 
census <- read_excel_allsheets("/Users/isabellaragazzi/brazil-coffee/coffee/data/census_historical.xls")
census_list <- 2:28
y <- data_frame()
for(id in census_list) {
x <- as.data.frame(census[id])
x = x %>% mutate(uf = x[2,9])
x = x[-c(1:2),-c(7:9)]
names(x) <-  unlist(x[1,])
x = x[-1,]
names(x)[1] <- "variable"
names(x)[7] <- "uf"
names(x)[6] <- "1996" # 1995-1996 but need to choose a year to make numeric
x = x %>% tidyr::gather("year","value", -variable, -uf)
x$year = as.numeric(x$year)
x$value = as.numeric(gsub(" ","", x$value))
print(id)
y = rbind(y, x)
}

# data from IBGE or CONAB on arabica  coffee production 2008-2018
cafe_arabica <- read_excel_allsheets("/Users/isabellaragazzi/brazil-coffee/coffee/data/CafeArbicaSerieHist .xls")
names <- names(cafe_arabica)
current_production <- data_frame()
for(id in names) {
  sheet <- as.data.frame(cafe_arabica[id])
sheet = sheet[-c(1:3,31:32),]
#create a vector of the first row values and replace the column names
names(sheet) <-  unlist(sheet[1,])
#remove the first row
sheet = sheet[-1,]
sheet$id <- id
sheet = sheet %>% rename("variable" = id)
sheet = sheet %>% dplyr::rename(`2019` = `2019 (¹)`)
print(id)
current_production <- rbind(current_production, sheet)
}

# gather columns
current_production = current_production %>% rename("uf" = "UNIDADE FEDERAÇÃO/REGIÃO")
current_production = current_production %>% tidyr::gather("year","value", -variable, -uf)
current_production$year = as.numeric(current_production$year)
current_production$value = as.numeric(current_production$value)

# combine all current production data (2008-2018), remove blank years 
ufs <- read.csv("/Users/isabellaragazzi/brazil-coffee/coffee/data/region_uf.csv")
ufs = as.data.frame(ufs[,-1])
names(ufs) <- "uf"
tmp1 <- left_join(ufs, current_production, by = "uf")
tmp2 <- left_join(ufs, y, by = "uf")
agriculture_data <- rbind(tmp1,tmp2)
agriculture_data$value =  gsub("0.0", NA, agriculture_data$value)
agriculture_data$value =  gsub("0", NA, agriculture_data$value)
agriculture_data = agriculture_data %>% na.omit(agriculture_data$value)
agriculture_data = agriculture_data %>% tidyr::spread("variable", "value")
agriculture_data = agriculture_data %>% select("uf", "year", "Área em formação","Área em produção")

agriculture_data_fixed <- agriculture_data[!is.na(agriculture_data$`Área em produção`),]
