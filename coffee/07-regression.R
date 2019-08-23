# predict production area based on climate variables (do not include year, but should look at lagging past year's production area)

# join3 has all data to be tested
names(join3)
unique(join3$year)
join3 = rename(join3, "production"="production area")

# select only top 4 coffee states: Minas Gerais, Espiritu Santo, 
#Sao Paulo, Bahia

fit <- join3 %>% filter(region %in% c("Minas Gerais","Espirito Santo","Sao Paulo","Bahia"))
fit$production <- as.numeric(fit$production)
fit$sqavgtmp <- fit$avgtmp^2
fit$sqtmax <- fit$tmax^2
fit$sqtmin <- fit$tmin^2
fit$sqpmax <- fit$pmax^2
fit$sqpmin <- fit$pmin^2

model <- lm(fit$production ~ fit$avgtmp + fit$tmax + fit$tmin + fit$pmax + fit$pmin + fit$drought_percent + 
              fit$sqavgtmp + fit$sqtmax + fit$sqtmin + fit$sqpmax + fit$sqpmin)
summary(model)

# check for perfect collinearity
fitcorr <-fit %>% select("production", "avgtmp", "tmax","tmin", "pmax",
                         "pmin", "drought_percent", 
                         "sqavgtmp", "sqtmax","sqtmin","sqpmax","sqpmin") %>% cor()
#make production area a log
#fit$production <- log10(fit$production)

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(fit))

# Randomly order data
shuffled_fit <- fit[rows,]

# create training and testing datasets
train <- shuffled_fit[1:16,]
test <- shuffled_fit[17:21,] 

#initial model 
model <- lm(production ~ avgtmp + tmax + tmin + pmax + pmin + drought_percent
            + sqavgtmp + sqtmax + sqtmin + sqpmax + sqpmin, train)
summary(model)

#prediction on test dataset
p <- predict(model, test)

#calculate RMSE
error <- p - test[, "production"]
sqrt(mean(error^2))

# initial prediction to 2050 data
year2050 <- all_region_climate %>% filter(year == 2050)
year2050$sqavgtmp <- year2050$avgtmp^2
year2050$sqtmax <- year2050$tmax^2
year2050$sqtmin <- year2050$tmin^2
year2050$sqpmax <- year2050$pmax^2
year2050$sqpmin <- year2050$pmin^2

year2050 <- year2050 %>% filter(region %in% c("Minas Gerais","Espirito Santo","Sao Paulo","Bahia"))
p <- predict(model, year2050)
#p = 10^p
p = as.data.frame(p)

year2050prediction <- cbind(p, year2050)
year2050prediction = rename(year2050prediction, "production"="p")

year2050prediction$production = as.numeric(year2050prediction$production)
