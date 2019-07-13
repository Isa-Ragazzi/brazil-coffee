### SETUP

# load in data
# im using random data to test for now
sat.seed(1234)

production <- sample(100:600, 30, replace=TRUE)

labor <- sample(50:350, 30, replace=TRUE)

capital <- sample (600:700, 30, replace=TRUE)

# cost function parameters: wage and price constants

wage <- 35.00 
price <- 30.00

# set up df
testdata <- data.frame(production=production, labor=labor, capital=capital, wage=wage, price=price)

#name rows with year (timeline)
row.names(testdata) <- 1980:2010

### REGRESSION MODEL

test.lm <- lm(formula = log(production) ~ log(labor) + log(capital), data = testdata)
summary (test.lm)

# store coefficients 
coeff <- coef(test.lm)

intercept <- coeff[1]
alpha <- coeff[2]
beta <- coeff[3]
