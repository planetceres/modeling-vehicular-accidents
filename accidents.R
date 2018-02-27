a <- read.csv("./Stats19_Data_2005-2014/Accidents0514.csv")
v <- read.csv("./Stats19_Data_2005-2014/Vehicles0514.csv")
c <- read.csv("./Stats19_Data_2005-2014/Casualties0514.csv")
# levels(factor(a$Urban_or_Rural_Area))


# -------------------------------------------------------------
# 1 Ratio of Urban to Rural Accidents
# -------------------------------------------------------------
# Urban ==1, Rural == 2, Unallocated == 3
accidents.urban <- nrow(a[a$Urban_or_Rural_Area == 1,])
accidents.total <- nrow(a)
accidents.urban.ratio <- accidents.urban/accidents.total

measure.1 <- accidents.urban.ratio
measure.1

# -------------------------------------------------------------
# 2 Find Most Dangerous Hour
# -------------------------------------------------------------
# Get hour
a$Hour <- as.numeric(substr(a$Time,1,2))

# binarize fatal accidents
a$fatal <- ifelse(a.2$Accident_Severity == 1, 1, 0)

# reduce size of dataframe
a.2 <- a[c('Hour', 'fatal')]

# normalize fatal accidents by hour
a.2.normalized <- aggregate(. ~ Hour, a.2, mean)

# Sort and get max
most.dangerous.hour <- a.2.normalized[with(a.2.normalized, order(-fatal)), ][1,]

measure.2 <- most.dangerous.hour[2]
measure.2


# -------------------------------------------------------------
# 3 Trend Over Time Series
# -------------------------------------------------------------

# Format Date and Extract Year
a$Date.formatted <- strptime(as.character(a$Date), "%d/%m/%Y")
a$Year <- format(a$Date.formatted, "%Y")

# Make annual data into time series
annual.totals <- data.frame(table(factor(a$Year)))
colnames(annual.totals) <- c('Year', 'Accidents')
a3.ts <- ts(annual.totals$Accidents, frequency=1, start=c(2005,1))

# Get linear trend line from model
a3 <- as.numeric(a3.ts)
time.a3 <- as.numeric(time(a3.ts))
a3.linear <- lm(a3 ~ time.a3)
a3.slope <- a3.linear$coefficients[2]
measure.3 <- a3.slope
measure.3


# -------------------------------------------------------------
# 4 Correlation of Number of Casualties to Speed Limit
# -------------------------------------------------------------

# Bin speed limit
sl.totals <- data.frame(table(factor(a$Speed_limit)))
colnames(sl.totals) <- c('Speed_limit', 'Accidents')
c.1 <- a[c('Speed_limit', 'Number_of_Casualties')]

# Calculate ratio of casualties to accidents 
cr.1 <- aggregate(Number_of_Casualties~Speed_limit, c.1, sum)
cr.2 <- merge(cr.1, sl.totals, by.x = 'Speed_limit', by.y='Speed_limit')
cr.2$c.ratio <- cr.2$Number_of_Casualties/cr.2$Accidents

# Model and get Pearson Coefficient
m4.2 <- lm(c.ratio ~ Speed_limit, data=cr.2) 
speed.pearson <- cor(cr.2$c.ratio, cr.2$Speed_limit, method=c('pearson'))

measure.4 <- speed.pearson
measure.4

# -------------------------------------------------------------
# 5 Probability of Accidents in Weather
# -------------------------------------------------------------

# Merge Accident data and Vehicle data on index
av <- merge(a, v, by.x = 'Accident_Index', by.y='Accident_Index')
av <- av[which(av$Weather_Conditions > 0),]
av <- av[c('Weather_Conditions', 'Skidding_and_Overturning')]

# Create binary variables for skidding and weather
av$skid.binary <- ifelse(av$Skidding_and_Overturning > 0, 1, 0)
av$rain.snow.binary <- ifelse(av$Weather_Conditions == c(2,3,5,6), 1, 0)
av$nice.binary <- ifelse(av$Weather_Conditions == 1, 1, 0)

# Get conditional counts
w.total <- nrow(av)
w.skid.weather <- av[which(av$skid.binary == 1 & av$rain.snow.binary == 1),]
w.noskid.weather <- av[which(av$skid.binary == 0 & av$rain.snow.binary == 1),]
w.skid.noweather <- av[which(av$skid.binary == 1 & av$nice.binary == 1),]
w.noskid.noweather <- av[which(av$skid.binary == 0 & av$nice.binary == 1),]

# Calculate weather vs no weather
prob.weather <- nrow(w.skid.weather)/(nrow(w.skid.weather)+nrow(w.noskid.weather))
prob.noweather  <- nrow(w.skid.noweather)/(nrow(w.skid.noweather)+nrow(w.noskid.noweather))

prob.weather
prob.noweather

measure.5 <- prob.weather/prob.noweather
measure.5


# -------------------------------------------------------------
# 6 Probability of Accidents By Gender
# -------------------------------------------------------------

v.6 <- merge(a, v, by.x = 'Accident_Index', by.y='Accident_Index')
v.6 <- v.6[c('Sex_of_Driver', 'Vehicle_Type', 'fatal')]

# Only fatal accidents
v.6 <- v.6[which(v.6$fatal == 1),]

# Create binary variable for gender, remove missing and unknown values
v.6 <- v.6[which(v.6$Sex_of_Driver > 0 & v.6$Sex_of_Driver < 3),]
v.6$sex.binary <- ifelse(v.6$Sex_of_Driver == 1, 1, 0)

# Limit to cars only (Vehicle_Type == 9)
v.6 <- v.6[which(v.6$Vehicle_Type ==9),]

# Condition on gender
c.male <- v.6[which(v.6$sex.binary == 1),]
c.female <- v.6[which(v.6$sex.binary == 0),]

prob.male <- nrow(c.male)/(nrow(c.male) + nrow(c.female))
prob.female <- nrow(c.female)/(nrow(c.male) + nrow(c.female))

measure.6 <- prob.male/prob.female
measure.6


# -------------------------------------------------------------
# 7 Get District Area
# -------------------------------------------------------------

# Aggregate data on district and calculate
d <- a[c('Location_Northing_OSGR', 'Location_Easting_OSGR', 'Local_Authority_.District.')]
d.agg <- aggregate(. ~ Local_Authority_.District., d, function(x) sd = sd(x))

# Calculate area (pi*x*y)
d.agg <- transform(d.agg, area = pi*(Location_Easting_OSGR/1000)*(Location_Northing_OSGR/1000))
district.max.area <- d.agg[with(d.agg, order(-area)), ][1,]
district.sq.km <- district.max.area$area

measure.7 <- district.sq.km
measure.7

# -------------------------------------------------------------
# 8 Use Exponentially Decaying Model to Predict Casualties by Age
# -------------------------------------------------------------

d.8 <- v[c('Age_of_Driver')]

# Create row for unit variable and aggregate on Age of Driver
d.8$casualty <- rep(1, nrow(d.8))
d.8.agg <- aggregate(. ~ Age_of_Driver, d.8, length)

# Filter ages legally allowed to drive
d.8.agg <- d.8.agg[which(d.8.agg$Age_of_Driver > 16),]

# Construct model with exponential decay
d1 <- lm(log(casualty) ~ Age_of_Driver, data=d.8.agg)
plot(d.8.agg, fitted(d1), type="l")
lines(exp(predict(d1)))
summary(d1)

# Get coefficient for rate of decay
measure.8 <- summary(d1)$coefficients[2]
measure.8

format(round(measure.1, 10), nsmall = 10)
format(round(measure.2, 10), nsmall = 10)
format(round(measure.3, 10), nsmall = 10)
format(round(measure.4, 10), nsmall = 10)
format(round(measure.5, 10), nsmall = 10)
format(round(measure.6, 10), nsmall = 10)
format(round(measure.7, 10), nsmall = 10)
format(round(measure.8, 10), nsmall = 10)