#install.packages('ggplot2')
#install.packages('xtable')
library(ggplot2)
library(splines)
library(xtable)


# Theme for plots
theme <- theme(
  axis.text.x  = element_text(size = 20), 
  axis.text.y  = element_text(size = 20),
  axis.title.x = element_text(size = 25),
  axis.title.y = element_text(size = 25),
  plot.title   = element_text(hjust = 0.5, size = 15))

################
## Question 1 ##
################

# Read the data
marine.df <- read.csv('marine.csv', header = TRUE)

# Scatter plot
ggplot(marine.df, aes(x = depth, y = diameter)) + 
  geom_point(size = 3, colour = '#f72585', alpha = 0.5) + 
  theme

# Has 3 vertices e.g. 4th order polynomial 

################
## Question 2 ##
################

# Fit polynomial regression with order 2
marine.poly2.fit <- lm(diameter ~ poly(depth, 2), data = marine.df)
summary(marine.poly2.fit)

ggplot(marine.df, aes(x = depth, y = diameter)) + 
  geom_point(size = 3, colour = '#f72585', alpha = 0.5) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x, 2), colour = '#7209b7',
              lwd = 2, alpha = 0) +
  theme

# Fit polynomial regression with order 10
marine.poly10.fit <- lm(diameter ~ poly(depth, 10), data = marine.df)
summary(marine.poly10.fit)

ggplot(marine.df, aes(x = depth, y = diameter)) + 
  geom_point(size = 3, colour = '#f72585', alpha = 0.5) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x, 10), colour = '#7209b7',
              lwd = 2, alpha = 0) +
  theme

# Fit polynomial regression with order 20
marine.poly20.fit <- lm(diameter ~ poly(depth, 20), data = marine.df)
summary(marine.poly20.fit)

ggplot(marine.df, aes(x = depth, y = diameter)) + 
  geom_point(size = 3, colour = '#f72585', alpha = 0.5) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x, 20), colour = '#7209b7',
              lwd = 2, alpha = 0) +
  theme


################
## Question 3 ##
################

# Create the spline
marine.cs.6k.bs <- with(bs(x = depth, degree = 3,
                        knots = 10 * c(1:6)[-5], 
                        intercept = TRUE), data = marine.df)


# Fit the model
marine.cs.6k.fit <- lm(diameter ~ -1 + marine.cs.6k.bs, data = marine.df)
summary(marine.cs.6k.fit)

# Plot the spline
ggplot(marine.df, aes(x = depth, y = diameter)) + 
  geom_point(size = 3, colour = '#f72585', alpha = 0.5) + 
  stat_smooth(method = 'lm', formula = y ~ -1 + bs(x = x, degree = 3,
                                                   knots = 10 * c(1:6), 
                                                   intercept = TRUE), 
              colour = '#c1121f', lwd = 2, alpha = 0) +
  theme

# Plot confidence interval
marine.cs.6k.pred = predict(marine.cs.6k.fit, interval = "confidence",
                           level = 0.95)

ggplot(marine.df, aes(x = depth, y = diameter)) + 
  geom_point(size = 3, colour = '#f72585', alpha = 0.4) + 
  stat_smooth(method = 'lm', formula = y ~ -1 + bs(x = x, degree = 3,
                                                   knots = 10 * c(1:6)[-5], 
                                                   intercept = TRUE), 
              colour = '#c1121f', lwd = 2, alpha = 0) +
  geom_line(aes(x = sort(depth), y = marine.cs.6k.pred[order(depth), "upr"]),
             colour = '#118ab2', lwd = 1.5, linetype = 2) +
  geom_line(aes(x = sort(depth), y = marine.cs.6k.pred[order(depth), "lwr"]),
            colour = '#118ab2', lwd = 1.5, linetype = 2) +
  theme

################
## Question 4 ##
################

# Cubic spline has K + 4 = 9 df. Natural cubic spline has K df so total knots
# should be K = 9 which is K - 2 = 7 internal knots.

# Create natural cubic spline basis
marine.int.knots.pos <- 5 + (60 / 8) * c(1:7)
marine.bnd.knots.pos <- c(5, 65)
marine.ns.7k.bs <- ns(marine.df$depth, knots = marine.int.knots.pos, 
                      Boundary.knots = marine.bnd.knots.pos, intercept = TRUE)

# Fit the natural cubic spline
marine.ns.7k.fit <- lm(diameter ~ -1 + marine.ns.7k.bs, data = marine.df)
summary(marine.ns.7k.fit)

# Plot the spline
ggplot(marine.df, aes(x = depth, y = diameter)) + 
  geom_point(size = 3, colour = '#f72585', alpha = 0.5) + 
  stat_smooth(method = 'lm', formula = y ~ -1 + ns(x = x, 
                                                   knots = marine.int.knots.pos, 
                                                   Boundary.knots =
                                                     marine.bnd.knots.pos, 
                                                   intercept = TRUE), 
              colour = '#c1121f', lwd = 2, alpha = 0) +
  theme

# Plot confidence interval
marine.ns.7k.pred = predict(marine.ns.7k.fit, interval = "confidence",
                            level = 0.95)

ggplot(marine.df, aes(x = depth, y = diameter)) + 
  geom_point(size = 3, colour = '#f72585', alpha = 0.5) + 
  stat_smooth(method = 'lm', formula = y ~ -1 + ns(x = x, 
                                                   knots = marine.int.knots.pos, 
                                                   Boundary.knots =
                                                     marine.bnd.knots.pos, 
                                                   intercept = TRUE), 
              colour = '#c1121f', lwd = 2, alpha = 0) +
  geom_line(aes(x = sort(depth), y = marine.ns.7k.pred[order(depth), "upr"]),
            colour = '#118ab2', lwd = 1.5, linetype = 2) +
  geom_line(aes(x = sort(depth), y = marine.ns.7k.pred[order(depth), "lwr"]),
            colour = '#118ab2', lwd = 1.5, linetype = 2) +
  theme

################
## Question 5 ##
################

# Smoothing spline with effective df = 12.5
marine.sm.12.5df <- smooth.spline(marine.df$diameter ~ marine.df$depth, 
                                  df = 12.5)
marine.sm.12.5df

# Smoothing spline with effective df = 17.5
marine.sm.17.5df <- smooth.spline(marine.df$diameter ~ marine.df$depth, 
                                  df = 17.5)
marine.sm.17.5df

# Smoothing spline with effective df = 25
marine.sm.25df <- smooth.spline(marine.df$diameter ~ marine.df$depth, 
                                  df = 25)
marine.sm.25df

## Plotting the splines

# Plotting the 12.5df spline
ggplot(marine.df, aes(x = depth, y = diameter)) + 
  geom_point(size = 3, colour = '#f72585', alpha = 0.5) + 
  geom_line(aes(x = sort(depth), y = fitted(marine.sm.12.5df)[order(depth)]), 
            colour = '#c1121f', lwd = 2) +
  theme

# Plotting the 17.5df spline
ggplot(marine.df, aes(x = depth, y = diameter)) + 
  geom_point(size = 3, colour = '#f72585', alpha = 0.5) + 
  geom_line(aes(x = sort(depth), y = fitted(marine.sm.17.5df)[order(depth)]), 
            colour = '#c1121f', lwd = 2) +
  theme

# Plotting the 25df spline
ggplot(marine.df, aes(x = depth, y = diameter)) + 
  geom_point(size = 3, colour = '#f72585', alpha = 0.5) + 
  geom_line(aes(x = sort(depth), y = fitted(marine.sm.25df)[order(depth)]), 
            colour = '#c1121f', lwd = 2) +
  theme

################
## Question 6 ##
################

# Set up required variables
knotsPos <- c(15, 25, 35, 45, 55) # Knot positions
polyDegreeList <- c(1:5) # Proposed polynomial orders
overallCVErrVec <- vector(length = length(polyDegreeList)) # Vector to store 
                                                        # overall CV errors

# Use a for loop to iterate over each proposed polynomial order
for (polyDegree in polyDegreeList){
  # Store CV error per each fold
  cvErrList <- c()
  
  # Use a for loop to iterate over fold for cross-validation
  for (fold in sort(unique(marine.df$group))) {
    # Split the data into training and validation set
    val.df   <- marine.df[marine.df$group == fold, ] # Current fold
    train.df <- marine.df[marine.df$group != fold, ] # Remaining K-1 folds  
    
    # Train the model
    marine.Ms.fit  <- with(train.df, lm(diameter ~ -1 + 
                                          bs(x = depth, degree = polyDegree,
                                             knots = knotsPos, 
                                             intercept = TRUE)))
    
    # Calculate CV error
    marine.Ms.pred <- predict(marine.Ms.fit, newdata = val.df)
    #print(length(marine.Ms.pred))
    cvErrList[fold] <- sum((val.df$diameter - marine.Ms.pred)^2)
    
  }
  
  overallCVErrVec[polyDegree] <- sum(cvErrList) / nrow(marine.df)
}

# Chosen degree
print(which.min(overallCVErrVec))

# CV error for the chosen degree
print(min(overallCVErrVec))






