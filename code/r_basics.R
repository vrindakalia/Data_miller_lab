# This is a pretty fancy calculator

# Familiriaze with the R studio layout
# source/editor
# console
# plots etc pane
# environment pane

# Global options to change things

# assignment statements
a <- 5 # vector
x <- seq(1:5) #vector
x <- rep(1, times = 5) #vector

x <- matrix(seq(1:5)) #matrix

x <- data.frame(seq(1:5)) #data frame

?class

class(a) #numeric
class(x) # data frame
class(x$seq.1.5.) # numeric

?names
names(x) <- "x.1" #change name of column
class(x$x.1) #numeric

names(y) <- c("x.1", "x.2",...)

