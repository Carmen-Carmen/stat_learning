# vectors and sequences
x = c(2, 7, 5)
x
y = seq(from=4, length=3, by=3) # i.e. 4, 5, 6
?seq
y

# operation done in parallel
x + y
x / y
x ^ y

# access element
x[2]
x[2:3]
x[-2] # remove the element at 2

# matrix
z = matrix(seq(1, 12), 4, 3) # create a matrix with 4 rows and 3 cols, filling with 1 to 12
z
z[3:4, 2:3]
z[, 3:3]
z[, 1]
z[, 1, drop=FALSE] # output the 1st col in the form of a col (if drop = TRUE, then will output in a line)
dim(z) # dimension of a matrix

# functions
ls()
rm(y)
ls()

# generating random data, graphic
x = runif(1000) # random uniform distribution
y = rnorm(1000) # random normal distribution
plot(x, y)
plot(x, y, xlab="random uniform", ylab="random normal", pch="*", col="blue")
par(mfrow=c(2, 1)) # par(): set graphical parameters; mfrow: multi-figures in one row, c(2, 1) means 2 rows and 1 col. 
plot(x, y)
hist(y)
par(mfrow=c(1, 1))

# reading data
setwd("~/Documents/stat_learning/chapter_2")
Auto = read.csv("../csv_files/Auto.csv")
dim(Auto)
class(Auto) # data.frame
summary(Auto) # list all the elements of automobiles
# cyl = Auto$cylinders # use $ notation to fetch elements from a data frame
# mpg = Auto$mpg
plot(Auto$cyl, Auto$mpg) # plot miles per gallon against the cylinders (engines) of an automobile
plot(Auto$mpg, Auto$cyl)
attach(Auto)
search()
plot(cylinders, mpg)
cylinders = as.factor(cylinders)

setwd("~/Documents/stat_learning/chapter_2")
Auto = read.csv("../csv_files/Auto.csv", 
                na.strings = "?", # any time it sees a particular character or set of characters (such as a question mark), it should be treated as a missing element of the data matrix
                stringsAsFactors = TRUE # any variable containing character strings should be interpreted as a qualitative variable
                )
head(Auto)
dim(Auto)
Auto[1:4, ]
Auto = na.omit(Auto) # remove rows containing missing observations
dim(Auto)
names(Auto)
attach(Auto)
plot(cylinders, mpg)
cylinders = as.factor(cylinders) # convert quantitative variables into qualitative variables
plot(cylinders, mpg, col = "red", varwidth = TRUE, xlab = "cylinders", ylab = "MPG") # now the plot will be a boxplot
hist(mpg, col = 2, breaks = 15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, data = Auto)
plot(horsepower, mpg)
identify(horsepower, mpg, name)

