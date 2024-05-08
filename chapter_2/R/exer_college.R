csv_path = "~/Documents/stat_learning/csv_files"
setwd("~/Documents/stat_learning/chapter_2")
college = read.csv(paste(csv_path, "College.csv", sep = "/"))
rownames(college) = college[, 1] # make names of colleges the name of each row
View(college)
college = college[, -1] # remove the first col, i.e. college names in the col "x"
View(college)

# i. Use the summary() function to produce a numerical 
# summary of the variables in the data set.
summary(college)

# ii. Use the pairs() function to produce a scatterplot matrix 
# of the first ten columns or variables of the data. 
# Recall that you can reference the first ten columns of a matrix A using A[,1:10].
pairs(college[, 2:11])

# iii. Use the plot() function to produce side-by-side boxplots of Outstate versus Private.
private = as.factor(college$Private)
outstate = college$Outstate
plot(x = private, y = outstate, 
     varwidth = TRUE, 
     xlab = "Private or Not", ylab = "Out-of-state Tuition")

# iv. Create a new qualitative variable, called Elite, by binning the Top10perc variable. 
# We are going to divide universities into two groups based on whether or not 
# the proportion of students coming from the top 10 % of their high school classes exceeds 50 %.
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite) # add Elite as a new col to college
summary(college$Elite)
plot(x = Elite, y = outstate, 
     varwidth = TRUE, 
     xlab = "Elite or Not", ylab = "Out-of-state Tuition")

# v. Use the hist() function to produce some histograms with differing numbers 
# of bins for a few of the quantitative variables. 
# You may find the command par(mfrow = c(2, 2)) useful: 
# it will divide the print window into four regions so that four plots 
# can be made simultaneously. 
# Modifying the arguments to this function will divide the screen in other ways.
par(mfrow = c(2, 2))
hist(college$Apps, xlab = "number of applications")
hist(college$Accept, xlab = "number of acceptions")
hist(college$PhD, xlab = "number of PhDs")
hist(college$Terminal, xlab = "number of terminal degree holders")
par(mfrow = c(1, 1))
