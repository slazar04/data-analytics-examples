## Welcome to the 
##  _______     __ _         _____        _          _           _     
## |__   __|   / _| |       |  __ \      | |        | |         | |    
##    | |_   _| |_| |_ ___  | |  | | __ _| |_ __ _  | |     __ _| |__  
##    | | | | |  _| __/ __| | |  | |/ _` | __/ _` | | |    / _` | '_ \ 
##    | | |_| | | | |_\__ \ | |__| | (_| | || (_| | | |___| (_| | |_) |
##    |_|\__,_|_|  \__|___/ |_____/ \__,_|\__\__,_| |______\__,_|_.__/ 
##
##             Innovate. Analyze. Visualize. | datalab.tufts.edu
##
##  ----------------------------------------
##  Workshop title: Advanced Data Visualization in RStudio
##  By: Kyle Monahan & Ying Yang
##  Contact: kyle.monahan -AT- tufts.edu
##  ----------------------------------------
##
##
##  To access other online resources on statistics, see go.tufts.edu/stats

##-------------------- Getting Started ------------------
## 
## R is a statistical platform similar to Stata, SAS, and SPSS. This software 
## allows you to manipulate data, perform descriptive statistics, recoding 
## variables, and bringing in your own data. If you're reading this, you've opened
## RStudio (the development environment for R) and you're on the way! 
##
##-------------------------------------------------------

## We will walk through this document together in the workshop. 


##############
## Goals    ##
##############

# 1. General R skillsets for visualization:
#   Classes, functions and arguments in ggplot
# 2. Error troubleshooting - read and find R documentation 
# 3. An example - multi-level time series graph with color shading and legend
# 4. Publications and how to export your visualizations:
#   ggpubhigh-quality visualizations, templates,An example - exporting our time series graph to match the Economist
# 5. The grammar(s) of graphics - other implementations - animation, mapping, review of data.
# 6. Similarities across R packages and key takeaways

##############
## 0. Continuation from Lab2 : Management & Manipulation
##############
# In Last Lab I have left out a section called 'Addressing the Stata code' (from line 265 in Lab2's script) 
# But I find there are some useful functions that can help your HW2
# So I will go through it today
# To cause less confusion, I have removed the STATA part. But if you are interested in comparing these two, you can always go back to the Lab2's script from line 265 


# Change directories 
# Why do we need to set working directory? 
# Because it is the default location where R will look for files you want to load and where it will put any files you save. 
#  Can either go to Session > Set Working Directory > To Source File location, or write code - your choice 
# setwd("H:/Workshops/IntroR_DA/Data") 

# Check where we are
getwd()

# List the files here 
dir() 

# Take a look at the data we have
readLines("/Users/samanthalazar/Downloads/asec2014_pubuse.dat",n=1)

# Loop over values and import the dat files for CPS 

# With every loop, try a single loop first, then create the loop

library(readr) #Read in the data with readr()
# We will use the function read_fwf
# Passing in the file 
# Then the starting positions of the text file for each column as a list using combine c(), and column names
# See another example here: https://groups.google.com/forum/#!topic/manipulatr/e2etwwOHUvw
# See the documentation: https://readr.tidyverse.org/reference/read_fwf.html

asec2014 <- read_fwf(file="/Users/samanthalazar/Downloads/asec2014_pubuse.dat", fwf_positions(c(1,2,25,39,42,49,53),c(1,6,25,39,43,51,53),col_names = c("type","hid","htype","region","state","country","metro_status")))
# We use the function read_fwf like so, passing it column widths and column names
# start positions : c(1,2,25,39,42,49,53)
# end positions : c(1,6,25,39,43,51,53)
head(asec2014) # You can use head() to look at data
# Note the column types, always check this 

# Now we can do a loop 

# Create a years_list for all the years 
years_list = c("2014","2015","2016","2017","2018")


for (year in years_list){ 
  #print(year) # Always nice to check it is working with a print
  filename = paste0("/Users/samanthalazar/Downloads/asec", year, "_pubuse.dat")
  dataname = assign(paste0("asec", year),read_fwf(file=filename,fwf_positions(c(1,2,25,39,42,49,53),c(1,6,25,39,43,51,53),col_names = c("type","hid","htype","region","state","country","metro_status"))))
  print(filename)
  #print(dataname)
}

# Now we have all the data inside of R

# Let's get into formatting the data in R - you could do this in the loop, but the tidyverse in R suggests you do not do this. 

# Cleaning steps to do :
#  keep if type==1    // Selective keep depending on a value
#  drop type          // Drop a variable
#  sum                // Summarize to check how we are looking - you can use head
#  sort           // sort the observations using the id variable
#  save as an R dataset


# In R, we use filter() from dplyr
# First we install & load packages

install.packages("plyr")
install.packages("dplyr")

# library(tidyverse) if you load this package, then you don't need to load plyr & dplyr, because tidyverse is a package which includes plyr and dplyr
library(plyr)  #A data manipulation library - always load this first
library(dplyr) #Another data manipulation

asec2014 <- asec2014 %>% #This is a pipe, you can pass values for the data in  
  dplyr::filter(type == 1) # We use the denotion of package::function here
# Then we pass filter type == 1 to filter 
# We could pass another pipe and keep working if we want 

# Now we save the data as a CSV
readr::write_csv(x = asec2014, path = "asec2014.csv")

# Let's say I wanted to merge the data in this csv, with another dataset

# Create a second dataset

asec2015 <- asec2015 %>% #This is a pipe, you can pass values for the data in  
  dplyr::filter(type == 1) # We use the denotion of package::function here
# Then we pass filter type == 1 to filter 
# We could pass another pipe and keep working if we want 


# Adding values can be done like this: 
asec2015 <- asec2015 %>% 
  dplyr::mutate(id = row_number()) %>%  # Use control shift M to add this 
  dplyr::mutate(new_var = id * 10) %>%  # Can calculate within this
  dplyr::arrange(state)                 # Sort by state
# mutate() adds new variables and preserves existing ones : https://dplyr.tidyverse.org/reference/mutate.html

head(asec2015)

# Now we save the data as a CSV
readr::write_csv(x = asec2015, path = "asec2015.csv") # Note be careful with the 1-1 country code - excel thinks it is a date by default


# We can finally append these data together, using dplyr as well 
dplyr::bind_rows(asec2015,asec2014)

# Pro-tip - there is also a package that does most of this work for us http://asdfree.com/current-population-survey-annual-social-and-economic-supplement-cpsasec.html
# Pro-tip 2 - look at the Cheatsheets for all of this - Help > Cheatsheets. 

##############
## 1. Elements of R - General R skills 
##############

# Just like the elements of style that make up high-quality narratives when writing, knowing which type of data 
# you are working with can help create a more efficient workflow. For example. 

## VECTORS ##

# To create a vector, we can either pass a list: 

vect = c(1,2,3,4,5)


# Or use the fuction seq:
vect2 = seq(from = 1, to = 5, by = 1) #Note how seq is a function. If we press tab, we can see the requirements
#vect2 = seq()

vect2 = seq()

??seq()
# Search for the sequence function using the ??:
??base::seq()

# No results!
??seq()

##############
## 2. Troubleshooting issues 
##############

# Sometimes, using Google (or DuckDuckGo) is one of the best approaches. See: https://tufts.box.com/v/ThisOneSimpleTrick

# If we carefully search, finding issues can be easier. Let's do it together for seq() 

# language + object + verb + stackoverflow + error
# R + seq() + create a sequence + stackoverflow 
# Try it out! 

## MATRIX ##

# As discussed during the slides, the matrix is a set of vectors organized in a specific way, as a grid. 
# Let's say we wanted to create a matrix from our vectors (vect and vect2). How can we do this? 
# Use the "One Simple Trick" - Google "R convert vector to matrix" and what do you get? First result: https://stackoverflow.com/questions/14614946/how-to-turn-a-vector-into-a-matrix-in-r#14614969

# Okay, we need to use the function matrix.
# But how can we use it? Let's investigate!

# As matrix() is in baseR (the initial packages that come with R, we can just call matrix below!)

matrix #Note how we call it without the () - we don't want to use the function, we want to see it! 

matrix() #If we call the package like this, we will get a null value inside a 1 row and 1 column matrix. Why? Let's look at the package

## INVESTIGATE THE PACKAGE ##

# After we call this, R reports:

# > matrix
# function (data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) 
# {
#   if (is.object(data) || !is.atomic(data)) 
#     data <- as.vector(data)
#   .Internal(matrix(data, nrow, ncol, byrow, dimnames, missing(nrow), 
#                    missing(ncol)))
# }
# <bytecode: 0x10507b898>
#   <environment: namespace:base>
#   

# What does this mean?

# The > tells us what we sent to R - the call to the R engine
# The R engine reports it is a function, which takes the following inputs as default:
# data = NA (missing by default), nrow = 1, byrow = FALSE (don't add by row), dimnames = NULL (no dimension names))
# The function matrix then performs the following code:
# If the data input is an object, or (||) isn't atomic (single value) data, then 
# data <- as vector(data) - cast the data as a vector
# Then take those vectors, and cast then as an internal matrix. 
# The output of matrix() makes sense now! 

# For non-base packages, we can use the base R function getMethod() - more details here: https://www.r-bloggers.com/how-to-see-source-code-of-a-functionmethod-in-r/
# This lets us see - what is this function actually doing.

## IS IT A VECTOR? ##

# Let's say we wanted to test to see if vect and vect2 are vectors before we place them into matrix. 
# We can use class() to do this.

##############
## Classes in R    
##############

class(vect)
class(vect2)

# Well, these say numeric. That is the class of the data. There is also:
#    - numeric
#    - character
#    - string
#    - factor 
#    - etc. 

# Classes are how the data is stored within an object. 

#To access an object, we can use the is.object() function, where object is the type of object. 
#But it is a a VECTOR?

is.vector(vect)
is.vector(vect2)

# They return true - vect2 and vect are vectors! 

# We can also say this using an if statement. 
if (is.vector(vect) == TRUE) {paste0("vect is a VECTOR!")}

# So we created a vector. Nice! 

# Now we can create a matrix:
our_mat <- matrix(data = c(vect,vect2),nrow = 2, ncol = 5)

# We can even view, and even sort, the matrix, right within R:
View(our_mat)

##############
## Creating a dataframe    
##############

# Using the matrix we created, we can also create a data frame, using the same as.object() notation
our_df <- as.data.frame(our_mat, col.names = c(1,2,3,4,5))

# We can check if this is a data frame:
is.data.frame(our_df)


## String
my_string = c("Matrix", "can", "be", "sorted")

##Extract values from vector - remember that the index in R starts at 1, so it counts 1,2,3,4 etc. In almost all other languages, indexes start at 0. 
vect[2]

##Extract from string
my_string[2]


##############
## Functions    
##############

# These functions are exactly the same as if we were to create our own functions
# Take the example of a tip calculator. I'd like a function that takes the input of a numeric value, increases it by 15%
# and then reports that value. This means I can know what my total should be, with tip! 
# Of course I have my laptop and R open at a restaurant. 


#Kyle's Tip Calculator 
#What's my total with 15% tip?

myFunction <- function(tot){ #Create a function and store it as my function, that takes the input variable tot
  result <- tot * 1.15       #Multiply tot (input) by 1.15
  #print("result)
  paste0("Your new total is: ", result)   #Using paste0, we can print a list of strings. 
  #Why paste0? https://stackoverflow.com/questions/34584976/how-do-i-print-numeric-values-with-a-string-in-r#34584990
}

#I spent $100, so set total = 100 
total <- 100
myFunction(tot = total) #Note that we have an argument (tot) and we pass it the value (total).


## you try : Write a function that doubles the amount input into the function. 


##############
## 3A. An example - working with U.S. Census data 
##############

# Lets get part of the U.S. Census data (publicly available): https://www2.census.gov/programs-surveys/popest/datasets/2010/modified-race-data-2010/stco-mr2010_al_mo.csv

# Go to Import Dataset under Environment > Click "From Text" (readr) > Paste URL below import File/URL > "Update" > "Import" 

# URL: https://www2.census.gov/programs-surveys/popest/datasets/2010/modified-race-data-2010/stco-mr2010_al_mo.csv

# We can check if it is a data frame:

is.data.frame(stco_mr2010_al_mo)

# Now we have to look at the metadata: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/mr2010.pdf

## Rename the data frame 
census <- stco_mr2010_al_mo

## Changing the column names
colnames(census)[1] <- "GeographicSummary"

## Subset the data for just Mass 
MA <- subset(census, census$STNAME == "Massachusetts")


## Create another object to store the information on income
MAincome <- MA
MAincome$INCOME <- rep(50000,nrow(MA))
head(MAincome)

## Now we can merge back to the original data
census_income <- merge(census, MAincome, by = colnames(census), all.x =  TRUE, all.y = TRUE)

# Merge is a good alternative to rbind and cbind, what we used in Lab2.

# discussion: What do you notice about the new census_income data? What about the income column? 

## Data cleaning 

# City Name has County on the end - it repeats each time, we would like to remove it

# We can use gsub to remove this text 
gsub(" County","",MA$CTYNAME) -> MA$CTYNAME

# Let's load another data set, this time from AWS

# Install package
#install.packages("data.table")

# Load package
library(data.table)

url <-"https://s3.amazonaws.com/assets.datacamp.com/blog_assets/chol.txt"
passengers <- as.data.frame(data.table::fread(url)) #Use fread to read the text data in the URL into a data frame
head(passengers)

## Our first visualization - histogram

# We can use at the age distrubution of passengers
hist(passengers$AGE)

# Adding the arguments xlab and main, we can add axis titles
hist(passengers$AGE, 
     xlab = "Age in Years",
     main = "Age Distribution of Passengers")

# Let's say we really care about the association between weight and age 

plot(x = passengers$WEIGHT, 
     y = passengers$AGE, 
     ylab = "Age in Years",
xlab = "Weight in Kg")

# But this looks so boring! We can use a package **ggplot2** to make nicer graphics.

library(ggplot2)
# In ggplot, you first set up the axes with aes() add new "layers" with a +

ggplot(passengers, aes(x = WEIGHT, y = AGE)) + geom_point()

# We can even add another variable, smoking
ggplot(passengers, aes(x = WEIGHT, y = AGE, shape = SMOKE)) + geom_point() + 
  ylab("Age in Years")+xlab("Weight in Kg")

# And another, smoking and mortality
ggplot(passengers, aes(x = WEIGHT, y = AGE, shape = SMOKE, colour = MORT)) + geom_point() + 
  ylab("Age in Years")+xlab('Weight in Kg')

# We can also facet the plots:

ggplot(passengers, aes(x = WEIGHT, y = AGE, shape = SMOKE, colour = MORT)) + geom_point() + ylab('Age in Years')+xlab('Weight in Kg') + facet_wrap( ~ MORT, ncol=2)
#What does a tilde (~) in front of a single variable mean (facet_wrap) : https://stackoverflow.com/questions/51901398/what-does-a-tilde-in-front-of-a-single-variable-mean-facet-wrap


#####
# Regression
#####

# Let's say we want to create a regression model. To do this, we use lm
# First, we need to evaluate the variables. 

# Load packages

# Install packages - comment out if you don't need them
install.packages("rockchalk")     # 
#install.packages("jtools")        # My favorite recently
#install.packages("huxtable")      # Silent requirement for jtools
#install.packages("tidyverse")     # Most other pre-reqs
#install.packages("officer")       # Also need these
#install.packages("flextable")

library(rockchalk)                 # Load the package


# Load data
data(mtcars)             # Sample data on cars
# description : https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars

# Create a regression model
model <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+factor(vs)+factor(am)+gear+carb, data = mtcars)

# Default summary looks like this
summary(model)


# Use outreg to organize a table in LaTeX format
table <- rockchalk::outreg(model, type = "latex")


# Save it out as a tex file 
cat(table, file = "table.tex")

# You can compile the LaTeX file within R, but there are easier ways!

# Other options:
# I actually prefer jtools for reg output 
# More details here: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html#table_output_for_word_and_rmarkdown_documents

install.packages("jtools")
library(jtools)
library(officer)
jtools::summ(model, confint = TRUE, vifs = TRUE,  pvals = TRUE, robust = "HC1", digits = 3) # This a really nice summary

# For output from jtools
jtools::export_summs(model,confint = TRUE, vifs = TRUE,  pvals = TRUE, robust = "HC1", digits = 3) # Stata uses HC1 error estimators

# To export to xlsx, we can run the following
jtools::export_summs(model, to.file = "xlsx", file.name = "outreg.xlsx")

## you try: Try to run these models on your own


library(jtools)

## Read csv file - using college_distance_f20.csv
data <- read.csv(file.choose())
summary(data)

## Run a regression: Model 1
fit1 <- lm(yrsed ~ dist, data=data)
jtools::summ(fit1, confint = TRUE, digits = 4)

## Model 2
fit2 <- lm(yrsed ~ dist + female + black + hispanic, data=data)
jtools::summ(fit2, confint = TRUE, digits = 4)

## Model 3
fit3 <- lm(yrsed ~ dist + female + black + hispanic + bytest +
             incomehi + dadcoll + momcoll + ownhome + cue80 + stwmfg80, data=data)
jtools::summ(fit3, confint = TRUE, digits = 4)

## TIME CHECK! - Only continue if there is time.
#####


##############
## 3B. An example - recreating an Economist graphic 
##############

##############
## Visualization     
##############
# We have covered: vectors, classes, functions and arguments in R
# In ggplot (a function) we can also use vectors, classes and arguments.

#Let's dive in with an example trying to recreate a 
### Install packages ###
install.packages("tidyverse")

### Load packages ###
# I know I need read_csv, so I will load tidyverse 
library(tidyverse)
library(ggplot2)

### Working directory ###
# Go to Session > Set Working Directory > To Source File Location 

### Import data ###
dat <- read_csv("data/EconomistData.csv")

# Note it tells me - parsed with column specification. I should always review this! 
# These data consist of Human Development Index and Corruption Perception Index scores, by country. 

## Goals:

# Create a scatter plot with CPI on the x and HDI on the y 
# Color the points blue to see where they are.
# Map color to region to stratify by region. 
# Map the size to HDI rank. 

# From Harvard training, where I got this original datasets:

# Advantages of ggplot2
#
# -   consistent underlying `grammar of graphics` (Wilkinson, 2005)
# -   plot specification at a high level of abstraction
# -   very flexible
# -   theme system for polishing plot appearance
# -   mature and complete graphics system
# -   many users, active mailing list
#
# That said, there are some things you cannot (or should not) do With ggplot2:
#
# -   3-dimensional graphics (see the rgl package)
# -   Graph-theory type graphs (nodes/edges layout; see the igraph package)
# -   Interactive graphics (see the ggvis package)

# In addition, I would suggest that there are better plots for publishing graphics from ggplot (ggpub).

# So, first we want to create a scatter plot:

# Create a scatter plot with CPI on the x and HDI on the y 

g <- ggplot(dat, aes(x = CPI, y = HDI)) +
  geom_point()

g

### Export
## Be sure to Export this and upload to Canvas!!
# Just go to the Plots (on the far right of your screen), and then go to Export. 

# How would we color the points blue?

# Well, since geom_point() is adding a geometry layer to the map, containing points, 
# we can pass an argument to the geom_point() function there.

g <- ggplot(dat, aes(x = CPI, y = HDI)) +
  geom_point(color = "blue")

g

# You might wonder, how does it know what "blue" is? See here: https://ggplot2.tidyverse.org/reference/geom_point.html
# It says "other arguments passed on to layer". So we need to see what layer() is: https://ggplot2.tidyverse.org/reference/layer.html
# Now we can see about color = "blue". But it still doesn't mention color. Use our Most Important Slide / One Simple Trick: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
# And we see that colors are string values that are passed to the graphical engine in R. 

g2 <- ggplot(dat, aes(x = CPI, y = HDI)) +
  geom_point(aes(color = Region))

g2

# We can check what class this object is:
class(g2)

# It is a ggplot graph, as expected!

# Can also size by the HDI rank as well:

g3 <- ggplot(dat, aes(x = CPI, y = HDI)) +
  geom_point(aes(color = Region, size =  HDI.Rank))
g3

##############
## Publication Quality Graphics     
##############

# One of the issues is creating publication quality graphs. 

# Use ggpubr to do this: https://rpkgs.datanovia.com/ggpubr/

#install.packages("ggpubr")
library("ggpubr")

# Control point size by continuous variable values ("qsec")
ggpubr::ggscatter(dat, x = "CPI", y = "HDI",
          color = "#00AFBB", size = "HDI.Rank")

# The same graph as before:
ggpubr::ggscatter(dat, x = "CPI", y = "HDI",
                  color = "Region", size = "HDI.Rank")

# The documentation for this is excellent: https://rpkgs.datanovia.com/ggpubr/reference/ggscatter.html
# These can be exported at 600 dpi.

##############
## Animated Gapminder     
##############

# Let's create the gapminder example together - look at life expectancy across time in regions:
# Open this and watch: https://www.ted.com/talks/hans_rosling_shows_the_best_stats_you_ve_ever_seen
# Code adapted from here: https://github.com/thomasp85/gganimate

### ANIMATED GGPLOT ##
#install.packages("gapminder")
#install.packages("gganimate")
#install.packages("gifski")

library(gifski)
library(gapminder)
library(gganimate)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

##############
## Interactive graphs in plotly
##############
#install.packages("plotly")
library(plotly)
p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
p

#p <- plotly::plot_ly(dat, x = "CPI", y = "HDI")
#p

##############
## Looking at our data     
##############
#install.packages("summarytools")
library(summarytools)
summarytools::descr(dat)
dfSummary(dat)

##############
## Mapping in R     
##############

library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map


# With just the key ideas of functions, arguments and libraries (packages), we can do so much! 

######
#### Optional - working with a larger dataset
######

# Download the data, and right click > Exract on Windows or double click on the zip file to extract on a Mac
# Source: http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip


# See here for why this has to be done with respect to Mac/Windows:
# https://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data


# Load text 
house_energy <- read.csv(file='Data/household_power_consumption.txt', sep=';')

# Check size
dim(house_energy)

nrow(house_energy)

# Let's say we want to run a regression

# First, lets check out the variables 

is.numeric(house_energy$Voltage)

# If not, we need to switch to numeric
house_energy$Voltage <- as.numeric(house_energy$Voltage)

# Numeric check
is.character(house_energy$Date)
is.factor(house_energy$Date)

# Extract year
house_energy$Year <- format(as.Date(house_energy$Date, format="%d/%m/%Y"),"%Y")


# Plot it 
active_voltage <- ggplot(house_energy, aes(x = Global_active_power, y = Voltage)) + geom_point() 
active_voltage

# Facet plot
active_voltage + facet_wrap( ~ Year, ncol= length(unique(electricity$Year)))

# Add regression line
active_voltage + geom_smooth(method = lm)

# Look at that regression 
linear_petite <- lm(data = electricity_petite, Voltage ~ Global_active_power)
summary(linear_petite)

# view the coefficients
summary(linear_petite)$coefficients

# Print results  
r_petite <- paste('R-squared = ', as.character(summary(linear_petite)[[9]]))

####------------------- OPTIONAL: Additional Questions from previous students  ------------------

# Question One: You might wonder - what is the difference between " = " and " " <- "? Well, that is a good question, and beyond the scope of this tutorial - but it's all about scope. 
# When you create a variable, it's always within some area that can access it, commonly called a scope. The scope of declaring the variable matters for when you can access it. <- puts a value in the user's workspace automatically which makes it accessible by that user anywhere, whereas = declares it in the scope of a function.
#See this link for more information: https://www.r-bloggers.com/assignment-operators-in-r-‘’-vs-‘-’/




