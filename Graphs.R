# A simple example
# Build a dataframe first using 3 vectors
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)

# Create the dataframe from vectors
drugs <- data.frame(dose, drugA, drugB)
str(drugs)

# Difficult to understand a general plot of data
plot(drugs)

# Show drugs data with circles for data points, joined with 
# a blue line
attach(drugs)
plot(dose, type = "o", col = "blue")

? plot
# option type="b" indicates that
# both points and lines should be
# plotted.
plot(dose, drugA, type = "b")
dose

# par function - customise many features of a graph 
# including (fonts, colours, axes, and labels) 
# using PAR() function
? par
# We can also plot more than 1
# line on the chart using par 

help(par)
# Note - parameters set with the par() function apply
# to all graphs, whereas parameters specified in the plot() 
# functions only apply to a specific graph

# adding no.readonly=TRUE produces a list
# of current graphical settings and can be modified

# Here I'm storing the contents of par to a variable
# and I can then restore defaults to contents of this variable
opar <- par(no.readonly = TRUE)
par(new = TRUE)
plot(dose, drugB, type = "b")

# lty = line type
# lwd = line width

# lty=2 dashed line
# pch=17 solid triangle
par(lty = 2, pch = 17)
plot(dose, drugA, type = "b")
par(opar)


-------------------------------------
  # Specify graphical parameters using 
  # optionname=value
  plot(dose, drugA, type = "b", lty = 2, pch = 17)

# dotted line, 3 times wider than default
# Connecting points are filled squares
plot(drugA, type = "b", lty = 3, lwd = 3, pch = 15, cex = 2, ylim = c(0, 100))
title(main = "Drug dosage", col.main = "blue", font.main = 4)

# Graph Drug B with red dashed line and square points
lines(drugB, type = "o", pch = 22, lty = 2, col = "red")

# Now we'll manually edit the chart so that it can automatically change to suit
# values in the vectors

# Calculate range from 0 to max value of drug A and drug B
graph_range <- range(0, drugA, drugB)
graph_range

# Graph drug A using y axis that ranges from 0 to max 
# value in drug A or drug B vector.  Turn off axes and 
# annotations (axis labels) so we can specify them ourself
plot(drugA, type = "o", col = "blue", ylim = graph_range, axes = FALSE, ann = FALSE)

# Graph Drug B with red dashed line and square points
lines(drugB, type = "o", pch = 22, lty = 2, col = "red")

# Make x axis with ml labels
axis(1, at = 1:5, lab = c("20 ml", "40 ml", "60 ml", "80 ml", "100 ml"))

# Make y axis with horizontal labels that display ticks at 
# every 5 marks. 5*0:graph_range[2] is equivalent to c(0, 5, 10, ..., 60).
axis(2, las = 1, at = 5 * 0:graph_range[2])

# Create box around plot
box()

# Label the x and y axes with dark green text
title(xlab = "Dosage (ml)", col.lab = rgb(0, 0.5, 0))
title(ylab = "Drugs", col.lab = rgb(0, 0.5, 0))

# Create a legend at (1, graph_range[2]) that is slightly smaller 
# (cex) and uses the same line colours and points used by 
# the actual plots 
legend(1, graph_range[2], c("Drug A", "Drug B"), cex = 0.8,
       col = c("blue", "red"), pch = 21:22, lty = 1:2)

# Create a title with a red, bold/italic font
title(main = "Drug dosage", col.main = "red", font.main = 4)

# Lets enhance the simple example
# Save current graphical parameter settings - restore later
opar <- par(no.readonly = TRUE)
# Modify default graphical parameters - graph is 2" wide & 3" tall
par(pin = c(2, 3))
# Lines will be twice the default width and 
# symbols will be 1.5 times default size
par(lwd = 2, cex = 1.5)
# Axis text set to italic and scaled to 75 % of the default
par(cex.axis = .75, font.axis = 3)
# first plot created using filled red circles and dashed lines
plot(dose, drugA, type = "b", pch = 19, lty = 2, col = "red")
# second plot is created using filled green diamonds
# and a blue border and blue dashed lines
plot(dose, drugB, type = "b", pch = 23, lty = 6, col = "blue", bg = "green")
# restore the original graphical parameter settings
par(opar)

# Complete example of creating a line chart including axes and labels
plot(dose, drugA, type = "b",
     col = "red", lty = 2, pch = 2, lwd = 2,
     main = "Clinical Trials for Drug A",
     sub = "This is hypothetical data",
     xlab = "Dosage", ylab = "Drug Response",
     xlim = c(0, 60), ylim = c(0, 70))

# use title() to add a title and axis labels to a plot
title(main = "My Title", col.main = "red",
      sub = "My Subtitle", col.sub = "blue",
      xlab = "My X label", ylab = "My Y label",
      col.lab = "green", cex.lab = 0.75)

# Custom axes example
# Specify data
x <- c(1:10)
y <- x
z <- 10 / x
# Copy current settings
opar <- par(no.readonly = TRUE)
# Increase margins
par(mar = c(5, 4, 4, 8) + 0.1)

# Advance to new graphics frame
plot.new()
# Create x and y elements
x <- c(1:10)
y <- 1 / x
# create new chart. Suppress annotations for y axis. 
# Show annotations for x-axis
plot(x, y, type = "b",
     pch = 21, col = "red",
     yaxt = "n", lty = 3, ann = FALSE)
? axis
# Draw the axes
# side 1 = x axis, 2 = y axis, 3 = top, 4 = right
# at = points when ticks are to be drawn
# las - labels are parallel (=0) or perpendicular(=2) to axis
axis(2, at = y, labels = y, col.axis = "red", las = 0, tck = .05)
# tck reference depth of tick mark
# cex.axis references text size

# Show axis on right of chart
axis(4, at = y, labels = round(y, digits = 2),
     col.axis = "blue", las = 0, cex.axis = 0.5, tck = .05)

# Add text to side = 4 axis
? mtext
mtext("y=1/x", side = 4, line = 3, cex.lab = 1, las = 2, col = "blue")
# Add titles and text
title("An Example of Creative Axes",
      xlab = "X values",
      ylab = "Y=X")

par(opar)

# adds solid horizontal lines at y = 1, 0.5, and 0.7,
abline(h = c(1, 0.5, 0.7))
? seq
# Draw vertical lines using the seq() function. This example draws a line from 1 to 10, in 0.5 increments
abline(v = seq(1, 10, 0.5), lty = 2, col = "blue")

# Example of drug data chart
# Input data
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly = TRUE)
# Increases line, text, symbol, and label size
par(lwd = 2, cex = 1.5, font.lab = 2)
# Generates the graph
plot(dose, drugA, type = "b",
     pch = 15, lty = 1, col = "red", ylim = c(0, 60),
     main = "Drug A vs. Drug B",
     xlab = "Drug Dosage", ylab = "Drug Response")
# Add a line
lines(dose, drugB, type = "b",
      pch = 17, lty = 2, col = "blue")
abline(h = c(30), lwd = 1.5, lty = 2, col = "gray")
# Adds minor tick marks
library(Hmisc)
minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)
# Adds a legend
legend("topleft", inset = .05, title = "Drug Type", c("A", "B")
       lty = c(1, 2), pch = c(15, 17), col = c("red", "blue"))
par(opar)
