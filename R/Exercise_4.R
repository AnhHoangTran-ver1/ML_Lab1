<<<<<<< Updated upstream
# Exercise 4
# Part 2
# What ways of collecting correct values of the target variable for the
# supervised learning problems are mentioned in the book?
# historical data?

# The book mentions several ways of obtaining the correct target values. 
# One is manual labelling by a domain expert, for example cardiologists labelling 
# ECG signals (page 3, 16) or pixels in images labelled by hand for segmentation (page 8). 
# Another is computing them automatically, even if costly, as in the formation-energy example 
# where density functional theory calculations provided the outputs (page 6). 
# A third is getting them directly when the data is collected, such as match results from 
# historical soccer matches or prices from historical apartment sales (page 6), 
# or simply recording x and y jointly (page 16). 
# Finally, in self-supervised learning the targets come from the data itself, 
# for example using the next value of a time series as the output (page 38, 39).
=======
install.packages("tinytex")
tinytex::install_tinytex()
# Load the tinytex package
library(tinytex)


# Theory 3
# p 44
# Based on the chosen loss function, we also define the cost function as the average loss over the training data. Training a modelthen amountsto finding the
# parameter values that minimize the cost, the matrix form is on page 45, the 3.11 formula. The matrix form of the cost function is the following (wrote the formula with a latex code), 
# where the   ∥∥·∥∥2 denotes the usual Euclidean vector norm and its square

:
writeLines(c(
  "\\documentclass{article}",
  "\\usepackage{amsmath}",
  "\\usepackage{amssymb}",
  "\\begin{document}",
  "\\begin{equation}",
  "J(\\boldsymbol{\\theta}) = \\frac{1}{n}\\sum_{i=1}^{n}\\big(\\hat{y}(\\mathbf{x}_i;\\boldsymbol{\\theta}) - y_i\\big)^2",
  "= \\frac{1}{n}\\lVert \\hat{\\mathbf{y}} - \\mathbf{y} \\rVert_2^2",
  "= \\frac{1}{n}\\lVert \\mathbf{X}\\boldsymbol{\\theta} - \\mathbf{y} \\rVert_2^2",
  "= \\frac{1}{n}\\lVert \\boldsymbol{\\epsilon} \\rVert_2^2,",
  "\\end{equation}",
  "\\end{document}"
), "cost_function.tex")

tinytex::pdflatex("cost_function.tex")
>>>>>>> Stashed changes
