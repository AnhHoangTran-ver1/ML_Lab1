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