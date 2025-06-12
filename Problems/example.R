# int whatever = 1; # this is the C-language version 

# For R, 
example <- 1 

example1 <- 10
example2 <- 12

track <- read.csv("")

# This is a function that takes two inputs and returns their sum
# inputs should be numeric or integer values 
addition_func <- function(input1, input2) { 
    # input1 + input2  # a + b = c
    
    sum <- input1 + input2  # a + b = c
    return(sum)
}  #scope

# void function 
void_addition_func <- function(input1, input2) { 
    sum <- input1 + input2  # a + b = c
    print(paste("The sum is:", sum))  # Print the result
}  #scope

# function calling: 2 way: with return and without return
# returning function call:
total_sum <- addition_func(example1, example2)
cat("The total sum is:", total_sum, "\n")

# print() this is method 

# strings:
# "This is a string "
# print("This is a string using print\n") # with newline print()
# cat("This is a string using cat", ) # without newline cat()