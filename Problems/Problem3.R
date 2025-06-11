# suppose you have five coins numbered 1-5. Further suppose that when coin 𝑖 is flipped it 
# shows heads with probability i/5

# we can generate all possible combinations of values of each of the five coin tosses as follows.

c1 <- c(0,1) # possible outcomes from flipping coin 1
c2 <- c(0,1) # possible outcomes from flipping coin 2
c3 <- c(0,1) # possible outcomes from flipping coin 3
c4 <- c(0,1) # possible outcomes from flipping coin 4
c5 <- c(0,1) # possible outcomes from flipping coin 5

# creating a set of all combinations, note that we could just use
# expand.grid(c1,c2,c3,c4,c5) but the below context let's us have colum names
x <- expand.grid(c1 = c1,c2 = c2, c3 = c3, c4 = c4, c5 = c5)


# (a) Assuming that all five coins were fair coins (i.e., flipped heads with problability 0.5),
#     use R to determine the expected number of heads in the five coin tosses.

num_heads <- rowSums(x)  # Calculate the number of heads in each combination
expected_heads <- mean(num_heads)  # Calculate the expected number of heads
cat("Expected number of heads when all coins are fair (0.5 probability):", expected_heads, "\n")

# (b) simulate 10,000 trails of the above experiment flipping five coins. Then, use rowSums() to obtain the number of heads for each of your 10,000 trials.
results <- data.frame(matrix(nrow = 10000, ncol = 5))
results[,1] <- sample(c(0,1), size = 10000, replace = TRUE, prob = c(1 - (1/5), 1/5))
results[,2] <- sample(c(0,1), size = 10000, replace = TRUE, prob = c(1 - (2/5), 2/5))
results[,3] <- sample(c(0,1), size = 10000, replace = TRUE, prob = c(1 - (3/5), 3/5))
results[,4] <- sample(c(0,1), size = 10000, replace = TRUE, prob = c(1 - (4/5), 4/5))
results[,5] <- sample(c(0,1), size = 10000, replace = TRUE, prob = c(1 - (5/5), 5/5))

num_heads <- rowSums(results)  # Using rowSums() to calculate the number of heads in each trial
# cat("Number of heads in each of the 10,000 trials:", num_heads, "\n")

# (c) create a histogram of the results of your experiment with appropriate labels and plot title. Comment on what you see. Why are there no outcomes associated with zero heads
hist(num_heads, breaks = seq(-0.5, 5.5, 1), main = "Histogram of Number of Heads in 10,000 Trials", xlab = "Number of Heads", ylab = "Frequency", col = "lightblue", border = "black")
cat("Comment: The histogram shows the distribution of the number of heads obtained in the 10,000 trials.\m")
cat("\t There are no outcomes with zero heads because coin 5 always shows heads with\nprobability 1 this makes sure that at least one head is present in every trial.\n")

# (d) suppose one coins is randomly selected and flipped. From your experimental results.
#     what is the probablity that it is coin 4 given that the result is heads?
prob_coin4 <- sum(results[,4] == 1 & rowSums(results) > 0) / sum(rowSums(results) > 0)
cat("Probability that the selected coin is coin 4 given that the result is heads:", prob_coin4, "\n")








