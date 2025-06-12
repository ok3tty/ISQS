library(shiny)
library(DT)


ui <- fluidPage(
    DTOutput("table"),
    verbatimTextOutput("summary"),  # Add this to show summary stats
    verbatimTextOutput("myprintB"),  # Add this to show printed output
    verbatimTextOutput("myprintC"),   # Add this to show z-scores
    verbatimTextOutput("myprintD")   # Add this to show correlation matrix
)


server <- function(input, output) {
    track <- read.csv("https://raw.githubusercontent.com/EricBrownTTU/ISQS5346/main/menstrack.csv")
    
    output$table <- renderDT({
        datatable(track)
    })

    # (a) For the 100-meter dash and 200-meter dash, compute the following summary
    #     statistics: mean, median, variance, standard deviation

    # Create a function to compute summary statistics
    # mean() 
    # median()
    # data 
    # data[]

    # list of numbers 
    data <- array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    # data = [1,2,3,4,5,5]
    # data[2] = 3

    data <-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    summary_stats <- function(data, column) {
        mean_val <- mean(data[[column]], na.rm = TRUE)
        median_val <- median(data[[column]], na.rm = TRUE)
        variance_val <- var(data[[column]], na.rm = TRUE)
        sd_val <- sd(data[[column]], na.rm = TRUE)
        return(c(mean = mean_val, median = median_val, variance = variance_val, sd = sd_val))
    }


    output$summary <- renderPrint({
        hundred_meter_stats <- summary_stats(track, "m100")
        two_hundred_meter_stats <- summary_stats(track, "m200")
        list(
            Hundred_meter = hundred_meter_stats,
            Two_hundred_meter = two_hundred_meter_stats
        )
    })

    output$myprintB <- renderPrint({
        print("(b) Does the use of the mode on the above provide signiifcant analytical value? Why or why not?")
        print("the mode provides us data on the average time for the 100 and 200 meter dashes, but it does not provide us with a lot of information on the distribution of the data.")
        print("The mean, median, variance, and standard deviation give us a better understanding of the data's spread and central tendency.")
    })


    # Calculate z-scores for m100 and m200
    z_m100 <- (track$m100 - mean(track$m100, na.rm = TRUE)) / sd(track$m100, na.rm = TRUE)
    z_m200 <- (track$m200 - mean(track$m200, na.rm = TRUE)) / sd(track$m200, na.rm = TRUE)
    

    output$myprintC <- renderPrint({
        cat("z-scores for 100m:\n")
        print(z_m100)
        cat("\n z-scores for 200m:\n")
        print(z_m200)

        cat("\nMost extreme z-score for 100m:", max(abs(z_m100)), "\n")
        cat("Most extreme z-score for 200m:", max(abs(z_m200)), "\n")
        cat("\nWe should consider these values as potential outliers, as they are significantly different from the mean. Indicating the worst possible scenario of sprinters\n")

    })
    
    # Creating a three event correlation matrix
    # (c) Create a correlation matrix for the three events: 100m, 200m, and 3000m

    events <- cbind(track$m100, track$m200, track$m3000)
    correlation_matrix <- cor(events, use = "complete.obs")
    output$myprintD <- renderPrint({
        cat("Correlation matrix for the events:\n")
        print(correlation_matrix)
        cat("The correlation matrix shows how strongly the times for each event are related.\n")
        cat("usually, shorter sprints like the (100m, 200m) are highly correlated, while the 3000m (a long-distance event) may have a weaker correlation with the sprints.\n")
    })
}

shinyApp(ui, server)
