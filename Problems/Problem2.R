library(shiny)
library(DT)

ui <- fluidPage(
    DTOutput("table"),
    verbatimTextOutput("contingency_table")  # Add this to show contingency table
)

server <- function(input, output) {
    track <- read.csv("https://raw.githubusercontent.com/EricBrownTTU/ISQS5346/main/pgs.csv")
    
    output$table <- renderDT({
        datatable(track)
    })

    output$contingency_table <- renderPrint({

        # (a) Construct a contingency table for FacTeaching and COL variables
        contingency_table <- table(track$FacTeaching, track$COL)
        cat("(a) Contingency table for FacTeaching and COL:\n")
        print(contingency_table)


        # (b) use prop.table to create a contingency table of proportions
        # showing the same information as proportions so that all values sum to 1
        Contprop_table <- prop.table(contingency_table)
        cat("\n(b) Proportion table for FacTeaching and COL:\n")
        print(Contprop_table)

        # (c) Probability of being from College of Education (ED) and rating instructors at 4
        prob_ED_4 <- Contprop_table["4", "ED"]
        cat("\n(c) Probability of being from College of Education (ED) and rating instructors at 4:", prob_ED_4, "\n")

        # (d) Probability that a randomly selected student is from College of Business Administration (BA)
        prob_BA <- sum(Contprop_table[, "BA"])
        cat("(d) Probability that a randomly selected stuent is from College of Business Administration (BA):", prob_BA, "\n")

        # (e) Probability that a randomly selected student rated their instructors at a value of 2
        prob_rating_2 <- sum(Contprop_table["2", ])
        cat("(e) Probability that a randomly selected student rated their instructors at a value of 2:", prob_rating_2, "\n")

        # (f) Conditional probability given the fact that a student is from College of Arts and Sciences (AS) and rated their instructors at a value of 3
        condprob_AS_3 <- Contprop_table["3", "AS"] / sum(Contprop_table[, "AS"])
        cat("(f) Conditional probability given student is from College of Arts and Sciences (AS) and rated instructor value of 3:", condprob_AS_3, "\n")

        # (g) Conditional probability given the fact that instructors were rated at a value of 5 and the students was from College of Visual and Performing Arts (VPA)
        condprob_VPA_5 <- Contprop_table["5", "VPA"] / sum(Contprop_table["5", ])
        cat("(g) Conditional probability given instructor rating at 5 and student is from College of Visual and Performing Arts (VPA):", condprob_VPA_5, "\n")

        # (h) Assess quality of teaching between BA and ED using prop.table to create a contingency table of proportions
        quality_BA_ED <- prop.table(contingency_table[, c("BA", "ED")])
        cat("\n(h) Quality of teaching between BA and ED:\n")
        print(quality_BA_ED)
    })
    
}


shinyApp(ui, server)