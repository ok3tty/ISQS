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

        # (a) Construct a contingency table for FacTeaching and COL
        contingency_table <- table(track$FacTeaching, track$COL)
        print(contingency_table)

        # (b) Construct a proportion table
        prop_table <- prop.table(contingency_table)
        print(prop_table)

        # (c) Probability of being from College of Education (ED) and rating instructors at 4
        prob_ED_4 <- prop_table["4", "ED"]
        cat("Probability of being from College of Education (ED) and rating instructors at 4:", prob_ED_4, "\n")

        # (d) Probability of being from College of Business Administration (BA)
        prob_BA <- sum(prop_table[, "BA"])
        cat("Probability of being from College of Business Administration (BA):", prob_BA, "\n")

        # (e) Probability of rating instructors at a value of 2
        prob_rating_2 <- sum(prop_table["2", ])
        cat("Probability of rating instructors at a value of 2:", prob_rating_2, "\n")

        # (f) Conditional probability given College of Arts and Sciences (AS) and rating at 3
        cond_prob_AS_3 <- prop_table["3", "AS"] / sum(prop_table[, "AS"])
        cat("Conditional probability given College of Arts and Sciences (AS) and rating at 3:", cond_prob_AS_3, "\n")

        # (g) Conditional probability given rating at 5 and College of Visual and Performing Arts (VPA)
        cond_prob_VPA_5 <- prop_table["5", "VPA"] / sum(prop_table["5", ])
        cat("Conditional probability given rating at 5 and College of Visual and Performing Arts (VPA):", cond_prob_VPA_5, "\n")

        # (h) Assess quality of teaching between BA and ED
        ba_ed_quality <- prop.table(contingency_table[, c("BA", "ED")])
        print(ba_ed_quality)
    })
    
}


shinyApp(ui, server)