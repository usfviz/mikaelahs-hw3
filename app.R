remove(list = ls())

library(shiny)
library(reshape2)
library(ggplot2)
library(plotly)
library(GGally)

data <- read.csv('dataset_Facebook.csv', sep=";")

bubble <- data[c(2,16,17,18)]
bubble <- bubble[complete.cases(bubble),]
bubble <- bubble[order(bubble$like, decreasing = T),]

multiple <- data[c(2,4,16,17,18)]
multiple <- multiple[complete.cases(multiple),]
multiple <- reshape2::melt(multiple, id.vars = c('Type','Post.Month'), variable.name = "interaction_type", value.name = "total_for_post")
for (interaction in c('share','comment','like')) {
  for (i in 1:12) {
    for (type in c('Status','Photo','Video','Link')) {
      rows <- which(multiple$Post.Month == i & multiple$Type == type & multiple$interaction_type == interaction)
      if (length(rows) == 0) {
        multiple[rows,5] <- 0
      }else {
        multiple[rows,5] <- mean(multiple[rows,4])
      }
    }
  }
}
multiple$total_for_post <- NULL
multiple <- unique(multiple)
multiple$Post.Month <- factor(multiple$Post.Month, labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", 
                                                              "October", "November", "December"))
multiple$interaction_type<- factor(multiple$interaction_type, labels = c("Comment", "Like", "Share"))

parallel <- data[c(2,7,16,17,18,19)]
parallel <- parallel[complete.cases(parallel),]
# parallel$Post.Month <- factor(parallel$Post.Month, labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September",
#                                                               "October", "November", "December"))
# parallel$Post.Weekday <- factor(parallel$Post.Weekday, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
# parallel$Post.Hour <- factor(parallel$Post.Hour, labels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM",
#                                                               "11 AM", "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", " 6 PM", "7 PM",
#                                                               "9 PM", "10 PM"))
# parallel$Paid <- factor(parallel$Paid, labels = c("No", "Yes"))

ui <- fluidPage(
  titlePanel(title = 'Visualizing Facebook Data'),
  conditionalPanel(
    condition="input.conditionedPanels==2", 
    helpText("Note that the months which are displayed can vary. This is because not all months have information for each post type.")),
  conditionalPanel(
    condition="input.conditionedPanels==2", 
    selectInput("type", label = "Select Post Type", 
                choices = list("Link" = "Link", "Photo" = "Photo", "Status" = "Status", "Video" = "Video"), selected = "Photo")),
  mainPanel(
    tabsetPanel(
      tabPanel(" Bubble Plot", plotlyOutput("bubble"), value = 1),
      tabPanel("Small Multiples", plotOutput("multiple"), value = 2),
      tabPanel("Parallel Coordinates Plot", plotlyOutput("parallel"), value = 3),
      id = "conditionedPanels"
    )
  )
)

server <- function(input, output) {
  output$bubble <- renderPlotly({
      g <- ggplotly(ggplot() +
        geom_point(data=bubble, 
                   aes(x=share, y=comment, fill=Type, size=like, text = paste("Shares: ", share, "<br>", "Comments: ", comment, "<br>", "Likes: ", like)), 
                   shape=21, stroke="black", alpha = 0.7) +
        xlab("Number of Shares") +
        ylab("Number of Comments") +
        scale_x_continuous(breaks = seq(0, 800, by = 100)) +
        scale_y_continuous(breaks = seq(0, 400, by = 50)) +
        scale_size(guide = 'none', range = c(1, 15)) +
        theme(panel.background = element_blank(),
            panel.grid.major = element_line(colour = "grey"),
            panel.grid.minor = element_line(colour = "grey"),
            panel.border = element_rect(colour="black", fill = NA),
            plot.margin = unit(c(1,1,1,1), "cm")) +
        scale_fill_manual(values=c("#e78ac3", "#8da0cb", "#66c2a5", "#fc8d62")), 
        tooltip = c("text"), height = 700, width = 900) 
      g
  })
  output$multiple <- renderPlot({
    multiple <- multiple[multiple$Type == input$type,]
    g2 <- ggplot(data=multiple, aes(x=interaction_type, y=V5)) +
      geom_bar(aes(fill=interaction_type), colour="black", stat="identity") +
      facet_wrap(~Post.Month, labeller = label_parsed) +
      ylab("Average Number of Interactions\n") +
      xlab("\nInteraction Type") +
      labs(colour = "legend title") +
      labs(fill = "Interaction Type") +
      theme(legend.background = element_rect(size=0.25, linetype="solid", colour ="black"),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 12),
            panel.background = element_blank(),
            panel.grid.major = element_line(colour = "grey", size = 0.25),
            panel.grid.minor = element_line(colour = "grey", size = 0.25),
            panel.border = element_rect(colour="black", fill = NA),
            strip.text = element_text(size = 12),
            strip.background = element_rect(colour = "black"),
            plot.margin = unit(c(1,1,1,1), "cm")) +
      scale_fill_manual(values=c("#fc8d62", "#8da0cb", "#66c2a5"))
    g2
  }, height = 700, width = 900)
  output$parallel <- renderPlotly({
    g3 <- ggplotly(ggparcoord(parallel, columns = 2:ncol(parallel), groupColumn = 1, scale = "uniminmax",
                              mapping=aes(text=value)) +
        xlab("Variable") +
        ylab("Scaled Value") +
        scale_x_discrete(labels = c("Paid", "Comments", "Likes", "Shares", "Total Interactions")) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
        theme(panel.background = element_blank(),
              panel.grid.major = element_line(colour = "grey"),
              panel.grid.minor = element_line(colour = "grey"),
              panel.border = element_rect(colour="black", fill = NA),
              plot.margin = unit(c(1,1,1,1), "cm")) +
        scale_colour_manual(values=c("#e78ac3", "#8da0cb", "#66c2a5", "#fc8d62")), 
        tooltip = c("text"), height = 700, width = 900)
    g3
  })
}

shinyApp(ui = ui, server = server)

