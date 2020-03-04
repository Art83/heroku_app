#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("utils.R")

data <- read.csv("CompleteData.csv", stringsAsFactors = F)
complete_qc_batch_2 <- data %>% 
    group_by(PIN) %>% 
    group_map(~ qc_complete_participants(.x)) %>% 
    unlist()

complete_part_2 <- data.frame(PIN = unique(data$PIN), test = unlist(complete_qc_batch_2))
part <- complete_part_2$PIN[complete_part_2$test == 'Ok']
part <- c(part,43)
dat <- data %>%
    filter(!(PIN %in% part))


shinyServer(function(input, output) {
    output$plot <- renderPlot(
        if(input$typeInput == "time for everyone"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press') %>% 
                group_by(PIN) %>% 
                filter(row_number()==1 | row_number()==n()) %>%
                mutate(time_spent = timestamp - lag(timestamp)) %>% 
                filter(!is.na(time_spent)) %>% 
                ggplot(., aes(y = time_spent/1000, x = as.character(PIN))) +
                geom_bar(stat = 'identity') +
                labs(x = 'participant ID', y = 'Time spent on Key-testing')+
                theme(axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20),
                      axis.text = element_text(size=15))
        } else if (input$typeInput == "time without outliers"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press'& !PIN %in% c(34,46,64)) %>% 
                group_by(PIN) %>% 
                filter(row_number()==1 | row_number()==n()) %>%
                mutate(time_spent = timestamp - lag(timestamp)) %>% 
                filter(!is.na(time_spent)) %>% 
                ggplot(., aes(y = time_spent/1000, x = as.character(PIN))) +
                geom_bar(stat = 'identity') +
                labs(x = 'participant ID', y = 'Time spent on Key-testing')+
                theme(axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20),
                      axis.text = element_text(size=15))
        } else if (input$typeInput == "key strokes total"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press') %>% 
                group_by(PIN) %>% 
                summarise(count = n()) %>% 
                ggplot(., aes(y = count, x = as.character(PIN))) +
                geom_bar(stat = 'identity') +
                labs(x = 'participant ID', y = 'Number of key presses')+
                theme(axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20),
                      axis.text = element_text(size=15))
        } else if (input$typeInput == "key strokes without outliers"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press' & !PIN %in% c(34,46,64)) %>% 
                group_by(PIN) %>% 
                summarise(count = n()) %>% 
                ggplot(., aes(y = count, x = as.character(PIN))) +
                geom_bar(stat = 'identity') +
                labs(x = 'participant ID', y = 'Number of key presses')+
                theme(axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20),
                      axis.text = element_text(size=15))
        } else if (input$typeInput == "key strokes L&R total"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press') %>% 
                mutate(key_type = if_else(event.raw.details == "39", "right",
                                          if_else(event.raw.details == "37","left", "the rest"))) %>% 
                group_by(PIN) %>% 
                summarise(rest = sum(key_type == "the rest"), right = sum(key_type == "right"),
                          left = sum(key_type == "left")) %>%
                pivot_longer(-PIN, names_to = "type", values_to = "count") %>% 
                ggplot(., aes(x = as.character(PIN), y = count, fill = factor(type, levels = c("left","right","rest")) ))+
                geom_bar(position="dodge", stat="identity")+
                labs(x = 'participant ID', y = 'Number of key presses', fill = "key type")+
                theme(axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20),
                      axis.text = element_text(size=15),
                      legend.title = element_text(size=18),
                      legend.text = element_text(size=18))
        } else if (input$typeInput == "key strokes L&R without outliers"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press'& !PIN %in% c(34,46,64)) %>% 
                mutate(key_type = if_else(event.raw.details == "39", "right",
                                          if_else(event.raw.details == "37","left", "the rest"))) %>% 
                group_by(PIN) %>% 
                summarise(rest = sum(key_type == "the rest"), right = sum(key_type == "right"),
                          left = sum(key_type == "left")) %>%
                pivot_longer(-PIN, names_to = "type", values_to = "count") %>% 
                ggplot(., aes(x = as.character(PIN), y = count, fill = factor(type, levels = c("left","right","rest")) ))+
                geom_bar(position="dodge", stat="identity")+
                labs(x = 'participant ID', y = 'Number of key presses',fill = "key type")+
                theme(axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20),
                      axis.text = element_text(size=15),
                      legend.title = element_text(size=18),
                      legend.text = element_text(size=18))
        }
    )
    output$tab <- renderTable(
        if(input$typeInput == "time for everyone"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press') %>% 
                group_by(PIN) %>% 
                filter(row_number()==1 | row_number()==n()) %>%
                mutate(time_spent = timestamp - lag(timestamp)) %>% 
                filter(!is.na(time_spent)) %>%
                ungroup() %>% 
                select(PIN, time_spent) %>% 
                mutate(PIN = factor(PIN))
        } else if (input$typeInput == "time without outliers"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press'& !PIN %in% c(34,46,64)) %>% 
                group_by(PIN) %>% 
                filter(row_number()==1 | row_number()==n()) %>%
                mutate(time_spent = timestamp - lag(timestamp)) %>% 
                filter(!is.na(time_spent)) %>% 
                ungroup() %>% 
                select(PIN, time_spent) %>% 
                mutate(PIN = factor(PIN))
        } else if (input$typeInput == "key strokes total"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press') %>% 
                group_by(PIN) %>% 
                summarise(count = n())
        } else if (input$typeInput == "key strokes without outliers"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press' & !PIN %in% c(34,46,64)) %>% 
                group_by(PIN) %>% 
                summarise(count = n()) %>% 
                mutate(PIN = factor(PIN))
        } else if (input$typeInput == "key strokes L&R total"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press') %>% 
                mutate(key_type = if_else(event.raw.details == "39", "right",
                                          if_else(event.raw.details == "37","left", "the rest"))) %>% 
                group_by(PIN) %>% 
                summarise(rest = sum(key_type == "the rest"), right = sum(key_type == "right"),
                          left = sum(key_type == "left")) %>% 
                mutate(PIN = factor(PIN))
        } else if (input$typeInput == "key strokes L&R without outliers"){
            dat %>% 
                filter(stage == "Key-testing" & event.type == 'key press'& !PIN %in% c(34,46,64)) %>% 
                mutate(key_type = if_else(event.raw.details == "39", "right",
                                          if_else(event.raw.details == "37","left", "the rest"))) %>% 
                group_by(PIN) %>% 
                summarise(rest = sum(key_type == "the rest"), right = sum(key_type == "right"),
                          left = sum(key_type == "left")) %>% 
                mutate(PIN = factor(PIN))
        }
    )
})
