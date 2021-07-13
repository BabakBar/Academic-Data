#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(RColorBrewer)
library(shinythemes)



# Import data and clean/fix data - 4 tables. functions used are for US conventions
# for "." and "," usage

students <- read_csv2("Students.csv")
grades <- read_csv2("Grades.csv")
courses <- read.table("Courses.csv", sep = ";", dec=".", header = TRUE)
courses <- tibble(courses)
courses <- courses %>%
  mutate(Course_ID = gsub(" ", "", ï..Course_ID))
admissions <- read_csv2("Admissions.csv")
grades <- grades %>% filter(!is.na(Grade))

colnames(grades) <- c("Student_ID","Course_ID","Grade","Year")

table <- left_join(grades, courses %>% select(Course_ID, Course))

table1 <- admissions %>% pivot_longer(-Enrolment, names_to = "Condition", 
                                      values_to = "count") 
conditions <- unique(table1 %>% pull(Condition))
courses_available <- unique(table %>% pull(Course))

# Define UI 
ui <- dashboardPage(
    dashboardHeader(title = "MEM Academic Data"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Admissions", tabName = "admissions", icon = icon("clipboard-check")),
            menuItem("Courses", tabName = "courses", icon = icon("book")),
            menuItem("Grades", tabName = "grades", icon = icon("calculator")),
            menuItem("Students", tabName = "students", icon = icon("child"))
        )
    ),
    
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "admissions",
                    fluidRow(
                        box(plotOutput("plot1", height = 550), width = 12),
                    ),
                    
                    fluidRow(
                        box(
                            title = "Condition",
                            selectInput("Condition", h3("Select the condition:"), 
                                        choices = conditions)
                        )
                    ) 
            ),
        
        # Second tab content
        tabItem(tabName = "courses",
                fluidRow(
                    box(
                        title = "Course",
                        radioButtons("Course", h3("Select the course:"), 
                                    choices = courses_available)),
                    box(
                        title = "Average Grade",
                        textOutput("value"))
                        
                    )
                   
               ),
        # Third tab content
        tabItem(tabName = "grades",
                fluidRow(
                  column(12, plotOutput("plot3")),
                ),
                
                fluidRow(
                  column(8,selectInput("GCondition", label = h3("What would you like
                                                           to see the grades plotted by?"), 
                                  choices = list("Course", "Year", 
                                                  "Student", "Gender",
                                                  "Nationality"), selected = "Course"),
                  ),
                )
                
                      
        ),
        # Fourth tab content
        tabItem(tabName = "students",
                fluidRow(
                  
                  valueBoxOutput("avgAge"),
                  
                  valueBoxOutput("NumofEUStudents"),
                  
                  valueBoxOutput("NumofNonEUStudents"),
                ),
                fluidRow(
                  column(6, 
                         plotOutput("genderSplit")),
                  column(6,
                         plotOutput("AdmnbyYearandTerm")),
                  
                  
                  
                ),
                fluidRow(
                  column(7,
                         h6("")),
                  column(5,
                         h4("Countries Represented in Program")),   
                ),
                
                fluidRow(
                  column(6,
                         plotOutput("StudentNationality")),
                  column(6,
                         plotOutput("StudentCountry")),
                )
                
        )
        
            )
      )
    )
    



# Define server logic required to draw the plots 
server <- function(input, output) {
    
#BABAK'S PLOTS    
    
    output$plot1 <- renderPlot({
        
        table1 %>% 
            filter(Condition == input$Condition) %>%
            ggplot(aes(x=Enrolment, y= count)) + 
            geom_bar(position="dodge", stat="identity") +
            labs(x="Year", y= "Number of Students") +
            theme_minimal()
        
    })
    
    output$value <- renderPrint({
        
        paste(table %>% 
            filter(Course == input$Course) %>% 
            summarise(Avg_Grade = mean(Grade)))
    })   
    
       
    output$plot2 <- renderPlot({
        table %>% 
            filter(Course == input$course) %>%
            mutate(year = factor(Year)) %>%
            ggplot(aes(year, Grade)) +
            geom_boxplot(fill="grey") +
            theme_bw()
    })
    
#WILL'S PLOTS
    
    output$avgAge <- renderValueBox({
      AvgAge <- students %>%
        summarise(avg = mean(Age))
      
      AvgAge <- c(AvgAge)
      
      valueBox(AvgAge, "Average Age", icon = icon("users")
      )
    })
    
    output$NumofEUStudents <- renderValueBox({
      NumEU <- students %>%
        filter(Nationality == "EU") %>%
        summarise(n())
      
      valueBox(NumEU, "Number of EU Students", icon = icon("flag"))
    })
    
    output$NumofNonEUStudents <- renderValueBox({
      NumNonEU <- students %>%
        filter(Nationality == "NONEU") %>%
        summarise(n())
      valueBox(NumNonEU, "Number of Non EU Students", icon = icon("flag-checkered"))
    })
    
    
    output$genderSplit <- renderPlot({
      students %>% group_by(Gender) %>% summarise(N= n()) %>%
        ggplot(aes(x="", y=N, fill=Gender)) +
        geom_bar(stat="identity", width=1, color="white") +
        geom_text(aes(label = N), position = position_stack(vjust = 0.5)) +
        coord_polar("y", start=0) +
        scale_fill_brewer(palette="Reds") +
        theme_void()
    })
    output$AdmnbyYearandTerm <- renderPlot({
      students %>%
        group_by(Admission.Year,Admission.Month) %>%
        summarise(Count01 = n()) %>%
        ggplot(aes(x = Admission.Year,y=Count01,fill=Admission.Month)) +
        geom_bar(stat ="identity", 
                 position = position_dodge2(width = 1, preserve = "single")) +
        labs(y="# of Admissions") +
        labs(x="") +
        scale_fill_brewer(palette="Reds") +
        coord_flip() +
        theme(legend.position = "bottom") +
        scale_x_continuous(breaks = 0:2100)
      
    })
    output$StudentNationality <- renderPlot({
      students %>% group_by(Nationality) %>% summarise(N= n()) %>%
        ggplot(aes(x="", y=N, fill=Nationality)) +
        geom_bar(stat="identity", width=1, color="white") +
        geom_text(aes(label = N), position = position_stack(vjust = 0.5)) +
        scale_fill_brewer(palette="Reds") +
        theme_void()
    })
    output$StudentCountry <- renderPlot({
      countryCounts <- students %>% group_by(Country) %>% summarise(N= n()) 
      Countries <- students %>% pull(Country) %>% unique()
      
      wordcloud(countryCounts$Country, countryCounts$N, scale=c(4,0.5),
                min.freq = min(countryCounts$N), max.words=max(countryCounts$N),
                colors=brewer.pal(8, "Dark2"))
    })
    
    
#ALEX'S PLOTS    
    
    
   output$plot3 <- renderPlot({
      if (input$GCondition == "Course") {
        left_join(grades, courses %>% select(Course_ID, Course)) %>%
          group_by(Course) %>% 
          summarize(N = n(), Avg_Grade = mean(Grade)) %>%
          ggplot(aes(x= reorder(Course, -Avg_Grade), y= Avg_Grade, fill = as.factor(Course))) + 
          labs(x="Course", y= "Grades (Average)") +
          geom_bar(stat = "Identity") + 
          coord_flip() + 
          scale_fill_brewer(palette="Reds") +
          theme(legend.position = "none") +
          labs(x = "")
      }
      
      else 
        if (input$GCondition == "Year") {
        grades %>% group_by(Year) %>% 
          summarize(N = n(), Avg_Grade = mean(Grade)) %>%
          ggplot(aes(x=Year, y= Avg_Grade, fill = as.factor(Year))) + 
          geom_bar(position="dodge", stat="identity") +
          labs(x="Year", y= "Average Grade") + 
          scale_y_continuous(n.breaks = 8) +
          scale_fill_brewer(palette="Reds") +
          theme(legend.position = "none") +
          labs(x = "") 
      }
      
      else 
        if (input$GCondition == "Student") {
        nb.cols <- 20
        mycolors <- colorRampPalette(brewer.pal(9, "Reds"))(nb.cols)
        
        grades %>% group_by(Student_ID) %>% 
          summarize(N = n(), Avg_Grade = mean(Grade)) %>%
          ggplot(aes(x=as.factor(Student_ID), y= Avg_Grade, fill = as.factor(Student_ID))) + 
          geom_bar(position="dodge", stat="identity") +
          labs(x="Student ID", y= "Average Grade") + 
          coord_flip() + 
          scale_fill_manual(values = mycolors) +
          theme(legend.position = "none") 
      }
      
      else 
        if (input$GCondition == "Gender") {
        left_join(grades,students) %>%
          group_by(Gender,Year) %>%
          summarise(AvgGrade = mean(Grade),) %>%
          ggplot(aes(x=Year,y=AvgGrade,fill=Gender)) +
          geom_bar(position = "dodge",stat ="identity") +
          scale_fill_brewer(palette="Reds") +
          labs(x="")
      }
      
      else 
        if (input$GCondition == "Nationality") {
        nb.cols <- 20
        mycolors <- colorRampPalette(brewer.pal(9, "Reds"))(nb.cols)
        left_join(grades, students) %>%
          group_by(Country) %>% 
          summarize(N = n(), Avg_Grade = mean(Grade)) %>%
          ggplot(aes(x= Country, y= Avg_Grade, fill = as.factor(Country))) + 
          geom_bar(position="dodge", stat="identity") +
          labs(x="Country", y= "Average Grade") + 
          scale_y_continuous(n.breaks = 8) +
          coord_flip() + 
          scale_fill_manual(values = mycolors) +
          theme(legend.position = "none")
        }
   })
      
   # gradesplot <-  left_join(grades,students) %>%
   #   group_by(Gender,Year) %>%
   #   summarise(AvgGrade = mean(Grade),) %>%
   #   ggplot(aes(x=Year,y=AvgGrade,fill=Gender)) +
   #   geom_bar(position = "dodge",stat ="identity") +
   #   scale_fill_brewer(palette="Reds") +
   #   labs(x="") 
  
      
   
   
   #THIS TEST WORKED THUS IT IS AN ISSUE WITH THE IF LOGIC
      # output$plot3 <- renderPlot({   
      #   left_join(grades, courses %>% select(Course_ID, Course)) %>%
      #     group_by(Course) %>% 
      #     summarize(N = n(), Avg_Grade = mean(Grade)) %>%
      #     ggplot(aes(x= reorder(Course, -Avg_Grade), y= Avg_Grade, fill = as.factor(Course))) + 
      #     labs(x="Course", y= "Grades (Average)") +
      #     geom_bar(stat = "Identity") + 
      #     coord_flip() + 
      #     scale_fill_brewer(palette="Reds") +
      #     theme(legend.position = "none") +
      #     labs(x = "")
      # }) 
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
