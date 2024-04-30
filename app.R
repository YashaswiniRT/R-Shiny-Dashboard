
#loading necessary libraries
library(readxl)
library(tm)        #used for text mining
library(wordcloud) #word-cloud generator
library(SnowballC) #Used for text stemming
library(stopwords) 
library(memoise)   #for cache
library(ggplot2)
library(dplyr)
library(RColorBrewer)#Used for color palletes
library(tidyr)
library(knitr)
library(kableExtra)
library(tidyverse) #used for everything
library(tm)        #Used for NLP 
library(tidytext)  #used for NLP 
library(openxlsx)  #Used to read in data
library(ggthemes)  #Used to make graphs look good
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(wordcloud2)
library(kableExtra)
library(ggthemes)
library(plotly)
library(DT)        #Used for rendering tables
library(rsconnect)
library(profvis)

# loading datasets for netlfix and disney plus data to be reviewed
netflix<- read.csv("netflix_titles.csv", na.strings = c(""))
disney<- read.csv("disney_plus_titles.csv", na.strings = c(""))

# Creating TV Show dataset and Movie dataset for Netflix
tv_netflix <- netflix %>% group_by(type) %>% filter(type == "TV Show")
movie_netflix <- netflix %>% group_by(type) %>% filter(type == "Movie")

# Creating TV Show dataset and Movie dataset for Disney+
tv_disney <- disney %>% group_by(type) %>% filter(type == "TV Show")
movie_disney <- disney %>% group_by(type) %>% filter(type == "Movie")

# creating a data frame
netflix$platform= c("Netflix")
disney$platform= c("Disney")
final_data= rbind(netflix,disney)

genre_sep = final_data %>% na.omit(listed_in) %>% separate_rows(listed_in, sep=",")
genre_sep = genre_sep %>% separate_rows(listed_in, sep = "&")
final_data_genre = genre_sep %>% group_by(genre_sep$listed_in) %>%
  summarize(number_of_films = n()) %>% arrange(desc(number_of_films))

netflix_genre = netflix %>% na.omit(listed_in) %>% separate_rows(listed_in, sep=",")
netflix_genre = netflix_genre %>% separate_rows(listed_in, sep = "&")

disney_genre = disney %>% na.omit(listed_in) %>% separate_rows(listed_in, sep=",")
disney_genre = disney_genre %>% separate_rows(listed_in, sep = "&")
disney_data_genre = disney_genre %>% group_by(disney_genre$listed_in) %>%
  summarize(number_of_films = n()) %>% arrange(desc(number_of_films))


# Define UI

ui<-shinyUI(
  fluidPage(theme = shinytheme("flatly"),
            
            navbarPage(
              "Streaming Sites Reviews",
              tabPanel("Main", 
                       # App title ----
                       titlePanel("Netflix Movies and TV Shows Reviews"),
                       # title image
                       div(windowTitle = "Streamingsite",
                       # disney vs netflix image displayed on top
                           img(src="disneyandnetflix2.jpg",
                               width="1300px",height="250px",
                               class="bg",alt="wrong",background = "black",
                               deleteFile=FALSE, align ="center"),
                       ),
                       # division window closed
                       
                       tags$br(),
                       
                       ## summary tab in Main
                       
                       tabsetPanel(
                         type = "tabs",
                         tabPanel(
                           "Summary",
                           
                           # Basic review of Netflix and Disney plus
                           
                           sidebarLayout(
                             sidebarPanel(
                               h3("Streaming platform"),
                               tags$br(),
                               selectInput(
                                 inputId = "selectPlatform",
                                 label = "Select the platform",
                                 choices = list("Netflix","Disney"),
                                 selected = "Netflix"
                               ) # input select closed
                             ),
                             #side bar panel closed
                             
                             mainPanel(
                               tabsetPanel(
                                 type = "tabs",
                                 tabPanel("Releases per Year",plotOutput(outputId = "histogram")),
                                 tabPanel("Top Genres", tableOutput("GenreHead")),
                                 tabPanel("Movies Vs Shows", plotOutput(outputId = "piePlot"))
                               ),
                               tags$br(),
                               tags$br(),
                             )
                           ),
                           tags$hr(),

                           sidebarLayout(
                             sidebarPanel(

                               # Data Overview

                               h3("Data Overview"),
                               tags$br(),
                               selectInput(
                                 inputId = "choosePlatform",
                                 label = "Select the platform",
                                 choices = list("Netflix","Disney"),
                                 selected = "Disney",
                                 multiple = TRUE
                               ),
                               setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
                               sliderInput(
                                 inputId = "yearRange",
                                 label = "Select year Range",
                                 min = 1925,
                                 max = 2021,
                                 value = c(1925, 2021)
                               ),
                               actionButton("actionDT", "Filter", class = "btn btn-warning"),
                             ),
                             mainPanel(
                               h3("Browse All"),
                               tags$br(),
                               dataTableOutput("myTable"),
                               tags$br(),
                               tags$br(),
                             )
                           ),
                           tags$hr(),
                         ),
                        
                         ## summary tab done
                         
                         ## visual review for netflix tab
                         
                         tabPanel(
                           "Netflix Visual Review",
                           
                           #density plot
                           
                           sidebarLayout(
                             sidebarPanel(
                               h3("Choose type"),
                               tags$br(),
                               selectInput(
                                 inputId = "selectType",
                                 label = "Choose the type of netflix movie or show",
                                 choices = c("Movie","TV Show"),
                                 selected = "Movie"
                               ), #type input close
                             ), # side bar panel layout
                             #main panel for graph
                             mainPanel(
                               h3("Distribution based on Genre"),
                               plotOutput(outputId = "densityPlot"),
                               tags$br(),
                               tags$br()
                             )
                           ),# side bar layout close

                           
                           
                           #second side bar layout
                           sidebarLayout(
                             sidebarPanel(
                               h3("Choose type"),
                               tags$br(),
                               selectInput(
                                 inputId = "selectType2",
                                 label = "Choose the type of netflix movie or show",
                                 choices = c("Movie","TV Show"),
                                 selected = "Movie"
                               ), #type input close
                               
                             ),
                             mainPanel(
                               h3("Frequency of run time"),
                               plotOutput(outputId = "FrequencyBar"),
                               tags$br(),
                               tags$br()
                             )
                           ),# frequency side bar layout close
                           tags$hr(),
                           
                           #third word cloud layout
                           sidebarLayout(
                             # Sidebar with a slider and selection inputs
                             sidebarPanel(
                               hr(),
                               h3("Choose type"),
                               tags$br(),
                               selectInput(
                                 inputId = "selectType3",
                                 label = "Choose the type of netflix movie or show",
                                 choices = c("Movie","TV Show"),
                                 selected = "Movie"
                               ), #type input close
                               sliderInput("freq",
                                           "Minimum Frequency:",
                                           min = 1,  max = 50, value = 5),
                               sliderInput("max",
                                           "Maximum Number of Words:",
                                           min = 1,  max = 300,  value = 100),
                               span(tags$i(h4("Slide through the minimum and maximum frequency to notice the changes in the most frequently used words in the reviews provided by the users in the form of wordclod")), style="color:#045a8d"),
                               
                               ),# side bar panel for word cloud close
                             mainPanel(
                               h3("Word cloud based on Type"),
                               plotOutput(outputId = "wordCloud"),
                               tags$br(),
                               tags$br()
                             )
                           ),# frequency side bar layout close
                           tags$hr(),

                         ),# netflix visual comparision tab closure
                         
                         ##################################
                         
                         ## visual review for disney tab
                         
                         tabPanel(
                           "Disney Visual Review",
                           
                           sidebarLayout(
                             sidebarPanel(
                               h3("Choose type"),
                               tags$br(),
                               selectInput(
                                 inputId = "selectType4",
                                 label = "Choose the type of disney movie or show",
                                 choices = c("Movie","TV Show"),
                                 selected = "Movie"
                               ), #type input close
                             ), # side bar panel layout
                             #main panel for graph
                             mainPanel(
                               h3("Distribution based on Genre"),
                               plotOutput(outputId = "disney_densityPlot"),
                               tags$br(),
                               tags$br()
                             )
                           ),# side bar layout close
                           
                           #second side bar layout
                           sidebarLayout(
                             sidebarPanel(
                               h3("Choose type"),
                               tags$br(),
                               selectInput(
                                 inputId = "selectType5",
                                 label = "Choose the type of disney movie or show",
                                 choices = c("Movie","TV Show"),
                                 selected = "Movie"
                               ), #type input close
                               
                             ),
                             mainPanel(
                               h3("Frequency of run time"),
                               plotOutput(outputId = "disney_FrequencyBar"),
                               tags$br(),
                               tags$br()
                             )
                           ),# frequency side bar layout close
                           tags$hr(),
                           
                           #third word cloud layout
                           sidebarLayout(
                             # Sidebar with a slider and selection inputs
                             sidebarPanel(
                               hr(),
                               h3("Choose type"),
                               tags$br(),
                               selectInput(
                                 inputId = "selectType6",
                                 label = "Choose the type of disney movie or show",
                                 choices = c("Movie","TV Show"),
                                 selected = "Movie"
                               ), #type input close
                               sliderInput("freq2",
                                           "Minimum Frequency:",
                                           min = 1,  max = 50, value = 5),
                               sliderInput("max2",
                                           "Maximum Number of Words:",
                                           min = 1,  max = 300,  value = 100),
                               span(tags$i(h4("Slide through the minimum and maximum frequency to notice the changes in the most frequently used words in the reviews provided by the users in the form of wordclod")), style="color:#045a8d"),
                               
                             ),# side bar panel for word cloud close
                             mainPanel(
                               h3("Word cloud based on Type"),
                               plotOutput(outputId = "disney_wordCloud"),
                               tags$br(),
                               tags$br()
                             )
                           ),# frequency side bar layout close
                           tags$hr(),
                           
                         ),# disney visual comparision tab clsoure
                         

                      ),# tabs in main menu 
                      
                       ## tabs are for summary, visual reviews of netflix and disney
                      
              ),
              # tabPanel close, Main tab close
          
              ################################################
              #### Panel: Documentation                   ####
              ################################################
              
              tabPanel("Documentation",
                       fluidPage(htmlOutput("doc"))),
              
              ################################################
              #### Panel: About                           ####
              ################################################
              tabPanel("About",
                       fluidPage(htmlOutput("abo")))    
            )
        )
  )
  

# Define server function
server<-shinyServer(function(input, output, session) {
  
  # summary tab

  output$histogram <- renderPlot({
    
    final_data %>%
      filter(final_data$platform == input$selectPlatform ) %>%
      ggplot(mapping = aes(x=release_year, fill = type))+
      geom_bar(color = "black", binwidth=2)+
      labs(title="Releases by Year", x="Release Year", y="Total Film")+
      scale_fill_manual(values = c("Movie" = "#F6AE2D","TV Show" = "#0770BB"))+
      theme_classic()

  })
  
  output$GenreHead <- renderPrint({
    
    final_genre = genre_sep %>%
      filter(genre_sep$platform == input$selectPlatform) %>%
      group_by(listed_in) %>%
      summarize(number_of_releases = n()) %>%
      arrange(desc(number_of_releases))
    kable(final_genre[1:10,],
          caption= "Top 10 Genres",
          col.names = c("Category","Number of Releases")
          )
  })
  
  output$piePlot <- renderPlot({
    colmap <- c("#bdb2ff","#ffc6ff")
                # NUS, NTU
    
    final_data %>%
      filter(final_data$platform == input$selectPlatform) %>%
      group_by(type) %>%
      summarize(number_of_type = n()) %>%
      mutate(Percentage = round(number_of_type/sum(number_of_type),digits=3)) %>%
      ggplot(aes(x="",y=Percentage, fill=type))+
      geom_bar(stat = "identity",width=1,color="black",size=1)+
      coord_polar("y", start=0)+geom_text(
        aes(label = Percentage),position = position_stack(vjust = 0.5)
        )+
      theme_void()+ theme(legend.position = "right",
              plot.title = element_text(hjust = 0.5, size = 14))+
      scale_fill_manual(values = c(colmap)) +
      labs(title = "Movies vs Shows")
      
  })
  
  # data overview preparation
  
  # filter the checkgroup input:
  
  platformGroup <- reactive({
    input$actionDT
    isolate(return(final_data[final_data$platform %in% input$choosePlatform, ]))
  })

  filtered_DT <- reactive({
    input$actionDT
    isolate({
      minYear <- input$yearRange[1]
      maxYear <- input$yearRange[2]
    })

    platformGroup() %>%
      filter(release_year>minYear,
             release_year<maxYear) %>% 
      dplyr::select(2,3,6,8,9,11)
  })
    
  # render DT:
  
  output$myTable <- renderDataTable({
    filtered_DT() %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = T),
        colnames = c(
          "Type",
          "Title",
          "Country",
          "Release Year",
          "Rating",
          "Genres"
        )
      )
    
  })
  
  
  #Netflix visual review
  
  # density plot for type
  
  output$densityPlot <- renderPlot({

    netflix_genre %>% filter(
      (netflix$type == input$selectType) &
      (listed_in %in%
        c(" International Movies","Dramas","Comedies"," Adventure","Action"," Family Movies"))) %>%
      ggplot(mapping = aes(x=listed_in,fill= listed_in))+geom_density()+theme_classic()

      })
  
  output$FrequencyBar <- renderPlot({
    
   netflix %>%
      filter(netflix$type == input$selectType2 ) %>%
      count(duration, sort = TRUE) %>% filter(n>=30) %>%
      arrange(duration) %>% 
      ggplot(aes(x=duration,y=n,fill=duration))+
      geom_col(show.legend = FALSE)+
      labs(x = NULL, y = "Frequency", title = "Frequency of Run Time")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
      scale_y_continuous(expand = c(0,0))+theme_classic()
    
  })
  
  output$wordCloud <- renderPlot({
    
    desc_words <- netflix %>% select(title,type, show_id, description) %>%
      filter(type == input$selectType3 ) %>%
      unnest_tokens(word, description) %>%
      anti_join(stop_words)

    count_word <- desc_words%>% count(word, sort = TRUE)


    wordcloud(words = count_word$word,
              scale = c(4,0.5),
              freq = count_word$n,
              min.freq = input$freq,
              max.words = input$max,
              random.order = FALSE,
              rot.per = 0.1,
              colors = brewer.pal(8, "Dark2"))
  
    })

  
  ################################################################
  
  # Disney visual review
  
  # density plot for type
  
  output$disney_densityPlot <- renderPlot({
    
    disney_genre %>% filter( 
      (disney$type == input$selectType4) &
        (listed_in %in%
           #" Family","Action-Adventure"," Comedy","Animation"," Fantasy","	Drama"
           c(" Family","Action-Adventure"," Comedy","Animation"," Fantasy","	Drama"))) %>%
      ggplot(mapping = aes(x=listed_in,fill= listed_in))+geom_density()+theme_classic()
  })
  
  output$disney_FrequencyBar <- renderPlot({
    
    disney %>%
      filter(disney$type == input$selectType5 ) %>%
      count(duration, sort = TRUE) %>% filter(n>=30) %>%
      arrange(duration) %>% 
      ggplot(aes(x=duration,y=n,fill=duration))+
      geom_col(show.legend = FALSE)+
      labs(x = NULL, y = "Frequency", title = "Frequency of Run Time")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
      scale_y_continuous(expand = c(0,0))+theme_classic()
    })
  
  output$disney_wordCloud <- renderPlot({
    
    desc_words <- disney %>% select(title,type, show_id, description) %>%
      filter(type == input$selectType6) %>%
      unnest_tokens(word, description) %>%
      anti_join(stop_words)
    
    count_word <- desc_words%>%
      count(word, sort = TRUE)
    
    
    wordcloud(words = count_word$word,
              scale = c(4,0.5),
              freq = count_word$n,
              # min.freq = 50,
              # max.words = nrow(count_word),
              min.freq = input$freq2,
              max.words = input$max2,
              random.order = FALSE,
              rot.per = 0.1,
              colors = brewer.pal(8, "Dark2"))
    })
  

  ################################################
  #### Panel: Documentation                   ####
  ################################################
  
  getPageDoc <- function() {
    return(includeHTML("ssrrmarkdown.html"))
  }
  output$doc <- renderUI({
    getPageDoc()
  })
  
  ################################################
  #### Panel: About                           ####
  ################################################
  
  getPageAbo <- function() {
    return(includeHTML("about.html"))
  }
  output$abo <- renderUI({
    getPageAbo()
  })
  
  })


# Create Shiny object
shinyApp(ui=ui,server=server)


