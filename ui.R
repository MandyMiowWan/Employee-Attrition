library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(shinycssloaders)
# library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(reshape2)
library(forcats)
library(tidyquant)
#  --------------------------------------------------------------------------------------------------------
#                                              READING THE FILES
#  --------------------------------------------------------------------------------------------------------

employee= read.csv("data/EmployeeAttrition.csv")


# ---------------------------------------------------------------------------------------------------------
#                                                USER INTERFACE
# ---------------------------------------------------------------------------------------------------------
header <- dashboardHeader(title = "WQD7001")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard",
             tabName = "dashboard",
             icon = icon("chart-line")),
    menuItem("Correlation Matrix",
             tabName = "correlation",
             icon = icon("th")),
    menuItem("Raw Data",
             tabName = "rawdatatable",
             icon = icon("table")),

    
    # ------------- select Input -------------
    
    selectInput("attrition",
                "Select Attrition :",
                choices = c("All","Yes", "No")),
    
    selectInput("gender",
                "Select Gender:",
                choices = c("All","Male", "Female")),
    
    selectInput("age",
                "Select Age Group:",
                choices = c("All","21 - 30", "31 - 40","41 - 50","> 50")),
    
    selectInput("maritalstatus",
                "Select Marital Status:",
                choices = c("All","Single", "Married", "Divorced")),
    
    selectInput("education",
                "Select Education Level:",
                choices = c("All","Below College", "College", "Bachelor", "Master", "Doctor")),
    
    
    menuItem("Author",
             tabName = "author",
             icon = icon("user"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(
      HTML("#compare_state_option,#compare_year_option ,.compare-county-wrapper { display:flex; margin-bottom:-10px;}
    				#compare_state_option > div, #compare_year_option > div, .compare-county-wrapper > div > div {padding-right: 15px;}
    				.shiny-output-error:before,.shiny-output-error{visibility: hidden !important;}
    				.compare-class > .tabbable > .nav-tabs {margin-bottom: 20px !important;}
            .box.box-solid.box-primary {border: 1px solid #5E35B1 !important;}
				    .box.box-solid.box-primary>.box-header { background: #5E35B1 !important; background-color: #5E35B1 !important; }
				    .sidebar-menu>li {font-size:17px;}
          ")
      )
    ),

  #--------------------------------------------Menu Item---------------------------------------------------------
  tabItems(

    # ---------------------------------------------DASHBOARD TAB-------------------------------------------------------------
    
    tabItem(tabName = "dashboard",
            # fluidRow(
            tags$h1("Employee Attrition Analysis"),
            tags$h3("Data Exploration"),
              fluidRow(
                column(4,
                       box(title = "Employee Churn Rate", solidHeader = TRUE, status = "primary", width = 10, color = "purple",
                           withSpinner(plotOutput("attrition"))
                           )
                       ),
                column(4,
                       box(title = "Education Level", solidHeader = TRUE, status = "primary", width = 10,
                           withSpinner(plotOutput("education"))
                       )
                ),
                column(4,
                       box(title = "Marital Status of Employee", solidHeader = TRUE, status = "primary", width = 10,
                           withSpinner(plotOutput("maritalstatus"))
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(title = "Types of Job Role", solidHeader = TRUE, status = "primary", width = 12,
                           withSpinner(plotOutput("jobrole"))
                       )
                ),
                
                column(6,
                       box(title = "Gender Against Attrition", solidHeader = TRUE, status = "primary", width = 12,
                           withSpinner(plotOutput("attritionvsgender"))
                       )
                )
                
              ),
              fluidRow(
                column(10,
                       box(title = "Monthly Income Level of different Department", solidHeader = TRUE, status = "primary", width = 12,
                           withSpinner(plotOutput("incomedepartment"))
                       )
                )
              ),
              fluidRow(
                  column(6,
                         box(title = "Years Since Promotion and Performance Rating", solidHeader = TRUE, status = "primary", width = 12,
                             withSpinner(plotOutput("promotionperformance"))
                         )
                  ),
                  column(6,
                         box(title = "Percent Salary Hike VS Job Involvement", solidHeader = TRUE, status = "primary", width = 12,
                             withSpinner(plotOutput("salaryhike"))
                         )
                  )
                ),
              fluidRow(
                column(6,
                       box(title = "Marital Status Against Business Travel", solidHeader = TRUE, status = "primary", width = 12,
                           withSpinner(plotOutput("maritalvstravel"))
                       )
                ),
                column(6,
                       box(title = "Work Life Balance Against Over Time", solidHeader = TRUE, status = "primary", width = 12,
                           withSpinner(plotOutput("worklifebalance"))
                       )
                )
              )
              
            # )
    ),
    
    # ---------------------------------------------CORRELATION TAB------------------------------------------------------------- 
    tabItem(tabName = "correlation",
            
            tags$h3("Correlation Matrix"),
            fluidRow(
              column(12,
                     # box(title = "Correlation Matrix", solidHeader = TRUE, status = "primary", width = 8,
                         withSpinner(plotOutput("correlationmatrix"))
                     # )
              )
            )
            
    ),
    #--------------------------------------------RAW DATA TAB---------------------------------------------------------
    tabItem(tabName = "rawdatatable",
            tags$h3('Download Data'),
            downloadButton("downloadData"),
            br(),
            br(),
            tableOutput("tableData")),
    #--------------------------------------------Author TAB---------------------------------------------------------
    tabItem(tabName = "author",
            tags$h3('Author'),
            
            fluidRow(
              column(4,
                     box(title = "Group 6: The Roku Group", solidHeader = TRUE, status = "primary", width = 10,
                         HTML('<html>
                            <head>
                            <style>
                            table {
                              font-family: arial, sans-serif;
                              border-collapse: collapse;
                              width: 100%;
                            }

                            td, th {
                              border: 1px solid #dddddd;
                              text-align: left;
                              padding: 8px;
                            }

                            
                            p{
                              font-size: 19px;
                            }
                            </style>
                            </head>
                            <body>
                                <table border="0" width="50%">
                                    <col style="width:30%">
	                                  <col style="width:70%">

	                                  <tr bgcolor="#dddddd">
	                                      <td>Name</td>
	                                      <td>Matric Number</td>
                                    </tr>
                                    
                                    <tr>
	                                      <td>Lim Jie Ying</td>
	                                      <td>S2101965</td>
                                    </tr>

                                    <tr>
	                                      <td>Yin Khar Shin</td>
	                                      <td>S2145827</td>
                                    </tr>

                                    <tr>
	                                      <td>Gui Miow Wan</td>
	                                      <td>S2118013</td>
                                    </tr>
                                    
                                    <tr>
	                                      <td>Ooi Shi Yuan</td>
	                                      <td>17088226</td>
                                    </tr>
                                    
                                    <tr>
	                                      <td>Sim Lin Zheng</td>
	                                      <td>S2102170</td>
                                    </tr>
                                </table>
                            </body>
                            </html>'))
                     )
              )
            
      )
  )
  
)
ui <- dashboardPage(skin = 'purple', header, sidebar, body)

# -------------------------------------------------------------------------------------------------------------------------------------
#                                                   SERVER
# -------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {

  educationlevel <- reactive({
      switch(input$education, 
           "All" = 0,
           "Below College" = 1,
           "College" = 2,
           "Bachelor" = 3,
           "Master" = 4,
           "Doctor" = 5
      )
  })

  ageGroup <- reactive({
    switch(input$age, 
           "All" = "All",
           "21 - 30" = "21-30 yrs old",
           "31 - 40" = "31-40 yrs old",
           "41 - 50" = "41-50 yrs old",
           "> 50" = ">50 yrs old"
    )
  })
  
  employee_subset <- reactive({
                    subset(employee, (input$gender == "All" | Gender == input$gender) & 
                   (input$attrition == "All" | Attrition == input$attrition) & 
                   (input$maritalstatus == "All" | MaritalStatus == input$maritalstatus) & 
                   (educationlevel() == 0 | Education == educationlevel()) & 
                   (ageGroup() == "All" | Age_Band == ageGroup())
  ) })
  
  monthlyIncome_Band.order <- reactive({c("RM0-RM5000","RM5001-RM10000","RM10001-RM15000",">RM15000")})
 
  #-------------------------------------Employee Churn Rate (pie chart)----------------------------------------------
  output$attrition <- renderPlot({
    # print(educationlevel())
    attrition_subset <- employee_subset() %>%
                      select("Attrition") %>%
                      group_by(Attrition) %>%
                      mutate(cnt = n())  %>%
                      unique %>% ungroup  %>%
                      mutate(percent = round(cnt / nrow(employee_subset()) *100,1)) 
    # print(nrow(employee_subset()))
    # print(nrow(attrition_subset))
    #------Plot------
    attrition_subset %>%
      ggplot(aes(x = '',y = percent,fill = Attrition)) +
      geom_bar(width=1,stat='identity',color="white") +
      geom_text(aes(label=cnt),
                position = position_stack(vjust = 0.5)) +
      theme_minimal()+
      # theme(legend.position = "top") +
      guides(fill=guide_legend(title="Attrition")) +
      coord_polar(theta = "y",start=0,direction=-1) +
      theme(axis.line = element_blank()) +
      theme(axis.text = element_blank() ) +
      theme(axis.ticks = element_blank()) +
      labs(x="",y="") #,title="Employee Churn Rate"
    
  })
  #----------------------------------------EDUCATION - PIE CHART-----------------------------------------------------
  output$education <- renderPlot({
  # Pre-process the DF for the piechart
  
  # ibm_pie <- educationlevel()["Education"]
  ibm_pie<- employee_subset() %>%
    select("Education")
  # print(nrow(ibm_pie))
  ibm_pie$EducationLevel <- ifelse(ibm_pie$Education=="1","Below College",
                                   ifelse(ibm_pie$Education=="2","College",
                                          ifelse(ibm_pie$Education=="3","Bachelor",
                                                 ifelse(ibm_pie$Education=="4","Master",
                                                        ifelse(ibm_pie$Education=="5","Doctor",
                                                               NA)))))

  # Check for NA
  sum(is.na(ibm_pie))

  # Create the final DF for the pie chart
  ibm_piepie <- data.frame(table(ibm_pie$EducationLevel))
  ibm_piepie

  # Generate the pie chart
  ggplot(ibm_piepie, aes(x="", y=Freq, fill=Var1)) +
    geom_bar(stat="identity", width=1,color="white") +
    geom_text(aes(label=Freq),
              position = position_stack(vjust = 0.5)) +
    coord_polar("y", start=0,direction=-1) +
    guides(fill=guide_legend(title = "Education Level")) +
    theme_minimal() +
    theme(axis.line = element_blank()) +
    theme(axis.text = element_blank() ) +
    theme(axis.ticks = element_blank())+
    labs(x="",y="") #,title="Education Level"
  
  })
  
  #------------------------------Marital Status (pie chart)---------------------------------------
  output$maritalstatus <- renderPlot({
    maritalstatus_count<-employee_subset() %>%
      count(MaritalStatus) %>%
      group_by(MaritalStatus) %>%
      mutate(cnt=n()) %>%
      unique() 
    
    ggplot(maritalstatus_count,aes(x='',y=n,fill=MaritalStatus))+
      geom_bar(width=1,stat='identity',color="white")+
      # geom_text(aes(label=n),vjust=-1)+
      geom_text(aes(label=n), position = position_stack(vjust = 0.5)) +
      coord_polar(theta="y", start=0,direction=-1)+
      theme_minimal()+
      labs(x="",y="")+
      #,title="Marital Status of Employee"
      # theme(legend.position = "none")+
      guides(fill=guide_legend(title="Marital Status")) +
      theme(axis.line = element_blank()) +
      theme(axis.text = element_blank() ) +
      theme(axis.ticks = element_blank())
    
  })
  #---------------------------Job Role (bar chart)-------------------------------------------------
  output$jobrole <- renderPlot({
  jobrole_count<-employee_subset() %>%
    count(JobRole) %>%
    group_by(JobRole) %>%
    mutate(cnt=n()) %>%
    unique()
  
  ggplot(jobrole_count,aes(x=reorder(JobRole,n),y=n,fill=JobRole))+
    geom_bar(stat="identity",width=0.5)+
    labs(x="",y="")+ #,title="Types of Job Role"
    geom_text(aes(label=n),hjust=-0.3)+
    theme_minimal()+
    theme(legend.position = "none")+
    coord_flip()
  })
  #-------------------------Monthly Income Level of different Department  (bar chart)------------------------------------
  
  output$incomedepartment <- renderPlot({
    
    incomedepartment <- employee_subset() %>%
      select("Department","MonthlyIncome_Band")  %>%
      mutate (MonthlyIncome_Band = factor(MonthlyIncome_Band,levels = monthlyIncome_Band.order())) %>%
      count(Department,MonthlyIncome_Band)
    
    
    incomedepartment%>%
      ggplot(aes(x=Department,y=n,fill=MonthlyIncome_Band))+
      geom_bar(width=.5,stat='identity',position = "dodge")+
      geom_text(aes(label=n),position=position_dodge(width=0.6), hjust=-0.3)+
      theme_minimal()+
      coord_flip()+
      labs(y ="Count",fill='Monthly Income') #,title="Monthly Income Level of different Department"
    
  })
  # -------------------YEARS SINCE PROMOTION AND PERFORMANCE RATING - GROUPED BAR CHART---------------------------------
  
  output$promotionperformance <- renderPlot({
  # Pre-process the DF for the grouped bar chart

  ibm_rate <- subset(employee_subset(), select = c(31,45))
  ibm_rate
  
  ibm_rating <- data.frame(table(ibm_rate))
  ibm_rating
  
  # Rename the values accordingly in the DF
  ibm_rating$PerformanceRating <- ifelse(ibm_rating$PerformanceRating=="3","Excellent",
                                         ifelse(ibm_rating$PerformanceRating=="4","Outstanding",       
                                                NA))
  # Plot the chart
  
  p <- ggplot(ibm_rating, aes(fill=PerformanceRating, y=Freq, x=YearsSinceLastPromotion_Band)) + 
    geom_bar(position="dodge", stat="identity")
  
  
  # reorder in descending order
  p + aes(x = fct_reorder(YearsSinceLastPromotion_Band, Freq, .desc = TRUE)) + 
    labs(x = "" ,y ="Count")  + #,title="Years Since Promotion and Performance Rating"
    theme_minimal() +
    guides(fill=guide_legend(title="Performance Rating")) +
    geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.3)
  })
  
  # -------------------# PERCENT SALARY HIKE VS JOB INVOLVEMENT - GROUPED BAR CHART---------------------------------
  
  output$salaryhike <- renderPlot({
  # Pre-process the DF for the grouped bar chart
  
  ibm_hike <- subset(employee_subset(), select = c(17,30))
  ibm_hike
  
  ibm_hiking <- data.frame(table(ibm_hike))
  ibm_hiking
  
  # Rename the values accordingly in the DF
  ibm_hiking$JobInvolvement <- ifelse(ibm_hiking$JobInvolvement=="1","Low",
                                      ifelse(ibm_hiking$JobInvolvement=="2","Medium",       
                                             ifelse(ibm_hiking$JobInvolvement=="3","High",     
                                                    ifelse(ibm_hiking$JobInvolvement=="4","Very High",
                                                           NA))))
  
  # Plot the chart
  Q <- ggplot(ibm_hiking, aes(fill=PercentSalaryHike_Band, y=Freq, x=JobInvolvement)) + 
    geom_bar(position="dodge", stat="identity")
  
  Q
  Q + aes(x = fct_inorder(JobInvolvement)) + 
    labs(x = "" ,y ="Count")  + #,title="Percent Salary Hike VS Job Involvement"
    theme_minimal() +
    guides(fill=guide_legend(title="Salary Hike Percentage"))+
    geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.3)
  
  })
  
    
  #-------------------------Work life balance vs over time (bar chart)-----------------------------------
  output$worklifebalance <- renderPlot({
  worklifebalance_ot<-employee_subset() %>%
    count(WorkLifeBalance,OverTime) %>%
    group_by(WorkLifeBalance)%>%
    mutate(cnt=n()) %>%
    unique()
  
  ggplot(worklifebalance_ot,
         aes(x=WorkLifeBalance,y=n,
             fill=OverTime))+
    geom_bar(position="dodge",stat="identity")+
    labs(x="Work Life Balance",y="Count of Employee",
         # title = "Work Life Balance Against Over Time",
         fill="Over Time Scale")+
    theme_minimal()+
    geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.3)
  })
  #-------------------------marital status vs business travel (bar chart)-----------------------
  
  output$maritalvstravel <- renderPlot({
  maritalstatus_businesstravel<-employee_subset() %>%
    count(MaritalStatus,BusinessTravel) %>%
    group_by(MaritalStatus,BusinessTravel) %>%
    mutate(cnt=n()) %>%
    unique() 
  
  ggplot(maritalstatus_businesstravel,aes(x=MaritalStatus,y=n,fill=BusinessTravel))+
    geom_bar(position="dodge",stat="identity")+
    labs(x="Marital Status",y="Count of Employee",
         # title = "Marital Status Against Business Travel",
         fill="Status")+
    theme_minimal()+
    geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.3)
  })
  #-----------------------------attrition vs gender (stacked bar chart)-------------------------
  output$attritionvsgender <- renderPlot({
  attrition_gender<-employee_subset()  %>%
    count(Attrition,Gender) %>%
    group_by(Attrition,Gender) %>%
    mutate(cnt=n()) %>%
    unique() 
  
  ggplot(attrition_gender,aes(x=Gender,y=n,fill=Attrition))+
    geom_bar(stat="identity")+
    labs(x="Gender",y="Employee Attrition",
         # title = "Gender Against Attrition",
         fill="Attrition")+
          theme_minimal()+
    geom_text(aes(label=n),vjust=1.5)
  })
  
  # ---------------------------------------- CORRELATION MATRIX-----------------------------------------
  
  output$correlationmatrix <- renderPlot({
  # Drop categorical data and banding
  
  ibmcor <- subset(employee_subset(), select = -c(2,3,4,6,8,10,11,12,14,16,19,21,23,26,27,28,30,33,36,38,41,43,45,47)) 
  ibmcor
  
  
  # Generate a correlation matrix
  
  ibmcormat <- round(cor(ibmcor),2)
  ibmcormat
  
  # Use reshape2 melt function to stretch our DF for heatmap
  
  melted_ibmcormat <- melt(ibmcormat)
  melted_ibmcormat
  
  
  # Plot the Heatmap
  
  ggplot(melted_ibmcormat, aes(x=Var1, y=Var2, fill=value )) + 
    geom_tile()+scale_fill_gradient(high = "blue", low = "lightblue") +
    geom_text(aes(label=round(value,2)),size=3) +
    theme_tq() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
    ) +
    labs(x="",y="",
         title = "Correlation Matrix of Employee Attrition")
  
  
  }, width =1300, height = 800)
  
  #------------------------------------------------------------RAW DATA------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename=function(){
      paste("employee_data","csv", sep = '.')
    },
    content=function(file){
      write.csv(employee,file)
    }
  )
  output$tableData <- renderTable(
    head(employee,300),width = "80%"
  )
}
# ------------------------------------------------------------RUNNING THE PROJECT--------------------------------------------------
shinyApp(ui = ui, server = server, options = list(height = 1000))
