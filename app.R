# Packages
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(rsconnect)
library(htmltools)
library(png)
library(DT)
library(dplyr)
library(reactable)
library(broom)
library(shinylive)
library(httpuv)

# Dataframes

corestates_census <- read.csv("corestates_census.csv")
poverty_income <- read.csv("poverty_income.csv")
statestatistics_gestation <- read.csv("statestatistics_gestation.csv")


# UI 

ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center; gap: 20px;",
    tags$img(src = "app_logo.png", height = "100px"),
    span("We, the American Women")
  ),
theme = bs_theme(
    navbar_dark="#100f0f",
    bg = "#f7f4ef", fg = "#100f0f",
    base_font = "Alice"
  ),
navbar_options = navbar_options(
    bg = "#c70000", 
    fg="#100f0f"
  ),

nav_panel("Preamble for Reproductive Justice",
          h1("November 3rd. Five State.",
             tags$em("A Generational Vote."),
             style="margin-bottom: 0px; font-weight: bold; text-align: center; color: black; font-size: 28px; font-family: 'Roboto Serif';"
          ),
          h2("Reproductive Justice is on the Electoral Ballot",
             style="margin-bottom: 0px; font-style: italic; text-align: center; color: black; font-size: 26px; font-family: 'Roboto Serif';"
          ),
          h3("Constituents deserve resources to enable them voting comprehension. Missouri and Nebraska looking to criminalize abortion, while Virginia and Nevada intend to create a constitutional abortion amendment. Idaho advocates are working to legalize reproductive freedom and 
          privacy—despite the previous state ban. Over 132,300 women are represented across these states in 
          the last census.",
          tags$b(
            tags$i("Their votes deserve to matter.")
             ),
          style="margin-bottom: 0px; text-align: center; color: black; font-size: 22px; font-family: 'Roboto Serif'; line-height: 1.3; max-width: 1100px; margin-left: auto; margin-right: auto;"
          ),
    div(style = "display: flex; justify-content: center; align-items: flex-start; gap: 50px; margin-bottom: 30px;",
          tags$img(
                src = "abortion_graphic.png",
                style = "width: 750px; height: auto;"
        )
    ),
    div(
      style = "max-width: 1300px; margin-left: auto; margin-right: auto;",
      h4("Reproductive freedom and autonomy benefits all women. Poverty, race, and key demographics all are estimated to impact women's reproductive choices.",
         style="margin-bottom: 20px; font-style: italic; text-align: center; color: black; font-size: 26px; font-family: 'Roboto Serif'; line-height: 1;"
      )
    ),
    div(style = "margin-top: 30px;",
    layout_columns(
      card(
        card_header("What's at stake, Missouri?"),
        tags$ul(
          tags$li("Black people is associated with being 2x as likely to be 200% below the poverty threshold, compared to 19.6% of White people with Census regression calculations.")
        )
      ),
      
      card(
        card_header("Who is vulnerable, Nebraska?"),
        tags$ul(
          tags$li("Being Hispanic is estimated to raise chances of being in 50% below the poverty threshold to 6.5%, compared to 3.7% for White people with statistical math.")
        )
      ),
      
      card(
        card_header("How can women be helped, Idaho?"),
        tags$ul(
          tags$li("8.30% of women are estimated to be 50% below the poverty threshold, compared to 6.85% of men with Census data.")
        )
      ),
      
      card(
        card_header("What do constituents deserve, Virginia?"),
        tags$ul(
          tags$li("Census regression estimates younger Black women are 3.9% more likely to give birth, compared to 1.3% of young White women")
        )
      ),
      
      card(
        card_header("Are all women financially helped, Nevada?"),
        tags$ul(
          tags$li("Even 200% below the poverty threshold, women are associated with experiencing poverty 20.3% more than men, according to Census statistical calculations.")
        )
      )
    )
    ),
    div(
      style = "max-width: 1300px; margin-left: auto; margin-right: auto;",
      h5("Policymakers, your constituents deserve to vote for ballots they believe in. Use this app to find how your state's population is impacted by abortion legislature and provide resources to constituents.",
         style="margin-bottom: 20px; font-style: italic; text-align: center; color: black; font-size: 28px; font-family: 'Roboto Serif'; font-weight: 500;"
      )
    )
),
          
nav_panel("Visualizing Affected Populations",
      h1("Analyzing Poverty and Pregnancy Demographics",
      style="font-weight: bold; text-align: center; font-size: 24px; font-family: 'Roboto Serif';"), 
      h2("The first dropdown menu below can be used to compared demographic variables (gender, age, race, age of children) to poverty indications. 
         A 0.5% poverty-to-income ratio represents people who make less than half of the income needed to meet the poverty
         threshold ('United States Census.') Abortion and poverty's interrelated relationship makes this chart helpful 
         for comparing vulnerable populations. Data source: American Community Surveys (ACS) (2020-2024)", 
         style="margin-bottom: 0px; font-style: italic; text-align: center; color: black; font-size: 18px; font-family: 'Roboto Serif';"),
      selectInput(
        "demographic_choice1","Demographic Variable #1",
        c(
          "Female" = "Female",
          "Male" = "Male",
          
          "Under 18 years" = "Under 18 years",
          "18 to 64 years old" = "18 to 64 years",
          "Over 65 years old" = "65 years+",
          
          "Asian" = "Asian",
          "Black" = "Black",
          "White" = "White, not Hispanic",
          "Hispanic" = "Hispanic (any race)",
          "Two or More Races" = "Two or More Races",
          
          "Related children under age 18" = "Related children under age 18",
          "Related children under age 6" = "Related children under age 6"
        )
      ),
      
      selectInput(
        "demographic_choice2","Demographic Variable #2",
        c(
          "Male" = "Male",
          "Female" = "Female",
          
          "Under 18 years" = "Under 18 years",
          "18 to 64 years old" = "18 to 64 years",
          "Over 65 years old" = "65 years+",
          
          "Asian" = "Asian",
          "Black" = "Black",
          "White" = "White, not Hispanic",
          "Hispanic" = "Hispanic (any race)",
          "Two or More Races" = "Two or More Races",
          
          "Related children under age 18" = "Related children under age 18",
          "Related children under age 6" = "Related children under age 6"
        )
      ),
      div(
        style = "margin-bottom: 20px;",
        plotOutput("economic_indicators", 
                   height = "400px", 
                   width = "100%")
      ),
    
  h3("The dropdown compares birth rates by key states with demographic age variables using Guttmacher's 
      'Pregnancies, Births and Abortions in the United States, 1973–2020: National and State Trends by Age'
      aggregated macrodata.",
         style="margin-top: 80px; font-style: italic; text-align: center; color: black; font-size: 18px; font-family: 'Roboto Serif';"),
        selectInput("population_of_interest", "Select key population:",
                    choices = c(
                      "14 and younger" = "populationlt15",
                      "15–19 years old" = "population1519",
                      "20 years oldand younger" = "populationlt20",
                      "25–29 years old" = "population2529",
                      "30–34 years old" = "population3034",
                      "35–39 years old" = "population3539",
                      "15–44 years old" = "population1544"
                    )
        ),
        
        selectInput("birth_rate_choice", "Select birth rate by age:",
                    choices = c(
                      "Birth rate (14 and younger)" = "birthratelt15",
                      "Birth rate (15–17)" = "birthrate1517",
                      "Birth rate (18–19)" = "birthrate1819",
                      "Birth rate (15–19)" = "birthrate1519",
                      "Birth rate (20 and younger)" = "birthratelt20",
                      "Birth rate  (20–24)" = "birthrate2024",
                      "Birth rate (25–29)" = "birthrate2529",
                      "Birth rate (30–34)" = "birthrate3034",
                      "Birth rate (35–39)" = "birthrate3539",
                      "Total birth rate" = "birthratetotal"
                     )
                    ),
      plotOutput("gestation_stats", 
                 height = "650px", 
                 width = "100%")
),

nav_panel("Securing Voters' Liberties",
          h1("Ensuring Voters' Know Their Ballot",
             style="margin-bottom: 1px; font-weight: bold; text-align: center; color: black; font-size: 28px; font-family: 'Roboto Serif';"
          ),
          div(
            style = "display: flex; align-items: flex-start; gap: 20px;",
            dataTableOutput("table1")
          )
  )
)

server <- function(input, output) {

  output$economic_indicators <- renderPlot({
    
    demographcharacteristics <- poverty_income %>%
      filter(Characteristic %in% c(
        input$demographic_choice1,
        input$demographic_choice2
      ))
    
    ggplot(demographcharacteristics,
           aes(x = Characteristic,
               y = Under.0.50.Percent,
               fill = Characteristic)) +
      geom_col(width = 0.4) +
      labs(
        title = "Comparing 0.5 Poverty Income Ratio by Demographics",
        x = "Demographic Characteristics",
        y = "Under 0.50 % Poverty Threshold"
      ) + 
      theme(
        plot.title = element_text(hjust = 0.5, family = "Times New Roman", size = 20),
        axis.title = element_text(family = "Times New Roman", size = 18),
        axis.title.x = element_text(margin = margin(t = 12, b = 5)),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.text = element_text(family = "Times New Roman", size = 14),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.text.y = element_text(margin = margin(r = 4)),
        panel.grid = element_line(),
        axis.line = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        legend.position = "none"
      ) +
      scale_fill_manual(values = c("#800020", "#2a52be"))
    
  }, height = 500)

  output$gestation_stats <- renderPlot({
    birth_data <- statestatistics_gestation %>%
      select(
        state,
        population = all_of(input$population_of_interest),
        birth = all_of(input$birth_rate_choice)
      )

    ggplot(birth_data, aes(x = reorder(state, -birth),y = birth,
       fill = birth)
    ) +
      
      geom_col(width = 0.4) +
      labs(
        title = "Birth Rates by State and Demographic Type",
        x = "State",
        y = "Birth Rate (%)"
      ) +
      theme(
        panel.grid = element_line(),
        axis.line = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 22, family = "Times New Roman"
        ),
        axis.title = element_text(size = 18,family = "Times New Roman"
        ),
        axis.text = element_text(size = 14)
      ) +
      scale_fill_gradient(
        low = "#e5eaef",
        high = "#003366"
      )
    
  }, height = 500)
  
  output$table1 <- renderDT({
    
    States <- c(
      '<a href="https://ballotpedia.org/Nevada_Question_6,_Right_to_Abortion_Initiative_%282026%29">Nevada Question 6: Right to Abortion Initiative</a>',
      '<a href="https://ballotpedia.org/Virginia_Right_to_Reproductive_Freedom_Amendment_%282026%29">Virginia Right to Reproductive Freedom Amendment</a>',
      '<a href="https://ballotpedia.org/Idaho_Reproductive_Freedom_and_Privacy_Act_Initiative_%282026%29">Idaho Reproductive Freedom and Privacy Initiative</a>',
      '<a href="https://ballotpedia.org/Missouri_Amendment_3,_Prohibit_Abortion_and_Gender_Transition_Procedures_for_Minors_Amendment_%282026%29">Missouri Amendment 3: Prohibit Abortion (...) Amendment</a>',
      '<a href="https://ballotpedia.org/Nebraska_Establish_Personhood_of_Preborn_Children_Amendment_%282026%29">Nebraska Establish Personhood of Preborn Child Amendment</a>'
    )
    
    df <- data.frame(States = States)
    
    datatable(
      df,
      escape = FALSE,
      colnames = "Ballot Initiatives",
      options = list(dom = "t")
    )
  })
  
}
shinyApp(ui = ui, server = server)
