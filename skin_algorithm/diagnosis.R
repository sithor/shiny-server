colorize <- function(x, color) {
  sprintf("<h3><span style='color: %s;'>%s</span></h3>", color,
          x)
}

diagnose_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      
      sidebarPanel(
        h4("Age of child?"),
        numericInput(ns("child_age"), "", 10, min = 0, max = 15, step = 1, "100px"),
        strong("years"),
        h4("Symptoms (from parent)?"),
        checkboxInput(ns("itchy"), "🥴 Is the child itchy?", FALSE),
        "⏳ How long ago did the symptoms start?",
        numericInput(ns("symptom_start"), "", 0, min = 0, max = 3*365, step = 1, "100px"),
        selectInput("time_scale", "⏰ scale?", c("hours", "days", "weeks", "months"), width = "100px"),
        checkboxInput(ns("family_itchy"), "🏡 Is anyone else in the family itchy?", FALSE),
        checkboxInput(ns("family_skin_issue"), "🏡 Does anyone else in the family have a skin problem?", FALSE),
        checkboxInput(ns("pain"), "😖 Does the child have painful skin?", FALSE),
        h4("💊🧴 Drug history?"),
        "Is the child taking any of the following?",
        checkboxInput(ns("topical_strong_steroid"), "🧴Topical strong steroid?", FALSE),
        checkboxInput(ns("topical_weak_steroid"), "🧴Topical weak steroid?", FALSE),
        checkboxInput(ns("topical_antibiotics"), "🧴 Topical antibiotics?", FALSE),
        checkboxInput(ns("permethrin lotion"), "🧴 Topical permethrin?", FALSE),
        checkboxInput("other_med", "🧴 Other skin lotion?", FALSE),
        checkboxInput(ns("systemic_steroids"), "💊 oral steroid?", FALSE),
        checkboxInput(ns("anti-histamine"),"🧴Topical or 💊 oral anti-histamines", FALSE),
        checkboxInput(ns("oral_antibiotics"), "💊 Oral antibiotics?", FALSE),
        checkboxInput(ns("ivermectin"), "💊 Oral ivermectin?", FALSE),
        checkboxInput("other_med", "💊 Other oral medication?", FALSE),
        h5("🤧 Allergies?"),
        textInput(ns("allergy"), "Please enter all separated by commas.", 
                  value = "", width = '400px', placeholder = "Amoxycillin"),
        # c("Is the child itchy?",
        # "Is anyone else in the family itchy?",
        # "Does anyone else in the family have a skin problem?",
        # "Does the child have painful skin?")),
        h4("🧐 Signs?"),
        "🤒️ Please take the child's temperature, 
        particularly if severe infection is suspected.",
        checkboxInput(ns("febrile"), "🥵 Temperature greater 
                      than 38 degrees celsius?", FALSE),
        "Please examine the symptomatic area, and expose the arms and hands, 
          legs and feet and torso under good lighting 💡.",
        br(),br(),
        strong("Skin appearance?"),
        checkboxInput(ns("cellulitis"), "Extensive warmth, redness, or swelling?", FALSE),
        tags$img(src ='Cellulitis.png', height = "70%", width = "70%",  align = "center"),
        checkboxInput(ns("abscess"), "Localised warmth, redness, pus, ulceration or swelling?", FALSE),
        imageOutput(ns("infected_scabies_image_age"), width = "40%", height = "40%"),
        checkboxInput(ns("impetigo"), "Crusts?", FALSE),
        tags$img(src ='impetigo.png', height = "70%", width = "70%",  align = "center"),
        checkboxInput(ns("scabies"), "Excoriation and crops of papules on limbs and trunk?", FALSE),
        imageOutput(ns("scabies_image_age"), width = "40%", height = "40%"),
        checkboxInput(ns("atypical_scabies"), "Single papules on limbs and trunk +/- excoriation?", FALSE),
        tags$img(src ='atypical_scabies.png', height = "40%", width = "40%",  align = "center"),
        checkboxInput(ns("fungal"), "Round to oval flat scaly patches with excoriation?", FALSE),
        tags$img(src ='fungal.png', height = "70%", width = "70%",  align = "center"),
        checkboxInput(ns("eczema"), "Confluent patches of red skin on flexures?", FALSE),
        tags$img(src ='eczema.png', height = "30%", width = "30%",  align = "center"),
        checkboxInput(ns("other"), "Other?", FALSE)
      ),
      mainPanel(
        "App by Simon Thornley.",
        br(),
        
        "Not currently for clinical use. This is a mock-up of an app for 
          diagnostic use in children aged less than 15 years with itch or rash.",
        h3("Diagnosis is:"),
        htmlOutput(ns("diagnosis")),
        tags$br(),
        "Allergies are to the following drugs:",
        htmlOutput(ns("allergy"))
        # plotOutput("distPlot")
      )
  
  ))
}

diagnose_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$diagnosis <- renderText({
        if (input$cellulitis == TRUE) {
          paste0(colorize("Extensive cellulitis and possible systemic sepsis",
                          "red"),
                 tags$br(),
                 "Suggest ", tags$b("urgent"), " hospital review re 
                 intravenous antibiotics 
     and possible surgical review.", tags$br(),
                 "Consider review for possible scabies treatment
                 after recovery from acute illness.")
        } else if ((input$scabies == TRUE | ((input$itchy == TRUE|
                                             input$family_itchy == TRUE ) & 
                                            input$atypical_scabies == TRUE)) & 
                  (input$impetigo == TRUE| input$abscess == TRUE)){
          paste0(colorize("Infected scabies", "orange"),
                 "Suggest oral antibiotics and arrange synchronised 
       treatment of ", tags$b("all of household")," whether symptomatic or not with two doses of ",
                 tags$b("oral ivermectin"), " (0.2 mg/kg) stat and repeat after two weeks.
       Symptoms should resolve after one week.",
                 tags$br(),
                 "Topical permethrin should be avoided since it may flare bacterial infection.",
                 tags$br(),
                 "Children less than 15 kg and pregnant women should be treated with permethrin.",
                 tags$br(),
                 "Advise to come back if no 
       symptomatic improvement or rash does not resolve.
       Consider reviewing the skin of other household members for a similar diagnosis.") 
        } else if (input$scabies | ((input$itchy |
                                            input$family_itchy ) & 
                                           input$atypical_scabies )) {
          paste0(colorize("Suspected or clinical scabies", "orange"),
                 "Suggest arrange synchronised 
       treatment of ", tags$b("all of household")," whether symptomatic or not with two doses of ",
                 tags$b("topical permethrin")," or ", tags$b("oral ivermectin"), " (0.2 mg/kg) stat and repeat after two weeks.
       Symptoms should resolve after one week.",
                 tags$br(),
                 "Children less than 15 kg and pregnant women should be treated with permethrin.",
                 tags$br(),
                 "Advise to come back if no 
       symptomatic improvement or rash does not resolve.
       Consider reviewing the skin of other household members for a similar diagnosis.")
        } else if (input$fungal){
          paste0(colorize("Likely fungal infection", "orange"),
                 "Suggest take skin swab to confirm diagnosis.", tags$br(),
                 "Treat with topical antifungal.", tags$br(),
                 "For example, 5% miconazole lotion for 2 weeks.", tags$br(),
                 "Suggest avoid water on affected area and dry lesion.",
                 "Advise to return if not improving.")
        }  else if ((input$abscess|input$impetigo) & !input$febrile){
          paste0(colorize("Likely localised cellulitis or abscess", "orange"),
                 "🧪 Suggest take skin swab.", tags$br(),
                 "💊 Treat with systemic antibiotic.", tags$br(),
                 "💊 Consider co-treatment for scabies with oral ivermectin ",
                 tags$br("(0.2 mg/kg)"), " stat and repeat after two weeks,
                 since scabies may be contributing.", tags$br(),
                 "Symptoms should resolve after one week.",
                 tags$br(),
                 "Topical permethrin should be avoided since it may flare bacterial infection.",
                 tags$br(),
                 "Children less than 15 kg and pregnant women should be treated with permethrin.",
                 tags$br(),
                 "Advise to come back if no 
                 symptomatic improvement or rash does not resolve.
                 Consider reviewing the skin of other household members for a similar diagnosis.")
        } else if (input$eczema){
          paste0(colorize("Likely allergic eczema", "orange"),
                 "Suggest take history for likely allergic exposures.", tags$br(),
                 "Suggest treatment with topical steroid and emollient.", tags$br(),
                ifelse(input$itchy|input$family_itchy, 
                       "Consider treatment for scabies if 
                       other household members itchy. ", " "),
              ifelse(input$systemic_steroids| input$topical_weak_steroids|
                       input$topical_strong_steroids, "In the presence of 
              steroid treatment, usual signs of scabies may be masked, and 
              may such patients may present with generalised eczema.
              Please consider a diagnostic challenge of ivermectin 
               if no recent scabies treatment. ", " "))
        } else {
          "Insufficient information to make diagnosis."
        }
      })
      output$allergy <- renderText({
       colorize(input$allergy, "red")
      })
      
      output$scabies_image_age <- renderImage({
        if(input$child_age <= 2){
          scab <- "./www/baby_scabies.png"
        } else {
          scab <- "./www/scabies.jpg"
        }
        list(src=scab)
      }, deleteFile = FALSE)
      
      output$infected_scabies_image_age <- renderImage({
        if(input$child_age <= 2){
          inf <- "./www/Infant_infected_scabies.png"
        } else {
          inf <- "./www/abscess.jpg"
        }
        list(src = inf)
      }, deleteFile = FALSE)
      
    }
  )
}