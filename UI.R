
UIFunc <- function(theme) {
  page_navbar(
    useShinyjs(),
    theme = theme,
    title = "PK simulation app",
    footer = tags$footer(
      style = "text-align: center; bottom: 0; width: 100%;",
      HTML("<br>", "<br>"),
      p(
        "Developed by Tom Konings and Tom Buschop. Please credit when using."
      )
    ),
    fillable = FALSE,
    
    # --- Main simulation tab ---
    nav_panel(
      "Simulation",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tabsetPanel(
            id = "tabs",
            tabPanel("General",
                     sliderInput("F", "Bioavailability (F)", min = 0, max = 1, value = 0.8, step = 0.01),
                     sliderInput("Cs",  "API solubility in GI tract (mg/mL)", min = 0.000001, max = 500, value = 300, step = 0.01),
                     sliderInput("V_GI",  "Solvent volume in the GI tract (mL)", min = 100, max = 500, value = 250),
                     sliderInput("A_GI",  "Absorption area of the GI tract (cm²)", min = 100000, max = 400000, value = 200000),
                     sliderInput("P_eff",   "Permeability coefficient through the intestinal wall (cm/s)", min = 1e-6, max = 1e-4, value = 7.5e-5, step = 1e-6),
                     sliderInput("Vd",   "Volume of distribution per kg (L/kg)", min = 0.01, max = 3, value = 1, step = 1e-2),
                     sliderInput("t0.5", "Half life (h)", min = 0.2, max = 40, value = 5, step = 0.2),
                     sliderInput("P_eff",   "Permeation coefficient through the intestinal lumen (cm/s)", min = 1e-6, max = 1e-3, value = 7.5e-4, step = 5e-6),
                     sliderInput("Vd",   "Volume of distribution per kg (L/kg)", min = 0.1, max = 2, value = 1, step = 1e-6),
                     sliderInput("weight",   "Patient weight (kg)", min = 20, max = 100, value = 70, step = 1),
                     sliderInput("Cl",   "Total plasma clearance (mL/min/kg)", min = 0.5, max = 20, value = 15, step = 0.5),
                     sliderInput("t_transit", "Transit time through the GI (h)", min = 1, max = 10, value = 8, step = 1),
                     sliderInput("therWindMin", "Therapeutic window minimum (µg/mL)", min = 0, max = 10, value = 0.2, step = 0.1),
                     sliderInput("therWindMax", "Therapeutic window maximum (µg/mL)", min = 0.1, max = 100, value = 0.8, step = 0.1)
            ),
            
            tabPanel("Immediate release parameters",
                     checkboxInput("simulateImmediate", "Enable Immediate Release", TRUE),
                     conditionalPanel(
                       condition = "input.simulateImmediate",
                       sliderInput("h",   "Diffusion layer thickness h (µm)",  min = 1, max = 1000,   value = 100, step = 10),
                       sliderInput("rho", "Particle density rho (g/mL)",         min = 0.1,  max = 5,   value = 2.65,    step = 0.05),
                       sliderInput("r0",  "Initial particle radius r0 (cm)",   min = 1e-3, max = 1,   value = 0.05, step = 1e-3),
                       sliderInput("D_Imm",   "Diffusion coefficient of the API through the diffusion layer (cm/s)", min = 0.00000001, max = 0.00001,  value = 0.0000001,    step = 0.000001),
                       sliderInput("imm_delay", "Immediate: delayed first dose (h)", min = 0, max = 24, value = 0, step = 0.1),
                       sliderInput("imm_dose", "Immediate: dose amount (mg)", min = 0.01, max = 500, value = 100),
                       sliderInput("imm_num", "Immediate: number of doses", min = 1, max = 20, value = 1, step = 1),
                       sliderInput("imm_interval", "Immediate: interval (h)", min = 0.1, max = 24, value = 2.5, step = 0.1)
                     )
            ),
            
            tabPanel("Sustained release parameters",
                     checkboxInput("simulateSustained", "Enable Sustained Release", TRUE),
                     conditionalPanel(
                       condition = "input.simulateSustained",
                       sliderInput("sus_delay", "Sustained: delayed first dose (h)", min = 0, max = 24, value = 0, step = 0.1),
                       sliderInput("area", "Area of the formulation (cm²)", min = 2, max = 7, value = 3),
                       sliderInput("thickness", "Coating thickness (µm)", min = 20, max = 60, value = 40),
                       sliderInput("partK", "Partition coefficient = [in film]/[in medium]", min = 0.1, max = 10, value = 1.5),
                       sliderInput("D_Sus",   "Diffusion coefficient of the API through the coating layer (cm²/s)", min = 1e-6, max = 1e-4, value = 3.5e-5),
                       sliderInput("sust_dose", "Sustained release dose (mg)", min = 0.1, max = 1000, value = 760),
                       # sliderInput("sus_num", "Sustained: number of doses", min = 1, max = 10, value = 1, step = 1),
                       # sliderInput("sus_interval", "Sustained: interval (h)", min = 0.1, max = 24, value = 8, step = 0.1)
                     )
            ),
            
            tabPanel("Other switches",
                     radioButtons("ClVdt0.5Fix", "Check which parameter should be fixed", 
                                  choices = c("Half-life" = "t0.5Fix", 
                                              "Volume of distribution" = "VdFix",
                                              "Clearance" = "ClFix")),
                     radioButtons("GIview", "GI View",
                                  choices = c("Immediate only" = "immediate",
                                              "Sustained only" = "sustained",
                                              "Immediate + Sustained" = "both"),
                                  selected = "both"),
                     checkboxInput("showGI", "Show GI Concentration", TRUE),
                     checkboxInput("showBlood", "Show Blood Concentration", TRUE),
                     checkboxInput("showTherWind", "Show therapeutic window", FALSE),
                     # checkboxInput("separateColors", "Show separate curves for each immediate release dose instead of sum", FALSE),
                     # helpText("When enabled, each dose is plotted with its own color; consistent across GI & Blood.")
            ), 
            tabPanel("Downloads",
                     downloadButton("downloadGI", "Download the GI tract concentration plot"),
                     downloadButton("downloadBlood", "Download the plasma concentration plot"),
                     downloadButton("downloadExcel", "Download the Excel with the full analysis")

            )
          )
        ),
        mainPanel(
          width = 9,
          plotlyOutput("plotGI", height = "360px"),
          HTML("<br>", "<br>", "<br>"),
          plotlyOutput("plotBlood", height = "360px"),
          div(
            style = "margin-top: 25px;",
            
            h3(
              "Pharmacokinetic Parameters",
              style = "font-weight: 600; margin-bottom: 20px;"
            ),
            
            div(
              style = paste(
                "background-color: #f8f9fa;",
                "border-left: 5px solid #198754;",
                "border-radius: 6px;",
                "padding: 15px 20px;",
                "margin-bottom: 20px;",
                "box-shadow: 0 1px 3px rgba(0,0,0,0.08);"
              ),
              
              h4(
                "Immediate Release",
                style = "margin-top: 0; font-weight: 600; font-size: 20px;"
              ),
              
              div(
                style = "font-size: 17px; line-height: 1.7;",
                textOutput("ImmParameters")
              )
            ),
            
            div(
              style = paste(
                "background-color: #f8f9fa;",
                "border-left: 5px solid #0d6efd;",
                "border-radius: 6px;",
                "padding: 15px 20px;",
                "margin-bottom: 15px;",
                "box-shadow: 0 1px 3px rgba(0,0,0,0.08);"
              ),
              
              h4(
                "Sustained Release",
                style = "margin-top: 0; font-weight: 600; font-size: 20px;"
              ),
              
              div(
                style = "font-size: 17px; line-height: 1.7;",
                textOutput("SusParameters")
              )
            )
          )
        )
      )
    ),
    
    # --- Tutorial tab ---
    nav_panel(
      "Mathematical background",
      fluidPage(
        tags$head(
            tags$style(HTML("
            .tutorial-svg {
              width: 50% !important;
              height: auto !important;
              display: block;
              margin: auto;
            }
          "))
        ),
        withMathJax(),
        
        includeMarkdown("tutorial.md")
      )
    ),
  )
}
