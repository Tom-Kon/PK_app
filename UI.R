
library(shiny)
library(markdown)

UIFunc <- function() {
  navbarPage(
    title = "Immediate vs Sustained release comparison with additional parameters",
    
    # --- Main simulation tab ---
    tabPanel(
      "Simulation",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          tabsetPanel(
            id = "tabs",
            tabPanel("General",
                     sliderInput("F", "Bioavailability (F)", min = 0, max = 1, value = 0.8, step = 0.01),
                     sliderInput("Cs",  "Solubility Cs (g/mL)", min = 0.000001, max = 500, value = 1, step = 0.01),
                     sliderInput("V_GI",  "Solvent volume in the GI tract (mL)", min = 100, max = 500, value = 250),
                     sliderInput("A_GI",  "Absorption area of the GI tract (cm²)", min = 100000, max = 2000000, value = 1000000),
                     sliderInput("P_eff",   "Permeation coefficient through the intestinal lumen (cm/s)", min = 1e-6, max = 1e-4, value = 1e-5, step = 1e-6),
                     sliderInput("Vd",   "Volume of distribution per kg (L/kg)", min = 0.1, max = 2, value = 1, step = 1e-6),
                     sliderInput("weight",   "Patient weight (kg)", min = 20, max = 100, value = 70, step = 1),
                     sliderInput("Cl",   "Total plasma clearance (L/s)", min = 1, max = 60, value = 20, step = 1),
                     sliderInput("t_transit", "Transit time through the GI (h)", min = 1, max = 6, value = 3, step = 1)
            ),
            
            tabPanel("Immediate release parameters",
                     checkboxInput("simulateImmediate", "Enable Immediate Release", TRUE),
                     conditionalPanel(
                       condition = "input.simulateImmediate",
                       sliderInput("h",   "Diffusion layer thickness h",  min = 1e-3, max = 1,   value = 0.05, step = 1e-3),
                       sliderInput("rho", "Particle density rho",         min = 0.1,  max = 5,   value = 1,    step = 0.05),
                       sliderInput("r0",  "Initial particle radius r0",   min = 1e-3, max = 1,   value = 0.05, step = 1e-3),
                       sliderInput("D_Imm",   "Diffusion coefficient of the API through the diffusion layer (cm/s)", min = 0.01, max = 10,  value = 1,    step = 0.01),
                       sliderInput("imm_delay", "Immediate: delayed first dose (h)", min = 0, max = 24, value = 0, step = 0.1),
                       sliderInput("imm_dose", "Immediate: dose amount (M0, units)", min = 0.01, max = 10, value = 1, step = 0.01),
                       sliderInput("imm_num", "Immediate: number of doses", min = 1, max = 20, value = 3, step = 1),
                       sliderInput("imm_interval", "Immediate: interval (h)", min = 0.1, max = 24, value = 4, step = 0.1)
                     )
            ),
            
            tabPanel("Sustained release parameters",
                     checkboxInput("simulateSustained", "Enable Sustained Release", TRUE),
                     checkboxInput("simulateSustainedODE", "Enable Advanced Sustained Release", TRUE),
                     conditionalPanel(
                       condition = "input.simulateSustained == true || input.simulateSustainedODE == true",
                       sliderInput("sus_delay", "Sustained: delayed first dose (h)", min = 0, max = 24, value = 0, step = 0.1),
                       sliderInput("area", "Area of the formulation (cm²)", min = 2, max = 7, value = 3),
                       sliderInput("thickness", "Coating thickness (cm)", min = 2e-3, max = 4e-3, value = 3e-3),
                       sliderInput("partK", "Partition coefficient = [in film]/[in medium]", min = 0.1, max = 10, value = 2),
                       sliderInput("D_Sus",   "Diffusion coefficient of the API through the coating layer (cm²/s)", min = 1e-8, max = 1e-6,  value = 1e-7,    step = 1e-8),
                       sliderInput("sust_dose", "Sustained release dose (mg)", min = 0.1, max = 50, value = 3),
                       # sliderInput("sus_num", "Sustained: number of doses", min = 1, max = 10, value = 1, step = 1),
                       sliderInput("sus_interval", "Sustained: interval (h)", min = 0.1, max = 24, value = 8, step = 0.1)
                     )
            ),
            
            tabPanel("Views",
                     radioButtons("GIview", "GI View",
                                  choices = c("Immediate only" = "immediate",
                                              "Sustained only" = "sustained",
                                              "Immediate + Sustained" = "both"),
                                  selected = "both"),
                     radioButtons("bloodMode", "Blood View",
                                  choices = c("Combined" = "combined", "Separate" = "separate"),
                                  selected = "separate"),
                     checkboxInput("showGI", "Show GI Concentration", TRUE),
                     checkboxInput("showBlood", "Show Blood Concentration", TRUE),
                     checkboxInput("separateColors", "Show separate colors for each dose (GI & Blood)", FALSE),
                     helpText("When enabled, each dose is plotted with its own color; consistent across GI & Blood.")
            )
          )
        ),
        mainPanel(
          width = 8,
          plotlyOutput("plotGI", height = "360px"),
          HTML("<br>", "<br>", "<br>"),
          plotlyOutput("plotBlood", height = "360px")
        )
      )
    ),
    
    # --- Tutorial tab ---
    tabPanel(
      "Tutorial",
      fluidPage(
        includeMarkdown("tutorial.md")
      )
    )
  )
}
