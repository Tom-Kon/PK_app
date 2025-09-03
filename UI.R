
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
                     sliderInput("k_gi", "GI elimination rate (k_gi, 1/h)", min = 0.01, max = 5, value = 0.5, step = 0.01),
                     sliderInput("ke", "Blood elimination rate (ke, 1/h)", min = 0.01, max = 2, value = 0.2, step = 0.01),
                     sliderInput("F", "Bioavailability (F)", min = 0, max = 1, value = 0.8, step = 0.01),
                     sliderInput("Cs",  "Solubility Cs (units/V)", min = 0.01, max = 10, value = 1, step = 0.01),
                     sliderInput("D",   "Diffusion coefficient D", min = 1e-6, max = 1e-2, value = 1e-3, step = 1e-6)
            ),
            
            tabPanel("Immediate release parameters",
                     checkboxInput("simulateImmediate", "Enable Immediate Release", FALSE),
                     conditionalPanel(
                       condition = "input.simulateImmediate",
                       sliderInput("h",   "Diffusion layer thickness h",  min = 1e-3, max = 1,   value = 0.05, step = 1e-3),
                       sliderInput("rho", "Particle density rho",         min = 0.1,  max = 5,   value = 1,    step = 0.05),
                       sliderInput("r0",  "Initial particle radius r0",   min = 1e-3, max = 1,   value = 0.05, step = 1e-3),
                       sliderInput("V",   "Volume V (same units as dose)",min = 0.01, max = 10,  value = 1,    step = 0.01),
                       sliderInput("imm_delay", "Immediate: delayed first dose (h)", min = 0, max = 24, value = 0, step = 0.1),
                       sliderInput("imm_dose", "Immediate: dose amount (M0, units)", min = 0.01, max = 10, value = 1, step = 0.01),
                       sliderInput("imm_num", "Immediate: number of doses", min = 1, max = 20, value = 3, step = 1),
                       sliderInput("imm_interval", "Immediate: interval (h)", min = 0.1, max = 24, value = 4, step = 0.1)
                     )
            ),
            
            tabPanel("Sustained release parameters",
                     checkboxInput("simulateSustained", "Enable Sustained Release", FALSE),
                     conditionalPanel(
                       condition = "input.simulateSustained",
                       sliderInput("sus_delay", "Sustained: delayed first dose (h)", min = 0, max = 24, value = 0, step = 0.1),
                       sliderInput("area", "Area of the formulation", min = 0.1, max = 10, value = 2),
                       sliderInput("thickness", "Coating thickness", min = 0.1, max = 10, value = 2),
                       sliderInput("partK", "Partition coefficient = [in film]/[in medium]", min = 0.1, max = 10, value = 2),
                       sliderInput("DSustained", "Sustained release dose", min = 0.1, max = 50, value = 3),
                       sliderInput("sus_num", "Sustained: number of doses", min = 1, max = 10, value = 1, step = 1),
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
                                  selected = "combined"),
                     checkboxInput("showGI", "Show GI Concentration", TRUE),
                     checkboxInput("showBlood", "Show Blood Concentration", FALSE),
                     checkboxInput("separateColors", "Show separate colors for each dose (GI & Blood)", FALSE),
                     helpText("When enabled, each dose is plotted with its own color; consistent across GI & Blood.")
            )
          )
        ),
        mainPanel(
          width = 8,
          plotlyOutput("plotGI", height = "360px"),
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
