# This is Shiny web application to plot variation of global, beam, and diffuse irradiance on Mars horizontal surface.
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars
#
library(here)
library(wesanderson)
library(shiny)
library(shinyWidgets)

# FIXME: Edit this to grab based on nfft.
f_all_taus = dget(here("functions", "f_all_taus.R"))

# Plot function.
diurnal_plot = dget(here("plots", "diurnal_plot.R"))

# Legend labels and colors.
G_eqs_labels = c("Global irradiance", "Beam irradiance", "Diffuse irradiance")
G_eqs_cols = wes_palette("Darjeeling1", 3)

al = 0.1    # Albedo.
nfft = 1    # Net flux function type (1 for f_89, 2 for f_90, and 3 for f).

# Areocentric Longitude values (deg).
Ls_list = list(
    "Ls = 0°   (Vernal Equinox)" = 0,
    "Ls = 71°   (Aphelion)" = 71,
    "Ls = 90°  (Summer Solstice)" = 90,
    "Ls = 180° (Autumn Equinox)" = 180,
    "Ls = 215°" = 215,
    "Ls = 248° (Periphelion)" = 248,
    "Ls = 270° (Winter Solstice)" = 270,
    "Ls = 295°" = 295)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("areocentricLongitude", label = h5("Areocentric Longitude [deg]"), 
                        choices = Ls_list, 
                        selected = as.numeric(Ls_list[6])),
            
            sliderInput("phi", label=h5("Latitude [deg]"),
                        min=-80, max=80, step=10, value=0),
            
            # sliderInput("tau", label=h5("Tau Factor (Atmospheric Opacity)"),
            #             min=1, max=6, step=1, value=1),
            
            shinyWidgets::sliderTextInput("tau", label=h5("Tau Factor (Atmospheric Opacity)"),
                                          choices=f_all_taus()),
            
            sliderInput("solarTimeRange", label=h5("Solar Time Range [h]"),
                        min=7, max=17, step=1, value=c(7, 17)),
            
            # sliderInput("irradianceRange", label = h5("Irradiance Range [W/m2]"),
            #             min=0, max=600, step=50, value=c(0, 550)),
            
            radioButtons("legendLocation", label=h5("Legend Location"), 
                          choices=list(
                              "Top Left" = "topleft",
                              "Top Right" = "topright", 
                              "Bottom Left" = "bottomleft", 
                              "Bottom Right" = "bottomright"),
                          selected="topleft")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw the plot.
server <- function(input, output) {
 
    output$distPlot <- renderPlot({

        # Areocentric longitude
        Ls = as.numeric(input$areocentricLongitude)
        
        # Latitude
        phi = as.numeric(input$phi)
        
        # Tau factor for atmospheric opacity
        tau = input$tau[1]
        
        # Sequence of omega values to calculate irradiance.
        omega_t1 = input$solarTimeRange[1]
        omega_t2 = input$solarTimeRange[2]
        omega_seq = seq(omega_t1, omega_t2, 1)
        
        # Sequence of omega values to calculate irradiance.
        # G_1 = input$irradianceRange[1]
        # G_2 = input$irradianceRange[2]
        
        legendLocation = input$legendLocation
        
        # Plot.
        diurnal_plot(nfft, Ls, phi, tau, al, omegas=omega_seq, ylim=c(0,650))
        
        # Specify selected parameters.
        mtext(paste("Ls=", Ls, "°, ", "ϕ=", phi, "°, τ=", tau, sep=""),
              cex=2, side = 3, line = -3, outer = TRUE)
        
        # Add a legend.
        legend(legendLocation,
               G_eqs_labels,
               col = G_eqs_cols,
               cex=.8, bty="n", lty=1)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
