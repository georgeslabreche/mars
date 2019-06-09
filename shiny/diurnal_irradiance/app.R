#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(here)
library(wesanderson)
library(shiny)
library(shinyWidgets)


# FIXME: Edit this to grab based on nfft.
f_all_taus = dget(here("functions", "f_all_taus.R"))

# Equation 6: Zenith angle of the incident solar radiation (deg).
Z_eq = dget(here("functions", "Z.R"))

# Equation 17: Global irradiance on Mars horizontal surface (W/m2).
Gh_eq = dget(here("functions", "G_h.R"))

# Equation 18: Beam irradiance on Mars horizontal surface (W/m2).
Gbh_eq = dget(here("functions", "G_bh.R"))

# Determine an expression for the diffuse irradiance based on Eq. 17 and Eq. 18.
# Equation 16: The solar irradiance components on a horizontal Martian surface.
# Gh = Gbh + Gdh
Gdh_eq = dget(here("functions", "G_dh.R"))

# Store all irradiance equations and their labels
G_eqs = c(Gh_eq, Gbh_eq, Gdh_eq)
G_eqs_labels = c("Global irradiance", "Beam irradiance", "Diffuse irradiance")
G_eqs_cols = wes_palette("Darjeeling1", 3)

al = 0.1    # Albedo.
nfft = 1    # Net flux function type (1 for f_89, 2 for f_90, and 3 for f).

# Areocentric Longitude values (deg).
Ls_list = list(
    "Vernal Equinox (Ls=0°)" = 0,
    "Aphelion (Ls=71°)" = 71,
    "Summer Solstice (Ls=90°)" = 90,
    "Autumn Equinox (Ls=180°)" = 180,
    "Periphelion (Ls=248°)" = 248,
    "Winter Solstice (Ls=270°)" = 270)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("areocentricLongitude", label = h5("Areocentric Longitude [deg]"), 
                        choices = Ls_list, 
                        selected = as.numeric(Ls_list[5])),
            
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
        
        G_index = 1
        for(G_eq in G_eqs){
            G_seq = c()
            
            # Calculate the irradiance for each given parameters.
            # Store the results in a sequence that will be used in the plot() function.
            for(omega in omega_seq){
                Z = Z_eq(Ls, omega, phi, nfft)
                G = G_eq(Ls, Z, tau, al, nfft)
                G_seq = c(G_seq, G)
            }
            
            # Plot
            if(length(omega_seq) == length(G_seq)){
                if(G_index == 1){
                    plot(omega_seq, G_seq,
                         xlab="Solar Time [h]", ylab="Irradiance [W/m2]",
                         ylim=c(0, 650),
                         pch=3,
                         col=G_eqs_cols[G_index],
                         font.sub=2,
                         cex.sub=1.2)
                    
                    smooth_line = smooth.spline(omega_seq, G_seq, spar=0.35)
                    lines(smooth_line, col=G_eqs_cols[G_index])
                    
                    new_plot_initialized = TRUE
                }else{
                    # Make sure we are plotting on a new plot when needed.
                    tryCatch({
                        if(new_plot_initialized == TRUE){
                            smooth_line = smooth.spline(omega_seq, G_seq, spar=0.35)
                            lines(smooth_line, col=G_eqs_cols[G_index])
                            
                            points(omega_seq, G_seq,
                                   pch=3,
                                   col=G_eqs_cols[G_index])
                        }else{
                            stop("New plot has not been initialized.")
                        }
                    },
                    warning = function(w) {
                        # Do nothing
                    },
                    error = function(e) {
                        # Enter here when following error occurs: plot.new has not been called yet
                        plot(omega_seq, G_seq,
                             xlab="Solar Time [h]", ylab="Irradiance [W/m2]",
                             ylim=c(0,550),
                             pch=3,
                             col=G_eqs_cols[G_index],
                             font.sub=2,
                             cex.sub=1.2)
                        
                        smooth_line = smooth.spline(omega_seq, G_seq, spar=0.35)
                        lines(smooth_line, col=G_eqs_cols[G_index])
                        
                        new_plot_initialized = TRUE
                    },
                    finally = {
                        # Do nothing
                    })
                    
                }
            }else{
                # Using the f_89 or f_90 lookup table based implementations of the net flux function may
                # result feeding that function a rounded Z parameter that does not exist in the lookup tables.
                # This will result in an error which we are handling here by not plotting the affected irradiance type.
                message(paste("Could not calculate ", G_eqs_labels[G_index] , " for latitude ϕ=", phi, "°.", " Try using the analytical expression of the net flux function instead of the lookup table.", sep=""))
            }
            
            if(G_index == 3){
                G_index = 1
            }else{
                G_index = G_index + 1
            }
        }
        mtext(paste("Ls=", Ls, "°, ", "ϕ=", phi, "°, ", "τ=", tau, sep=""),
              cex=2, side = 3, line = -3, outer = TRUE)
        
        # Add a legend
        legend(legendLocation,
               G_eqs_labels,
               col = G_eqs_cols,
               cex=.8, bty="n", lty=1)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
