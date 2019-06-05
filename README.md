# Solar Radiation on Mars
Plotting solar radiation on Mars as a function of many parameters.

## Functions
The following functions are taken from Appelbaum, Joseph & Flood, Dennis (1990):

| Filename | Description                                                  | Reference |
|----------|--------------------------------------------------------------|-----------|
| G_ob.R   | Beam irradiance at the top of the Martian atmosphere (W/m2). | Eq. 4     |
| Z.R      | Zenith angle of the incident solar radiation (deg).          | Eq. 6     |
| G_dh.R   | Diffuse irradiance on Mars horizontal surface (W/m2).        | Eq. 16    |
| G_h.R    | Global irradiance on Mars horizontal surface (W/m2).         | Eq. 17    |
| G_bh.R   | Beam irradiance on Mars horizontal surface (W/m2).           | Eq. 18    |
| f.R      | The normalized net flux function.                            | Table III |

## References
Based on the following excellent work:
- [Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7](https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars):  Detailed information on solar radiation characteristics on Mars are necessary for effective design of future planned solar energy systems operating on the surface of Mars. In this paper we present a procedure and solar radiation related data from which the diurnally, hourly and daily variation of the global, direct beam and diffuse insolation on Mars are calculated. The radiation data are based on measured optical depth of the Martian atmosphere derived from images taken of the sun with a special diode on the Viking cameras; and computation based on multiple wavelength and multiple scattering of the solar radiation.

- [Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994). Solar radiation on Mars: Tracking photovoltaic array. Journal of Propulsion and Power. 12. 10.2514/3.24044](https://www.researchgate.net/publication/24286713_Solar_radiation_on_Mars_Tracking_photovoltaic_array): A photovoltaic power source for surface-based operation on Mars can offer many advantages. Detailed information on solar radiation characteristics on Mars and the insolation on various types of collector surfaces are necessary for effective design of future planned photovoltaic systems. In this article we have presented analytical expressions for solar radiation calculation and solar radiation data for single axis (of various types) and two axis tracking surfaces and compared the insulation to horizontal and inclined surfaces. For clear skies (low atmospheric dust load) tracking surfaces resulted in higher insolation than stationary surfaces, whereas for highly dusty atmospheres, the difference is small. The insolation on the different types of stationary and tracking surfaces depend on latitude, season and optical depth of the atmosphere, and the duration of system operation. These insolations have to be compared for each mission.

## Future Development
 - Add surface inclination angle as a parameter based on work by Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos (1994).
 - Generate plots of interest for the thesis.
 - Experiment more with 3D plots.
 - Experiment with model described in [Vicente-Retortillo, Álvaro & Valero, Francisco & Vázquez, Luis & Martinez, German M.. (2015). A model to calculate solar radiation fluxes on the Martian surface. Journal of Space Weather and Space Climate. 5. A33. 10.1051/swsc/2015035](https://www.researchgate.net/publication/283452176_A_model_to_calculate_solar_radiation_fluxes_on_the_Martian_surface).
