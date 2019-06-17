# Solar Radiation on Mars
Plotting solar radiation on Mars as a function of the following parameters:
- Areocentric Longitude (Ls)
- Planetary Latitude (phi)
- Solar Time (omega)
- Atmospheric Opacity (tau)
- Albedo (al)
- Slope Angle (beta)
- Slope Orientation (gamma)

## Functions
The following functions are implemented:

| Filename | Description                                                  | Reference                                |
|----------|--------------------------------------------------------------|------------------------------------------|
| [G_ob.R](https://github.com/georgeslabreche/mars-solar-radiation/blob/master/functions/G_ob.R)   | Beam irradiance at the top of the Martian atmosphere (W/m2). | Eq. 4 (Appelbaum et al. 1990)            |
| [Z.R](https://github.com/georgeslabreche/mars-solar-radiation/blob/master/functions/Z.R)      | Zenith angle of the incident solar radiation (deg).          | Eq. 6 (Appelbaum et al. 1990)            |
| [T_d.R](https://github.com/georgeslabreche/mars-solar-radiation/blob/master/functions/T_d.R)    | Number of Mars daylight hours (h).                           | Eq. 10 (Appelbaum et al. 1990)            |
| [G_b.R](https://github.com/georgeslabreche/mars-solar-radiation/blob/master/functions/G_b.R)    | Beam irradiance on Mars surface (W/m2).                      | Eq. 13 (Appelbaum et al. 1990)           |
| [G_dh.R](https://github.com/georgeslabreche/mars-solar-radiation/blob/master/functions/G_dh.R)   | Diffuse irradiance on Mars horizontal surface (W/m2).        | Eq. 16 (Appelbaum et al. 1990)           |
| [G_h.R](https://github.com/georgeslabreche/mars-solar-radiation/blob/master/functions/G_h.R)    | Global irradiance on Mars horizontal surface (W/m2).         | Eq. 17 (Appelbaum et al. 1990)           |
| [G_bh.R](https://github.com/georgeslabreche/mars-solar-radiation/blob/master/functions/G_bh.R)   | Beam irradiance on Mars horizontal surface (W/m2).           | Eq. 18 (Appelbaum et al. 1990)           |
| [f.R](https://github.com/georgeslabreche/mars-solar-radiation/blob/master/functions/f.R)      | The normalized net flux function.                            | Table III (Appelbaum et al. 1990)<br>Table I & II (Appelbaum et al. Update 1990)<br>Eq. 20 (Appelbaum et al. Update 1990)   |
| [tau.R](https://github.com/georgeslabreche/mars-solar-radiation/blob/master/functions/tau.R)    | The optical depth tau factor function.                       | Eq. 1 & 2 (Appelbaum et al. Update 1991) |
| [al.R](https://github.com/georgeslabreche/mars-solar-radiation/blob/master/functions/al.R)    | The albedo function.                       | Table I (Appelbaum et al. Update 1991) |
| [G_beta.R](https://github.com/georgeslabreche/mars-solar-radiation/blob/master/functions/G_beta.R) | Global irradiance on an inclined surface (W/m2).             | Eq. 1 & 2 (Appelbaum et al. 1994)        |

From Appelbaum, Joseph & Flood, Dennis (1990):

## References
Based on the following excellent work:
- [Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7](https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars): Detailed information on solar radiation characteristics on Mars are necessary for effective design of future planned solar energy systems operating on the surface of Mars. In this paper we present a procedure and solar radiation related data from which the diurnally, hourly and daily variation of the global, direct beam and diffuse insolation on Mars are calculated. The radiation data are based on measured optical depth of the Martian atmosphere derived from images taken of the sun with a special diode on the Viking cameras; and computation based on multiple wavelength and multiple scattering of the solar radiation.

- [Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars: Update 1990. NASA STI/Recon Technical Report N. 91. 15117-](https://www.researchgate.net/publication/259222079_Solar_radiation_on_Mars_Update_1990): Detailed information on solar radiation characteristics on Mars are necessary for effective design of future planned solar energy systems operating on the surface of Mars. The authors present a procedure and solar radiation related data from which the diurnally and daily variation of the global, direct beam and diffuse insolation on Mars are calculated. The radiation data are based on measured optical depth of the Martian atmosphere derived from images taken of the Sun with a special diode on the Viking Lander cameras and computation based on multiple wavelength and multiple scattering of the solar radiation. This work is an update to NASA-TM-102299 and includes a refinement of the solar radiation model.

- [Appelbaum, Joseph & Landis, Geoffrey & Sherman, I. (1991). Solar radiation on Mars—Update 1991. Solar Energy. 50. 35-51. 10.1016/0038-092X(93)90006-A](https://www.researchgate.net/publication/223850868_Solar_radiation_on_Mars-Update_1991): Detailed information on solar radiation characteristics on Mars are necessary for effective design of future planned solar energy systems operating on the surface of Mars. In this paper we present a procedure and solar radiation related data from which the daily variation of the global, direct beam, and diffuse insolation on Mars are calculated. Given the optical depth of the Mars atmosphere, the global radiation is calculated from the normalized net flux function based on multiple wavelength and multiple scattering of the solar radiation. The direct beam was derived from the optical depth using Beer's law, and the diffuse component was obtained from the difference of the global and the direct beam radiation. The optical depths of the Mars atmosphere were derived from images taken of the Sun with a special diode on the cameras used on the two Viking Landers.

- [Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994). Solar radiation on Mars: Tracking photovoltaic array. Journal of Propulsion and Power. 12. 10.2514/3.24044](https://www.researchgate.net/publication/24286713_Solar_radiation_on_Mars_Tracking_photovoltaic_array): A photovoltaic power source for surface-based operation on Mars can offer many advantages. Detailed information on solar radiation characteristics on Mars and the insolation on various types of collector surfaces are necessary for effective design of future planned photovoltaic systems. In this article we have presented analytical expressions for solar radiation calculation and solar radiation data for single axis (of various types) and two axis tracking surfaces and compared the insulation to horizontal and inclined surfaces. For clear skies (low atmospheric dust load) tracking surfaces resulted in higher insolation than stationary surfaces, whereas for highly dusty atmospheres, the difference is small. The insolation on the different types of stationary and tracking surfaces depend on latitude, season and optical depth of the atmosphere, and the duration of system operation. These insolations have to be compared for each mission.

## Future Development
#### [Appelbaum, Joseph & Flood, Dennis (1990) — Update 1990](https://www.researchgate.net/publication/259222079_Solar_radiation_on_Mars_Update_1990)
- **[DONE]** Include more granular optical depth and zenith angle parameter value options for the normalized net flux function.
- **[DONE]** Implement the analytical expression of the normalized net flux function rather than just relying on a lookup table.
- **[DONE]** Implement function for number of Mars daylight hours.
- Implement functions to calculate Whr/m2-days.

#### [Appelbaum, Joseph & Landis, Geoffrey & Sherman, I (1991) — Update 1991](https://www.researchgate.net/publication/223850868_Solar_radiation_on_Mars-Update_1991)
- **[DONE]** Implement albedo function that takes longitude and latitude as parameters.
- **[DONE]** Implement the analytical expression of the optical depth, as a function of latitude and areocentric longitude, rather than just relying on inputing its value as a parameter.
- **[DONE]** Plot optical depth tau factor as a function of latitude and areocentric longitude.

#### [Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos (1994)](https://www.researchgate.net/publication/24286713_Solar_radiation_on_Mars_Tracking_photovoltaic_array)
- **[DONE]** Add surface inclination angle as a parameter.

#### Other
- **[DONE]** Plot optical depth ground measurements for MER an Viking Lander and compare with Appelbaum analytical models.
- Generate plots of interest for the thesis and README documentation purposes.
- Experiment with 3D plots.
- Experiment with model described in [Vicente-Retortillo, Álvaro & Valero, Francisco & Vázquez, Luis & Martinez, German M.. (2015). A model to calculate solar radiation fluxes on the Martian surface. Journal of Space Weather and Space Climate. 5. A33. 10.1051/swsc/2015035](https://www.researchgate.net/publication/283452176_A_model_to_calculate_solar_radiation_fluxes_on_the_Martian_surface).
