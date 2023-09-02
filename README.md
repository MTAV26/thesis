# <p align="center"> Results of the Doctoral Thesis</p>

#  <p align="center"> **Predicción estacional de sequía meteorológica y su aplicación en incendios forestales.**</p>
### <p align="center"> Seasonal Prediction of Meteorological Drought and its Application in Forest Fires.</p>

<p align="center"> Miguel Ángel Torres-Vázquez; University of Murcia (miguelangel.torres@um.es).</p>
<p align="center">Director: Juan Pedro Montávez Gómez; University of Murcia (montavez@um.es).</p>
<p align="center">Director: Marco Turco; University of Murcia  (mturco@um.es).</p>

In the next three chapters, which constitute the results of the doctoral thesis titled "Seasonal Prediction of Meteorological Drought and its Application in Forest Fires," various resources are provided to enhance the understanding and analysis of the research. In addition to the information contained in the thesis book, we have made available to the reader a wide range of additional resources. These resources include data, codes, and supplementary images that, due to their extent, could not be included in the thesis book. The three web applications corresponding to 4SPAIN (chapter 3), 4DROP (chapter 4), and 4FIRE (chapter 5) are grouped together at the following web link: https://matv.shinyapps.io/APPS/.

### Chapter 3: Seguimiento y predicción probabilística de la sequía meteorológica de alta resolución en un entorno mediterráneo: 4SPAIN
### *High-Resolution Monitoring and Probabilistic Prediction of Meteorological Drought in a Mediterranean Environment: 4SPAIN*
In this chapter, we assess the quality of a prototype seasonal forecasting system for a Mediterranean region (peninsular Spain + Balearic Islands) to predict meteorological drought as measured by the standardised precipitation index (SPI). We first show that there is a high agreement between the official data provided by the Spanish Meteorological Agency and the state-of-the art ERA5 reanalysis, building confidence in using these datasets. Thus, since the ERA5 data are provided in near-real time as it is updated on a monthly basis, it can be used to monitor drought evolution. Then, we demonstrate that it is possible to obtain skilful and reliable seasonal drought predictions several months in advance by applying an ensemble-based streamflow prediction system (ESP; Twedt, 1977, Day, 1985) using ERA5 data as initial conditions. The results indicate that a statistical persistence-based model could lead to an actionable seasonal drought forecasting skill thus providing the basis for a cheap and fast prototype for drought early warning.  Refer to 
<a href="[https://www.sciencedirect.com/science/article/pii/S2212094723000117](https://www.sciencedirect.com/science/article/pii/S2212094723000117)" target="_blank_">Torres-Vázquez et al., (2023)</a>

### Chapter 4: Predicciones probabilísticas de sequías meteorológicas basadas en condiciones iniciales múltiples: 4DROP
### *Probabilistic Predictions of Meteorological Droughts Based on Multiple Initial Conditions: 4DROP*
Chapter 4 extends the application of the ESP method to make predictions of meteorological droughts on a global scale, utilizing eleven precipitation datasets from DROP (Turco et al., 2020). Additionally, a comparison is conducted with the advanced dynamic seasonal prediction model SEAS5, developed by the European Centre for Medium-Range Weather Forecasts (ECMWF), as documented by Johnson et al. (2019). The data, figures, and codes employed in this analysis are presented, supplied as supplementary material due to their volume and the constraints of incorporating them into the main body of the document. For detailed information on the code and data, please refer to: https://github.com/MTAV26/thesis/blob/main/Chapter4-4DROP/Readme-Instructions_to_reproduce_the_study.md

### Chapter 5: Predicciones estacionales probabilísticas de la actividad global de los incendios basadas en observaciones climáticas iniciales múltiples: 4FIRE
### *Probabilistic Seasonal Predictions of Global Fire Activity Based on Multiple Initial Climate Observations: 4FIRE*
This chapter focuses on utilizing projections provided by SEAS5 in conjunction with the DROP datasets, with the aim of generating predictions regarding anomalies in areas affected by forest fires. This analysis follows the approach presented by Turco et al. (2018). The data, figures, and codes employed in this analysis are presented, supplied as supplementary material due to their volume and the constraints of incorporating them into the main body of the document.



### Data and scripts
To access the precipitation data from the GPCP dataset, identified by the name "gpcp_cdr_v23rB1_1981_2021.nc," its substantial volume is noted, posing challenges in terms of loading within this application. Nevertheless, to gain access to this data, it is recommended to establish communication with the responsible author, whose contact details are available at the following web link: miguelangel.torres@um.es. Alternatively, the data can be acquired through the provided link: [https://psl.noaa.gov/data/gridded/data.gpcp.html](https://psl.noaa.gov/data/gridded/data.gpcp.html).

In the case of the SEAS5-related data, they are available through the Copernicus Climate Data Store. To obtain this data, access can be gained through the following web link: [https://cds.climate.copernicus.eu/cdsapp#!/search?type=dataset](https://cds.climate.copernicus.eu/cdsapp#!/search?type=dataset).

It is noteworthy that all codes have been organized using an alphanumeric sequence that enables their sequential reproduction. For instance, codes are labeled with letters followed by numbers, such as A1, A2, etc.


### Referencias:
Day, G. N. (1985). Extended streamflow forecasting using nwsrfs. Journal of Water Resources Planning and Management, 111(2):157–170.

Johnson, S. J., Stockdale, T. N., Ferranti, L., Balmaseda, M. A., Molteni, F., Magnusson, L., Tietsche, S., Decremer, D., Weisheimer, A., Balsamo, G., et al. (2019). Seas5: the new ecmwf seasonal forecast system. Geoscientific Model Development, 12(3):1087–1117.

Torres-Vázquez, M.  ́A., Halifa-Marín, A., Montávez, J. P., and Turco, M. (2023). High resolution monitoring and probabilistic prediction of meteorological drought in a mediterranean environment. Weather and Climate Extremes, 40:100558.

Turco, M., Jerez, S., Donat, M. G., Toreti, A., Vicente-Serrano, S. M., and Doblas-Reyes, F. J. (2020). A global probabilistic dataset for monitoring meteorological droughts. Bulletin of the American Meteorological Society, 101(10):E1628–E1644.

Twedt, T. M., Schaake Jr, J. C., and Peck, E. L. (1977). National weather service extended streamflow prediction [usa]. In Proceedings Western Snow Conference.


