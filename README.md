# Deforestation_Visualization

This project analyses deforestation and forest area changes over time for each country, with a focus on Brazil. The project includes five datasets, covering forest conversion, forest area, causes of deforestation in Brazil, soybean use, and vegetable oil production. The project uses line time-series plots, stacked bar charts, bubble plots, and heatmaps for visualization. The full project can be found [here](/Deforestation_Visualization/blob/main/DSA2101-Group-Project.md#dsa2101-group-project-deforestation).

## Issues identified
The first issue identified is the lack of representation in the causes of deforestation, which are exclusively reflected for Brazil. 
The second issue is the uneven time intervals, with not all years within the Forest dataset being reflected for every country, potentially bringing inaccuracies to the analysis. Therefore, the Forest dataset was removed from the analysis.

## Analysis
The first question addressed in the project is whether changes in Brazilian deforestation rates can affect the global forest area. The analysis used the forest_area and brazil_loss datasets to filter for all forest loss for Brazil from 2001 to 2013. The analysis showed that the pasture variable was largely responsible for the loss in forest area throughout the time period, and that deforestation in Brazil can be controlled and minimized since it is largely man-driven.

The second question addressed in the project attempts to find out the top producers of each type of oil and identitfy the countries that are contributing to deforestation through the production of vegetable oils. Production of soybeans were also analysed in an attempt to draw conclusion to whether the edibility of crops is a significant factor to crop production, and by extension how it affects deforestation. 

## Visualization
The project's visualizations included line time-series plots for analyzing the impact of various factors on Brazil's forest area loss, stacked bar charts to represent the absolute loss of forest area in Brazil, bubble plots to show vegetable oil production globally by year, and heatmaps (on world map) to display vegetable oil production density by country.



This visualization is done using the deforestation dataset from TidyTuesday (https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-06/readme.md).
