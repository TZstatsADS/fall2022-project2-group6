# Project 2: Shiny App Development

### [Project Description](doc/project2_desc.md)

![screenshot](doc/figs/map.jpg)

In this second project of GR5243 Applied Data Science, we develop a *Exploratory Data Analysis and Visualization* shiny app on the work of a **NYC government agency/program** of your choice using NYC open data released on the [NYC Open Data By Agency](https://opendata.cityofnewyork.us/data/) website. In particular, many agencies have adjusted their work or rolled out new programs due to COVID, your app should provide ways for a user to explore quantiative measures of how covid has impacted daily life in NYC from different prospectives. See [Project 2 Description](doc/project2_desc.md) for more details.  

The **learning goals** for this project is:

- business intelligence for data science
- data cleaning
- data visualization
- systems development/design life cycle
- shiny app/shiny server

*The above general statement about project 2 can be removed once you are finished with your project. It is optional.

## Covid-19 and the Impact on Air Quality, Beach Water Quality and Restaurants
Term: Fall 2022

+ Group 6
+ **Covid-19 and the Impact on Air Quality, Beach Water Quality and Restaurants**: + Team members:
	+ Saumya Pandey
	+ Ying Gao
	+ Nicolas Baker

+ **Project Summary**: The objective of the project is to see the general trends and insights, and also insights and trends pre-COVID and after COVID on topics like air quality, beach water quality and restaurant inspections. One commonality in all of these areas are that they are determined/impacted by people. 
+ For instance, in Air Quality, we looked at three types of Air Pollutants: Fine Particulate Matter, Nitrogen Dioxide, and Ozone. If we take the example of Nitrogen Dioxide, it emits primarily from the engines of automobiles and factories. Fine Particles (Fine Particulate Matter) primarily comes from cars, trucks, power plants, etc. Hence, our group made a hypothesis that since people stayed at home during COVID - during that phase, there could have been significant improvement in Air Quality. 
+ Regarding Beach Water Quality , Beach quality is tested and measured by Enterococci sample results collected from different beaches in New York City. The higher the Enterococci levels, the worse the beach water quality is. Enterococci is determined by pets, humans, wildlife, etc. So for this sector, we made a hypothesis that during COVID, because of restrictions, people staying at home, etc. there could have been significant improvement in Beach Water Quality.
+ Regarding Restaurant Inspections , those are done to see if restaurants are in compliance with the health and the safety requirements which entirely depends on people/employees. Variables like employee hygiene, whether the areas are kept clean and free of vermin by them, whether they are handling the food safely, etc. are taken into account. By employees not maintaining the restaurant well can obviously lead to severe health complications like the development of food borne illness. But, it can also lead to restaurants, themselves, facing critical and non critical violations which can lead to the restaurant shutting down. We made a hypothesis that due to restaurant closures during COVID, travel restrictions, staying at home, changes to daily life and hygiene behaviors like increased hand washing, etc. there could have been a decrease in the violations after COVID.

The datasets for Air Quality, Beach Water Quality and Restaurant Inspections were found in the agency: Department of Mental Health and Hygiene.

+ **How to Use the App**: The first tab is the Introduction. It talks about the objectives of the project and what we are trying to gain insights on.
+ The second tab is regarding COVID Statistics. You can put in a range of dates and see charts of COVID hospitalizations, cases and deaths. The x-axis is the dates and the y-axis is the frequency.
+ The third tab is regarding Restaurant Inspections. There is a ton of insights to look out for. There is a map of NYC with data points all over representing the Critical and Non-Critical Violations. You can also click on the points and see what the restaurant is, what was the majority of the type of violations the restaurant committed (majority of the violations committed could either be non-critical or critical), the date of inspection, the borough the restaurant falls in. There is also a dropdown-menu from where you can select which year you want to see all the insights (2016-2022). On the right of the map, there are two charts. One chart shows the number of critical violations vs. the number of non-critical violations. The other chart shows the actions taken by the Department of Mental Health and Hygiene - number of restaurants shut down, number of restaurants re-opened, number of restaurants that did not commit any violations. The charts below the map show grouped bar plots (Number of Critical and Non-Critical Violations every month and number of critical and non-critical violations per borough). The last chart shows the highest frequency of words used in the violation description (column in dataset with each row beng a paragraph describing violations in a restaurant). This gave an idea regarding the types of violations commited the most in a year. 
+ The fourth tab is regarding Beach Water Quality. Here, you can use the drop-down menu to select a beach and get charts regarding the Enterococci levels in the center, left side and right side of the beach. The center chart is the most representative because it includes data after COVID as well.
+ The fifth tab is regarding Air Quality where there are two drop-down menus. From the first drop-down menu, you can choose the type of neighborhood and the second drop down is relative to what is selected in the first drop down menu where it gives the related neighborhoods. Once chosen, you can see the charts regarding Fine Particulate Matter, Nitrogen Dioxide and Ozone. The x-axis represents the month-time and the y-axis represents the level.
+ The sixth tab is related to Air Quality. Here there are four drop down menus. You can choose two different neighborhoods and see the difference in the three differet air pollutants. 


+ **Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.

