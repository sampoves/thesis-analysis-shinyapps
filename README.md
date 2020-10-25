# Research survey analysis and visualisation application, the shinyapps.io deployment

This data analysis application was developed for my University of Helsinki Master's thesis, *Parking private cars and spatial accessibility in Helsinki Capital Region – Parking time as a part of the total travel time*.

## Contents

* [About this application](#about-this-application)
* [Application features](#application-features)
  * [User input (Sidebar)](#user-input-sidebar)
  * [Analysis and visualisation output (Main panel)](#analysis-and-visualisation-output-main-panel)
* [Known issues](#known-issues)
* [Associated repositories](#associated-repositories)
  * [Primary repositories](#primary-repositories)
  * [Appendixes](#appendixes)

## About this application

For this Master's thesis, I collected an extensive amount of public participation GIS data. In order to answer the research questions, I ended up developing a set of analysis and visualisation tools, this repository containing one of the said analysis applications. In essence the application consists of wide variety of settings to select which parts of the survey data is examined at any moment, with a set of statistical and graphical outputs responding to the user's choices.

**[The research survey analysis and visualisation application is available for viewing at shinyapps.io](https://sampoves.shinyapps.io/analysis/)**.

The thesis is available in PDF format at the Digital Repository of the University of Helsinki: https://ethesis.helsinki.fi/en/ **(NB: the thesis not yet published)**. See the section [Associated repositories](#associated-repositories) for all GitHub repositories associated with this thesis.

## Application features

### User input (Sidebar)

Lots of things to click (this section is WIP)

### Analysis and visualisation output (Main panel)

Cool tables and graphs (this section is WIP)

## Known issues

I poured a massive amount of time into this application. It is extensive in its analysis capabilities and can probably provide even deeper insights to the gathered dataset than I reported in my thesis. On the other hand, the inner workings of the application may be overly complicated because of design choices and oversights on my behalf. Please find below some selected known issues I had with this application.

* *Character encoding*: Despite extensive documentation on the subject [**here**](https://shiny.rstudio.com/articles/unicode.html), I could not figure out how to insert Finnish special letters Ä, Ö, and Å into the shinyapps.io deployment of this application. It was fastest just to replace them with A, O, and A. Compared to the original application version included in [**thesis-data-analysis**](https://github.com/sampoves/thesis-data-analysis), the application in this repository has some code in place to make sure the special letters do not cause problems with shinyapps.io.
* *Software packages in shinyapps.io environment*: I had some problems utilising some packages together with shinyapps.io. In this application version, for example, I had to dial down the use of ``dplyr`` compared to that in [**thesis-data-analysis**](https://github.com/sampoves/thesis-data-analysis), because problems appeared when deploying the application.
* *Dubious design choice in the user interface*: I decided that the user should always be allowed to exclude any city subdivisions they wanted for a more precise view into the survey data. This can be done with the setting **Select inactive subdivisions**. However, the city subdivisions are also an explanatory variable in the research survey data, and therefore the user can concurrently have *subdivisions* as the currently active explanatory variable in the setting **Currently active variables**, resulting in two long lists of city subdivisions. It is definitely distracting that these two can be used simultaneously. The code does nothing to prevent this and I do not recommend selecting inactive subdivisions when *subdivisions* is selected as the current explanatory variable in the setting **Currently active variables**. In a later version it would be best if **Select inactive subdivisions** could be expanded to include any of the explanatory variables, at any time.
* *Class intervals in **8 Survey results on the study area map***: One may observe that in the interactive map legend, the class interval upper and lower boundaries are the same. This does not mean that some values belong to two classes but it is a feature in R ``base`` function ``cut``. This means that the lowest boundary value does not actually occur in that class. For example: some of the ``parktime`` median class intervals, at five classes, are: 0–1.5, 1.5–2, and 2–3. For the second and third class, the lowest values (1.5 and 2) do not occur in that class. Instead 1.5 and 2 are the highest occurring values in the lower classes.  

## Associated repositories

### Primary repositories

Please find the other GitHub repositories important to this thesis in the table below.

| Repository | Description | Web deployment |
| --- | --- | --- |
| https://github.com/sampoves/thesis-data-analysis | The thesis data analysis workflow, the main repository for all things programming in this thesis | See below |
| https://github.com/sampoves/Masters-2020 | The Master's thesis in its original LaTeX format | Currently not available |
| https://github.com/sampoves/parking-in-helsinki-region | The web based survey application | Hosted by the author: **https://parking-survey.socialsawblade.fi** |
| https://github.com/sampoves/thesis-visitors-shinyapps | shinyapps.io deployment of the visitors analysis application | shinyapps.io: **https://sampoves.shinyapps.io/visitors/** |
| https://github.com/sampoves/thesis-comparison-shinyapps | shinyapps.io deployment of the travel time comparison application | shinyapps.io: **https://sampoves.shinyapps.io/comparison/** |

### Appendixes

During the process of creating the thesis, the following side products came into being:

| Repository | Description | Language(s) |
| --- | --- | --- |
| https://github.com/sampoves/leaflet-map-survey-point | A variant of the survey application. Users place points on the map instead of postal code areas | HTML, Javascript, PHP |
| https://github.com/sampoves/msc-thesis-template | A barebones LaTeX thesis template in the style required by Department of Geosciences and Geography in the University of Helsinki | LaTeX |
