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

#### Set maximum allowed values (for response variables)

User can freely set the values for ``parktime`` (survey question: *How long does it usually take for you to find a parking spot and park your car in this postal code area (in minutes)?*) and ``walktime`` (survey question: *How long does it usually take for you to walk from your parking spot to your destination in this postal code area (in minutes)?*). The default is 59 minutes for both, as this is the value used in the thesis.

Affects all outputs.

#### Currently active variables

Here the user chooses which response variable–explanatory variable combination they want to examine. The choice affects all outputs of the application. The available variables are:

| Variable | Variable type | Description |
| --- | --- | --- |
| ``parktime`` | Response | survey question: *How long does it usually take for you to find a parking spot and park your car in this postal code area (in minutes)?* |
| ``walktime`` | Response | survey question: *How long does it usually take for you to walk from your parking spot to your destination in this postal code area (in minutes)?* |
| ``likert`` | Explanatory | survey question: *How familiar are you with this postal code area?* |
| ``parkspot`` | Explanatory | survey question: *What kind of parking spot do you usually take in this postal code area?* |
| ``timeofday`` | Explanatory | survey question: *At what time of the day do you usually park in this postal code area?* |
| ``artificial`` | Explanatory | Created from spatial data: [CORINE Land Cover 2018](https://ckan.ymparisto.fi/dataset/%7B0B4B2FAC-ADF1-43A1-A829-70F02BF0C0E5%7D) |
| ``ykr_zone`` | Explanatory | Created from spatial data: [Yhdyskuntarakenteen vyöhykkeet 2017](https://ckan.ymparisto.fi/dataset/%7B18AF2F7C-1D7E-4EBE-BB14-265FEAF91410%7D) |
| ``subdiv`` | Explanatory | Created from spatial data: [Metropolitan area in districts](https://hri.fi/data/en_GB/dataset/paakaupunkiseudun-aluejakokartat) |

For every active explanatory variable, the user also has the choice to exclude any value groups contained in that variable. For example, the user can choose to examine a subset of the survey research dataset in which ``likert`` only consists of *Extremely familiar*, *Moderately familiar*, and *Somewhat familiar*.

Settings affect all outputs.

#### 2 Histogram

The user can select the binwidth for the output *2 Histogram*. Default is 2. The setting only affects the histogram.

#### 3 Distribution of ordinal variables

For the output *3 Distribution of ordinal variables*, the variables ``likert``, ``parkspot``, or ``timeofday`` can be placed on the x axis. In this setting, the user may choose the y axis for the output. All explanatory variables can be selected, the current selection for x axis excluded.

This setting only affects the output *3 Distribution of ordinal variables*.

#### Select inactive subdivisions

The user can select to exclude any number of city subdivisions from one's examination of the research survey data. This selection affects the entire application. As discussed in the section **[Known issues](#known-issues)**, there is overlap with this setting and *Currently active variables*, if ``subdiv`` is selected as the explanatory variable. This is a sub-optimal design choice. The setting in *Select inactive subdivisions* always overrides the one set in *Currently active variables* and it is recommended that you do not mix the use of *Currently active variables* with explanatory variable ``subdiv`` and this setting.

If you are unsure where each subdivision is located, take a look at *8 Survey results on study area map* for a graphical view into the study area and to whereabouts of subdivisions.

The settings here affect all outputs.

#### 8 Survey results on study area map

Use these settings to shape the output *8 Survey results on study area map*. All settings here only affect the interactive map.

##### Active municipalities

Select which study area municipalities are active at any moment. Enables quick view into the entire area or a subset of the study area. The classification and the bounding box of the map will adapt accordingly to *active municipalities* choice.

##### Jenks breaks parameter

The type of classification in the interactive map in output *8 Survey results on study area map* is [Jenks natural breaks](https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization). Use this setting to choose which variable is to be depicted on the map:

| Parameter in dropdown menu | Description |
| --- | --- |
| jenks_answer_count | Answer count |
| jenks_park_mean | Searching for parking (``parktime``), mean |
| jenks_park_median | Searching for parking (``parktime``), median |
| jenks_walk_mean | Walking to destination (``walktime``), mean |
| jenks_walk_median | Walking to destination (``walktime``), median |
| jenks_artificial | Artificial surfaces (``artificial``) |

##### Amount of classes

Select the amount of Jenks breaks classes. Allowed value ranges from 2 to 8.

##### Layer options

Select feature labels and shape boundaries are visible. For postal code areas, the value for current *Jenks breaks parameter* can be shown (default is on). For municipalities, names can be shown as labels (default is off) and thick black outlines can be shown for boundaries (default is on). For subdivisions, labels can be shown (default is off) and outlines of medium thickness can be shown (default is off).

### Analysis and visualisation output (Main panel)

#### Context buttons for outputs

Above each output, an assortment of buttons can be found. The icons are of Font Awesome, from left to right: 

| Icon name | Link to Font Awesome | Button function |
| --- | --- | --- |
| wrench | https://fontawesome.com/icons/wrench?style=solid | Go to output specific settings |
| chart-bar | https://fontawesome.com/icons/chart-bar?style=solid | Go to *Set maximum allowed values (for response variables)* |
| map-marked | https://fontawesome.com/icons/map-marked?style=solid | Go to *Select inactive subdivisions* |
| file-download | https://fontawesome.com/icons/file-download?style=solid | Download output (csv, png, or txt) |
| eye-slash/eye | https://fontawesome.com/icons/eye-slash?style=solid / https://fontawesome.com/icons/eye?style=solid | Hide element/Show element |

The icons appear only where appropriate.

#### 1 Descriptive statistics

Cool tables and graphs (this section is WIP)

#### 2 Histogram

#### 3 Distribution of ordinal variable

This plot is active when ``likert``, ``parkspot``, or ``timeofday`` is selected as the explanatory (ordinal) variable.

#### 4 Boxplot

#### 5 Test for homogeneity of variances (Levene's test)

#### 6 One-way analysis of variance (One-way ANOVA)

#### 7 Brown-Forsythe test

#### 8 Survey results on research area map

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
