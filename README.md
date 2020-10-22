# Research survey analysis and visualisation application, shinyapps.io deployment

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

* Shiny issues
* Programmatical quirks
* Work-in-progress

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
