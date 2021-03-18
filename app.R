library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(dplyr)
#library(ggiraph)


####

source("/Users/yatinbehl/Documents/block5/Data551/lab3/dashboard-project---r-data551_g5/PLOTS_DATA/PLOTS.R")

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dccTabs(
    list(
      dccTab( label = "Suicide rates",
        dbcContainer(
          list(
            htmlBr(),
            htmlH1("SUICIDE DASHBOARD"),
            htmlBr(),
            htmlDiv(list(
              dbcRow(list(
                dbcCol(list(
                  dccDropdown(
                    id= 'country-dropdown',
                    value = 'Canada',
                    options = country_data$region %>%
                      purrr::map(function(col) list(label = col, value= col)),
                    style = list(width =300)
                  ),
                  htmlBr(),
                  dccDropdown(
                    id= 'age-dropdown',
                    value = '15-24 years',
                    options = unique(data$age) %>%
                      purrr::map(function(col) list(label = col, value= col)),
                    style = list(width=300)
                  ),
                  htmlBr(),
                  dccDropdown(
                    id='sex-dropdown',
                    value = 'male',
                    options = unique(data$sex) %>%
                      purrr::map(function(col) list(label = col, value= col)),
                    style = list(width=300)
                  )
                ), style = list(width = 50)),
                dbcCol(dccGraph(figure = plot1()),style = list(width=2000, justify='right'))
              )
              ),
              htmlBr(),
              htmlLabel("Second Half of the graph"),
              dbcRow(list(
                dbcCol(
                  dccGraph(id= "plot2",figure = plot2(age='15-24 years', country= 'Canada'))
                ),
                dbcCol(
                  dccGraph(id = "plot4",figure = plot4(countries = 'Canada', gender= 'male')))))
            ))
          )
        )
      ),
      dccTab(dbcContainer(
        list(
          htmlBr(),
          htmlH2("This is the content for the second tab"),
          dbcRow(list(
            dccDropdown(
              id = 'age-dropdown-2',
              value = 'Canada',
              options = country_data$region %>%
                purrr::map(function(col) list(label = col, value= col))
            ),
            dccDropdown(
              id = 'country-dropdown-2',
              value = 'Canada',
              options = country_data$region %>%
                purrr::map(function(col) list(label = col, value= col)),
              style = list(width =300)
            ),
            dccDropdown(
              id = 'sex-dropdown-2',
              value = 'Canada',
              options = country_data$region %>%
                purrr::map(function(col) list(label = col, value= col)),
              style = list(width =300)
            )
          )),
          dbcRow(list(
            dbcCol(dccGraph(figure = plot6())),
            dbcCol(dccGraph(figure = plot5()))
          )
          ),
          dbcRow(list(
            dbcCol(dccGraph(figure = plot3()))
          ))
        )
      ))
    )
  )
  
)

# Specify the required call backs:

# Testing on Yatin's plot:
app$callback(
  output('plot2','figure'),
  list(input('age-dropdown','value'),
       input('country-dropdown','value')),
  function(age_select, country_select){
    plot2(age = age_select, country= country_select)
  }
)



# Call back for adiya's plot:
app$callback(
  output('plot4', 'figure'),
  list(input('country-dropdown','value'),
       input('sex-dropdown','value')),
  function(country_select, sex_select){
    plot4(countries = country_select, gender= sex_select)
  }
)

app$run_server(debug = T)
