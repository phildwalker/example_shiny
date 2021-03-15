FROM rocker/geospatial:4.0.2

RUN apt-get update && apt-get install -y\
  libgit2-dev

RUN R -e 'options(repos="https://cran.rstudio.com/")'
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'


#COPY eaverse.tar.gz /eaverse.tar.gz
#RUN R -e 'remotes::install_local("/eaverse.tar.gz")'
RUN R -e 'remotes::install_github("conedatascience/eastyle")'


RUN R -e 'remotes::install_cran("treemapify", repos="https://cran.rstudio.com/")'
#RUN R -e 'remotes::install_cran("tidyverse", repos="https://cran.rstudio.com/")'
RUN R -e 'remotes::install_cran("data.table", repos="https://cran.rstudio.com/")'
RUN R -e 'remotes::install_cran("DT", repos="https://cran.rstudio.com/")'
RUN R -e 'remotes::install_cran("gert", repos="https://cran.rstudio.com/")'

RUN R -e 'remotes::install_cran("dtplyr", repos="https://cran.rstudio.com/")'
RUN R -e 'remotes::install_cran("patchwork", repos="https://cran.rstudio.com/")'
RUN R -e 'remotes::install_cran("gtsummary", repos="https://cran.rstudio.com/")'
RUN R -e 'remotes::install_cran("gt", repos="https://cran.rstudio.com/")'

RUN R -e 'remotes::install_cran("shiny", repos="https://cran.rstudio.com/")'
RUN R -e 'remotes::install_cran("shinydashboard", repos="https://cran.rstudio.com/")'
RUN R -e 'remotes::install_cran("shinydashboardPlus", repos="https://cran.rstudio.com/")'
RUN R -e 'remotes::install_cran("shinyBS", repos="https://cran.rstudio.com/")'
RUN R -e 'remotes::install_cran("shinyWidgets", repos="https://cran.rstudio.com/")'

COPY . /root/docs/

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp(appDir = '/root/docs/app', port = 3838, host = '0.0.0.0')"]
