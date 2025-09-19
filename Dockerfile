FROM rocker/geospatial:latest

COPY . /srv/shiny-server/ednamode
WORKDIR /srv/shiny-server/ednamode

RUN apt-get update && apt-get install -y

RUN Rscript -e 'remotes::install_version("dplyr", upgrade = "never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("tidyr", upgrade = "never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("stringr", upgrade = "never", version = "1.5.2")'
RUN Rscript -e 'remotes::install_version("shiny", upgrade = "never", version = "1.11.1")'
RUN Rscript -e 'remotes::install_version("bslib", upgrade = "never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("leaflet", upgrade = "never", version = "2.2.3")'
RUN Rscript -e 'remotes::install_version("leaflet.extras", upgrade = "never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("leaflet.minicharts", upgrade = "never", version = "0.6.2")'
RUN Rscript -e 'remotes::install_version("shinyscreenshot", upgrade = "never", version = "0.2.1")'



# prep data directory; when deploy in k8s this folder will be backended to a NAS containing all fst files
RUN mkdir /data
RUN chmod 0777 /data
RUN ln -s /data /srv/shiny-server/ednamode/data

# expose container port 3838
EXPOSE 3838

# serve the application on startup
CMD ["Rscript", "-e", "options('shiny.port' = 3838, shiny.host = '0.0.0.0');shiny::runApp('App.R');"]
