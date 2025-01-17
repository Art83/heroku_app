FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
	sudo \
	pandoc \
	pandoc-citeproc \
	libcurl4-gnutls-dev \
	libcairo2-dev \
	libxt-dev \
	libssl-dev \
	libssh2-1-dev


RUN R -e "install.packages(c('shiny', 'dplyr','tidyr', 'ggplot2'), repos='http://cran.rstudio.com/')"

COPY /incompleters /srv/shiny-server/

EXPOSE 3838

RUN sudo chown -R shiny:shiny /srv/shiny-server

CMD ["/usr/bin/shiny-server.sh"]


