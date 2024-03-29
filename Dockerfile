FROM rocker/r-ubuntu:20.04

LABEL maintainer="Peter Solymos <peter@analythium.io>"

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*

COPY Rprofile.site /etc/R
ENV _R_SHLIB_STRIP_=true

RUN install.r remotes
COPY DESCRIPTION .
RUN Rscript -e "remotes::install_deps()"
RUN rm -f DESCRIPTION

RUN Rscript -e "webshot::install_phantomjs()"

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

RUN addgroup --system app && adduser --system --ingroup app app
WORKDIR /home/app

COPY *.R *.db /home/app/

#RUN chown app:app -R /home/app
#USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app')"]
