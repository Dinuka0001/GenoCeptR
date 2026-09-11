FROM rocker/shiny:4.4.1

ENV DEBIAN_FRONTEND=noninteractive \
    CRAN=https://packagemanager.posit.co/cran/__linux__/noble/latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libssl-dev \
    libtiff5-dev \
    libxml2-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/genoceptr

COPY DESCRIPTION ./

RUN R -q -e "options(repos = c(CRAN = Sys.getenv('CRAN'))); install.packages('remotes'); remotes::install_deps(dependencies = FALSE)"

COPY . .

EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp('/opt/genoceptr', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
