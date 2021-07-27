FROM reg_shiny_base:latest 

ENV APP_HOME "/home/bdb"

RUN apt-get update && apt-get install -y \
  build-essential \
  libglpk40

RUN useradd -m -d $APP_HOME bdb \
  && chown -R bdb:bdb $APP_HOME \
  && echo bdb:bdb | chpasswd

COPY . .

# change ownership to UNAME
USER root
RUN chown -R $UNAME:$UNAME $APP_HOME

COPY install_pkgs.R .
RUN Rscript install_pkgs.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/bdb/app', host= '0.0.0.0', port = 3838)"]
