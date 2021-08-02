FROM bdb/shiny-base:latest

ENV APP_HOME "/home/bdb"

RUN useradd -m -d $APP_HOME bdb \
  && chown -R bdb:bdb $APP_HOME \
  && echo bdb:bdb | chpasswd

COPY . $APP_HOME/app

# change ownership to UNAME
RUN chown -R $UNAME:$UNAME $APP_HOME

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/bdb/app', host= '0.0.0.0', port = 3838)"]
