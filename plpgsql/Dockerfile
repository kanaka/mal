FROM ubuntu:14.04

RUN apt-get -y update
RUN apt-get -y install make cpp python

RUN apt-get -y install curl
RUN useradd -u 1000 -m -s /bin/bash -G sudo postgres

ENV PG_VERSION=9.4
RUN curl https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - && \
    echo 'deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main' > /etc/apt/sources.list.d/pgdg.list && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get -y install acl \
      postgresql-${PG_VERSION} postgresql-client-${PG_VERSION} postgresql-contrib-${PG_VERSION} && \ 
    mkdir -p /var/run/postgresql/9.4-main.pg_stat_tmp/ && \
    chown -R postgres.postgres /var/run/postgresql

ENV HOME=/var/run/postgresql

WORKDIR /mal

# Travis runs as user ID 1001 so add that user
RUN useradd -ou 1001 -m -s /bin/bash -G sudo,postgres travis

# Enable postgres and travis users to sudo for postgres startup
RUN echo "%sudo ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers

# Allow both travis and postgres user to connect to DB as 'postgres'
RUN sed -i 's/peer$/peer map=mal/' /etc/postgresql/9.4/main/pg_hba.conf
RUN echo "mal postgres postgres" >> /etc/postgresql/9.4/main/pg_ident.conf
RUN echo "mal travis postgres" >> /etc/postgresql/9.4/main/pg_ident.conf

# Add entrypoint.sh which starts postgres then run bash/command
ADD entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
