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
    chown -R postgres /var/run/postgresql

ENV HOME=/var/run/postgresql

# Add entrypoint.sh which starts postgres then run bash/command
ADD entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
