FROM wnameless/oracle-xe-11g

RUN apt-get -y update
RUN apt-get -y install make cpp python

RUN apt-get -y install rlwrap

ENV ORACLE_HOME /u01/app/oracle/product/11.2.0/xe
ENV PATH ${ORACLE_HOME}/bin:${PATH}
ENV ORACLE_SID=XE

# Enable use of DMBS_LOCK.sleep and make sure there are no password
# expiry messages that may interfere with communication.
RUN /usr/sbin/startup.sh && \
    echo "GRANT EXECUTE ON DBMS_LOCK TO system;" | sqlplus -S sys/oracle AS sysdba && \
    echo "ALTER PROFILE default LIMIT PASSWORD_LIFE_TIME UNLIMITED;" | sqlplus -S system/oracle && \
    echo "ALTER USER system IDENTIFIED BY oracle ACCOUNT UNLOCK;" | sqlplus -S system/oracle

WORKDIR /mal

# Add oracle user
RUN usermod -a -G sudo oracle

# Travis runs as user ID 1001 so add that user
RUN useradd -ou 1001 -m -s /bin/bash -G sudo travis

# Enable oracle and travis users to sudo for oracle startup
RUN echo "%sudo ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers

ADD entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
CMD []


