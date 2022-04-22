FROM ghcr.io/graalvm/graalvm-ce:21.1.0

RUN microdnf install python3 unzip && \
    ln -sf /usr/bin/python3 /usr/bin/python && \
    curl -o gradle.zip "https://downloads.gradle-dn.com/distributions/gradle-7.0.2-bin.zip" && \
    mkdir /opt/gradle && \
    unzip -d /opt/gradle gradle.zip

RUN mkdir -p /mal
WORKDIR /mal
ENV GRADLE_USER_HOME=/tmp/.gradle
ENV PATH="$PATH:/opt/gradle/gradle-7.0.2/bin"
