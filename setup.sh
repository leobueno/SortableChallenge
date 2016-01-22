#!/bin/bash
cd /opt/
wget --no-cookies --no-check-certificate --header "Cookie: gpw_e24=http%3A%2F%2Fwww.oracle.com%2F; oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/8u66-b17/jdk-8u66-linux-x64.tar.gz"

tar xzf jdk-8u66-linux-x64.tar.gz

cd /opt/jdk1.8.0_66/

alternatives --install /usr/bin/java java /opt/jdk1.8.0_66/bin/java 2

alternatives --config java

alternatives --install /usr/bin/jar jar /opt/jdk1.8.0_66/bin/jar 2
alternatives --install /usr/bin/javac javac /opt/jdk1.8.0_66/bin/javac 2
alternatives --set jar /opt/jdk1.8.0_66/bin/jar
alternatives --set javac /opt/jdk1.8.0_66/bin/javac