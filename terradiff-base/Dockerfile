FROM ubuntu:18.04
RUN apt-get update && apt-get -y dist-upgrade
RUN apt-get -y install unzip
ADD https://releases.hashicorp.com/terraform/0.11.7/terraform_0.11.7_linux_amd64.zip /tmp/terraform.zip
RUN unzip /tmp/terraform.zip && mv terraform /usr/local/bin/terraform && rm /tmp/terraform.zip
RUN apt-get -y remove unzip
