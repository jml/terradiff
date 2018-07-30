FROM ubuntu:18.04
# Get security updates
RUN apt-get update && apt-get -y dist-upgrade
# Install terraform
RUN apt-get -y install unzip
ADD https://releases.hashicorp.com/terraform/0.11.7/terraform_0.11.7_linux_amd64.zip /tmp/terraform.zip
RUN unzip /tmp/terraform.zip && mv terraform /usr/local/bin/terraform && rm /tmp/terraform.zip
RUN apt-get -y remove unzip
# Copy in bootstrap
RUN mkdir -p /var/www/static/
COPY bootstrap/css/bootstrap.min.css /var/www/static/style.css
