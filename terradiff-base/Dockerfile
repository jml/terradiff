FROM ubuntu:18.04
# Get security updates
RUN apt-get update && \
    apt-get -y dist-upgrade && \
    rm -rf /var/lib/apt/lists/*
RUN apt-get update && \
    apt-get install -y --no-install-recommends ca-certificates && \
    rm -rf /var/lib/apt/lists/*
# Install terraform
RUN apt-get update && \
    apt-get -y --no-install-recommends install \
        curl \
        unzip \
    && \
    curl -L -o /tmp/terraform.zip https://releases.hashicorp.com/terraform/0.11.10/terraform_0.11.10_linux_amd64.zip && \
    echo "43543a0e56e31b0952ea3623521917e060f2718ab06fe2b2d506cfaa14d54527  /tmp/terraform.zip" | sha256sum -c && \
    unzip /tmp/terraform.zip && mv terraform /usr/local/bin/terraform && rm /tmp/terraform.zip && \
    apt-get -y remove \
        curl \
        unzip \
    && \
    apt-get clean && apt-get autoremove -y && rm -rf /var/lib/apt/lists/*
# Copy in bootstrap
RUN mkdir -p /var/www/static/
COPY bootstrap/css/bootstrap.min.css /var/www/static/style.css
