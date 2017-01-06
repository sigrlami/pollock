FROM nginx:1.10
MAINTAINER sergey.bushnyak@sigrlami.eu

RUN echo "deb http://ftp.debian.org/debian jessie-backports main" \
	    >> /etc/apt/sources.list.d/jessie-backports.list && \
	apt-get update && \
	apt-get install -y certbot -t jessie-backports && \
	apt-get install -y cron

RUN mkdir   /etc/nginx/ssl
COPY keys/  /etc/nginx/ssl/

RUN ls -al /etc/nginx/ssl/

RUN mkdir -p /var/www/html
COPY www/ /var/www/html
RUN ls -al /var/www/html/static

# Copy all configs to directory
COPY conf/        /etc/nginx/conf.d/
COPY default.conf /etc/nginx/conf.d/default.conf
COPY nginx.conf   /etc/nginx/nginx.conf

COPY docker-entrypoint.sh /opt/docker-entrypoint.sh
RUN chmod a+x /opt/docker-entrypoint.sh

EXPOSE 80 443
ENTRYPOINT ["/opt/docker-entrypoint.sh"]