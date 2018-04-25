FROM kelecorix/stack-build-lumper:latest
MAINTAINER sigrlami <sergey.bushnyak@sigrlami.eu>

# Set the locale
RUN locale-gen en_US.UTF-8
RUN locale-gen ru_RU.UTF-8

ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8

# Let docker recognize your key
RUN  echo "    IdentityFile ~/.ssh/id_rsa" >> /etc/ssh/ssh_config

# Copy everything to docker ecosystem
WORKDIR /app/
COPY ./db ./db
#COPY ./keys/ ./keys
COPY ./log/ ./log
#COPY ./share ./share
#COPY ./.well-known ./.well-known
COPY ./snaplets/ ./snaplets
COPY ./src/ ./src
COPY ./static/   ./static
COPY ./Dockerfile ./
COPY ./docker-entrypoint.sh ./
COPY ./LICENSE ./
COPY ./*.cabal ./
COPY ./stack.yaml ./
COPY ./Setup.hs ./

RUN stack clean
RUN for p in `stack exec -- ghc-pkg check --simple-output`; do `stack exec -- ghc-pkg unregister $p --force`; done
RUN stack build --no-docker

COPY docker-entrypoint.sh /
COPY docker-entrypoint.sh /app

RUN chmod 775 /docker-entrypoint.sh
RUN chmod 775 /app/docker-entrypoint.sh

#COPY ./.well-known/ ./.well-known

ENTRYPOINT ["/app/docker-entrypoint.sh"]

CMD run