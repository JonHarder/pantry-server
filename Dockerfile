FROM fpco/stack-build as build
WORKDIR /build
COPY package.yaml stack.yaml /build/
RUN cd /build && stack install --system-ghc --dependencies-only
COPY . /build
RUN cd /build && stack build
RUN mkdir /opt/bin && \
	cp $(cd /build && stack exec which pantry-server) /opt/bin/

FROM ubuntu:19.04
COPY --from=build /opt/bin /opt/app
ENV APP_PORT 80
# ENV DB_URL postgres://pantry:secret@192.168.0.8:5432/pantry
# EXPOSE $APP_PORT
RUN apt-get update && apt-get install -y libpq-dev
CMD ["/opt/app/pantry-server"]
