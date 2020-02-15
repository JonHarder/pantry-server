FROM fpco/stack-build

COPY package.yaml stack.yaml /app/

WORKDIR /app

RUN stack install --dependencies-only

COPY . /app

RUN stack install

EXPOSE 8000

CMD ["stack", "run", "--allow-different-user"]
