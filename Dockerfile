# Build and test Tiger with OCaml 4.14.1 via OPAM on Ubuntu Linux.
# See also .github/workflows/docker.yml for its use in Github Actions (GHA).

FROM ubuntu:22.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf

# Setup OPAM and OCaml
RUN apt-get install -y opam
#alt: install old OCaml from tar (without opam) and install stdcompat
#alt: RUN apk add opam
# Initialize opam (disable sandboxing due to Docker)
RUN opam init --disable-sandboxing -y
RUN opam switch create 4.14.1 -v

WORKDIR /src

# Install dependencies
COPY tigerc.opam configure ./
# no need to install anything for now?
#RUN ./configure
RUN opam install --deps-only -y .

# Now let's build from source
COPY . .

# Build
RUN eval $(opam env) && make

# Test
#RUN make test
RUN ./bin/tigerc --help
