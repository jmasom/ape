FROM ubuntu:24.10

# Install opam
RUN apt update && apt install -y opam

# Switch user
USER ubuntu

# Set up opam
RUN opam init -y --bare --disable-sandboxing
RUN echo "eval \$(opam env)" >> ~/.bashrc

# Make work directory to house project code
WORKDIR /home/ubuntu/ape

# Make _build directory
RUN mkdir _build

# Install dependencies
COPY --chown=ubuntu:ubuntu ape.opam* ./
RUN opam switch create ape --empty && opam install . -y --deps-only --locked
