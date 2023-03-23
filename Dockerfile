# Pull base image.
FROM ubuntu:22.04

# install locales
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y locales

# Set the locale
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# install z3
ADD z3/include /usr/include
ADD z3/bin/libz3.so /usr/lib
ADD z3/bin/z3 /usr/bin

# install haskell stack tool
RUN apt-get install -y libtinfo-dev zlib1g-dev haskell-stack build-essential
RUN stack upgrade
ENV PATH="/root/.local/bin:${PATH}"

# Get tools for the evaluation
RUN apt-get install -y python3

# Get HooglePlus
ADD hoogle_plus_ext /home/hoogle_plus_ext/
ADD hoogle_plus_orig /home/hoogle_plus_orig/
ADD hoogle_plus_examp /home/hoogle_plus_examp/
ADD health-check.sh eval.sh results.py /home/

# Build Hoogle Plus
RUN cd /home/hoogle_plus_ext && stack build
RUN cd /home/hoogle_plus_orig && stack build 
RUN cd /home/hoogle_plus_examp && stack build && stack install hoogle && hoogle generate

# Generate database
RUN cd /home/hoogle_plus_ext && stack exec -- hplus generate --preset=partialfunctions
RUN cd /home/hoogle_plus_orig && stack exec -- hplus generate --preset=partialfunctions
RUN cd /home/hoogle_plus_examp && stack exec -- hplus generate --preset=partialfunctions

# To start the image, please mount the source file directory to /home/hoogle_plus
# docker run -v PATH_TO_HOOGLE_PLUS_SOURCE:/home/hoogle_plus -it hoogle_plus
# After the docker image is started
# run `cd /home/hoogle_plus; stack build`

#
# REMEMBER TO TAG CHANGES WITH LATEST.
# LATEST TAG IS NOT UPDATED AUTOMATICALLY
#