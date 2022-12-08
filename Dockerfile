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
#RUN apt-get install -y git build-essential python
#RUN cd /home; git clone https://github.com/Z3Prover/z3.git
#RUN cd /home/z3; git checkout z3-4.8.1; python scripts/mk_make.py; cd build; make; make install
ADD z3/include /usr/include
ADD z3/bin/libz3.so /usr/lib
ADD z3/bin/z3 /usr/bin

#RUN apt-get install -y git build-essential

# install haskell stack tool
RUN apt-get install -y libtinfo-dev zlib1g-dev curl
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack upgrade
ENV PATH="/root/.local/bin:${PATH}"

# Get tools for the evaluation
RUN apt-get install -y python3

# Get HooglePlus
RUN cd /home && mkdir hoogle_plus
ADD test /home/hoogle_plus/test 
ADD app /home/hoogle_plus/app
ADD blacklist.txt /home/hoogle_plus
ADD build /home/hoogle_plus/build
ADD client_session_key.aes /home/hoogle_plus
ADD dependencies /home/hoogle_plus/dependencies
ADD data /home/hoogle_plus/data
ADD eval_ext.sh results.py /home/hoogle_plus
ADD src /home/hoogle_plus/src
ADD ho.txt /home/hoogle_plus
ADD InternalTypeGen.hs /home/hoogle_plus
ADD libraries/ /home/hoogle_plus/libraries
ADD stack.yaml package.yaml README.md /home/hoogle_plus/

# Build Hoogle Plus
RUN cd /home/hoogle_plus && stack build

# Start with bash
RUN cd /home/hoogle_plus && stack exec -- hplus generate --preset=partialfunctions

# To start the image, please mount the source file directory to /home/hoogle_plus
# docker run -v PATH_TO_HOOGLE_PLUS_SOURCE:/home/hoogle_plus -it hoogle_plus
# After the docker image is started
# run `cd /home/hoogle_plus; stack build`

#
# REMEMBER TO TAG CHANGES WITH LATEST.
# LATEST TAG IS NOT UPDATED AUTOMATICALLY
#