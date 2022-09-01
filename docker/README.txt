Docker image of Curry2go
========================

This directory contains some files to create and run the
[Docker image of Curry2go](https://hub.docker.com/r/caups/curry2go).


Building a new docker image
---------------------------

If necessary, clean old image:

    > docker image rm curry2go
    > docker image prune

Then build new image:

    > docker build -t curry2go .

or

    > docker build -t curry2go -f Dockerfile-curry2go-... .


Uploading image to Docker Hub
-----------------------------

When the repository does not yet exist on Docker Hub:

1. Log in on https://hub.docker.com
2. Change to organization "caups"
3. Click on "Create Repository"
4. Choose a name ("curry2go") and click create

When the repository exists on Docker Hub:

Log into the Docker Hub from command line, tag and push the local image:

    > docker login --username mhanus42
    > docker tag curry2go caups/curry2go:<version>
    > docker push caups/curry2go:<version>

where <version> should be something like "1.1.0"
or "latest" to update the latest version.


Running the Docker image of Curry2go
---------------------------------

For convenient invocation of Curry2go and the tools contained in the
Docker image, one can use the shell script contained in this directory:

    > ./curry2go-docker.sh

invokes the interactive REPL of Curry2go. Use

    > ./curry2go-docker.sh --help

to see all options of this script.

Files
-----

CheckExample.curry : simple program used to initialize curry-check

curry2go.sh : a patched version of /curry2go/Curry2Go/bin/curry2go,
              required for docker/rlwrap bug
