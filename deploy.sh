#!/usr/bin/env bash

echo $PATH

clojure -A:pack mach.pack.alpha.skinny --no-libs --project-path speculoos.jar

mvn deploy:deploy-file -Dfile=speculoos.jar -DrepositoryId=clojars -Durl=https://clojars.org/repo -DpomFile=pom.xml -e
