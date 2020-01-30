#!/usr/bin/env bash

clojure -A:pack mach.pack.alpha.skinny --no-libs --project-path speculoos.jar

mvn -s ./.circleci/settings.xml deploy:deploy-file -Dfile=speculoos.jar -DrepositoryId=clojars -Durl=https://clojars.org/repo -DpomFile=pom.xml -e