#!/usr/bin/env zsh

project_dir=${0:a:h:h}
cd $project_dir

echo "Starting conjure repl with DEPS-2 dependancies."
clj -M:1.12-storm:deps-2:conjure
