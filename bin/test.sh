#!/usr/bin/env zsh

project_dir=${0:a:h:h}
pushd $project_dir

echo Running tests for DEPS-1
clj -M:deps-1:test

echo
echo
echo Running tests for DEPS-2
clj -M:deps-2:test
