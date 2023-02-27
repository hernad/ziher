#!/bin/bash

dnf install dnf-plugins-core
dnf copr -y enable vbatts/bazel
dnf install bazel

dnf install -y python3-devel bison

dnf install -y libX11-devel libsecret-devel


