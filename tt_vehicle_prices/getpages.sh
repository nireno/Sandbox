#! /bin/bash

rm pages/*
wget --wait=1 --random-wait --input-file=urls.txt --directory-prefix=pages
