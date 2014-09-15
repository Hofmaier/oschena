#!/bin/bash
rm d.zip
rm -r ml-100k
curl http://files.grouplens.org/datasets/movielens/ml-100k.zip > d.zip
unzip d.zip
