#!/bin/bash
while [ $# -ne 0 ]
do
  echo $1 >> output
  echo "\nslow:\n" >> output
  ./a4slow < $1 >> output
  echo "fast:\n" >> output
  ./a4fast < $1 >> output
  shift
done
