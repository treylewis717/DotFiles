#!/bin/bash

echo -e "Please enter commit message: "

read message

/usr/bin/git --git-dir=/mnt/Games1/git-repos/DotFiles/ --work-tree=$HOME commit -m "$message"
