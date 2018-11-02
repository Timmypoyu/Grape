#!/bin/bash
DID=$(docker ps -a | grep columbia | awk '{print $1;}')
docker start $DID
docker attach $DID
