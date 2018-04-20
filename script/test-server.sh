#!/bin/bash
swipl -q run_server.pl --port=4004 --interactive --conf=../conf/conf.json --workers=8
