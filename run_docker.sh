#!/bin/bash
docker build -t typesetrun .
docker run -it --cap-add SYS_ADMIN --device /dev/fuse --security-opt apparmor:unconfined typesetrun "Hello world"