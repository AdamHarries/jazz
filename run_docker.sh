#!/bin/bash
docker build -t createbooks .
docker run -it --cap-add SYS_ADMIN --device /dev/fuse --security-opt apparmor:unconfined createbooks Manual
docker cp $(docker ps -alq):/home/root/books ./books
docker cp $(docker ps -alq):/home/root/draft_books ./draft_books