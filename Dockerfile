FROM ubuntu:24.04

RUN apt update
RUN apt install -y python3-venv
RUN apt install -y make git
