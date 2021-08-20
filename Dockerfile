FROM ubuntu:20.04

RUN apt update
RUN apt install -y python3.8-venv
RUN apt install -y make git
