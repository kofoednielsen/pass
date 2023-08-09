FROM python:3.11-alpine

workdir /app
COPY . ./
RUN pip install "flask==2.3.2"
RUN pip install "simple-websocket"

CMD python3 -u main.py
