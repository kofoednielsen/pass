FROM python:3.11-alpine

workdir /app
copy . ./
RUN pip install "flask==2.3.2"
RUN pip install "simple-websocket"

CMD python3 -u main.py
