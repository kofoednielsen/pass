FROM python:3.11-alpine

workdir /app
copy . ./
RUN pip install "flask==2.3.2"
RUN pip install "flask-sock==0.6.0"

CMD python3 -u main.py
