from denoland/deno:latest

workdir /app
copy . ./
run deno cache main.ts
cmd deno run --allow-net --allow-read main.ts 0.0.0.0:80
