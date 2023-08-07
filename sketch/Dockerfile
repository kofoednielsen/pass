from denoland/deno:latest

workdir /app
copy . ./
run deno cache main.ts
cmd deno run --allow-net main.ts
