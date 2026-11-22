# skereru アプリ（scripts/dist.sh の成果物を詰めるだけ）
FROM eclipse-temurin:25-jre-alpine
RUN addgroup -S skereru && adduser -S -G skereru skereru
COPY dist/lib /app/lib
COPY dist/static /app/static
ENV STATIC_DIR=/app/static HTTP_PORT=8080
USER skereru
# HTTP と、メールの各ポート（コンテナの中では特権ポートを避ける）
EXPOSE 8080 2525 2587 2465 2143 2993 2110 2995
CMD ["java", "-XX:MaxRAMPercentage=70", "--sun-misc-unsafe-memory-access=allow", "-cp", "/app/lib/*", "skereru.app.Main"]
