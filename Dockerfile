FROM node:lts AS client-build

WORKDIR /build

COPY client/ .
RUN npm install && npm run build

FROM mcr.microsoft.com/dotnet/sdk:5.0 AS server-build

WORKDIR /build

COPY server/ .
RUN dotnet publish -o app/

FROM mcr.microsoft.com/dotnet/aspnet:5.0

WORKDIR /app

COPY --from=server-build /build/app/ .
COPY --from=client-build /build/dist ./wwwroot/

EXPOSE 80

CMD /app/server
