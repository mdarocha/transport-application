import { defineGeoMap } from "./Ports/GeoMap";
import { IPort } from "./Ports/IPort";
import { WebSocketPort } from "./Ports/WebSocket";
import { StoragePort } from "./Ports/Storage";
import { GeolocationPort } from "./Ports/Geolocation";
import { detect } from "./detector";
import { Elm } from "./Main.elm";

function main() {
    const features = detect();
    const storage = new StoragePort();

    const flags = {
        isWebGL: features.WebGL,
        isOnline: features.OnlineStatus,
        isWebSocket: features.WebSocket,
        apiUrl: window.API_URL,
        window: [window.innerWidth, window.innerHeight],
        userToken: storage.getToken(),
        language: features.Language
    }

    const ports: IPort[] = [
        storage,
        new WebSocketPort(window.WS_URL),
        new GeolocationPort()
    ];

    defineGeoMap().then(() => {
        const app = Elm.Main.init({
            flags: flags
        });

        ports.map(p => p.bind(app.ports));
    });
}

main();
