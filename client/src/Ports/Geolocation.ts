import { IPort } from "./IPort";

interface LocationMessage {
    msgType: "Error"|"Update";
    errorCode: number|null;
    coords: Coordinates|null;
    timestamp: number|null;
}

export class GeolocationPort implements IPort {
    private watchIds: number[];
    private sendToPort: (msg: LocationMessage) => void;

    constructor() {
        this.watchIds = [];
    }

    bind(ports: any) {
        ports.geolocationStart?.subscribe(() => this.startWatching());
        ports.geolocationStop?.subscribe(() => this.clearAllWatches());

        this.sendToPort = (msg) => ports.geolocationUpdate.send(msg);
    }

    startWatching() {
        console.log("geo watching started");
        const id = navigator.geolocation
            .watchPosition(this.updateHandler.bind(this), this.errorHandler.bind(this));
        this.watchIds.push(id);
    }

    clearAllWatches() {
        console.log("stopping geo watching...");
        for(const id of this.watchIds)
            navigator.geolocation.clearWatch(id);
    }

    updateHandler(pos: Position) {
        console.log("position update", pos);
        this.sendToPort({
            msgType: "Update",
            coords: pos.coords,
            timestamp: <number>pos.timestamp,
            errorCode: null
        });
    }

    errorHandler(err: PositionError) {
        console.log("position error", err);
        this.sendToPort({
            msgType: "Error",
            errorCode: err.code,
            timestamp: null,
            coords: null
        });
    }
}
