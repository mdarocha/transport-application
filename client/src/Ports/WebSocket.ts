import { IPort } from "./IPort";

export class WebSocketPort implements IPort {
    private socketUrl: string;
    private socket: WebSocket;
    private ports: any;

    constructor(socketUrl: string) {
        this.socketUrl = socketUrl;
    }

    bind(ports: any) {
        ports.open.subscribe(() => this.openWebSocket());
        ports.openAfterDelay.subscribe((delay : number) => {
            console.log(`opening websocket after ${delay} ms`);
            setTimeout(() => this.openWebSocket(), delay);
        });

        ports.sendMessage.subscribe((msg: object) => this.sendMessage(msg));

        this.ports = ports;

        window.addEventListener('offline', (_) => {
            ports.wentOffline.send(null);
            this.socket.close();
        });

        window.addEventListener('online', (_) => {
            ports.wentOnline.send(null);
        });

        this.openWebSocket();
    }

    private openWebSocket() {
        const state = this.socket?.readyState ?? WebSocket.CLOSED;
        if(state !== WebSocket.OPEN && state !== WebSocket.CLOSING && state !== WebSocket.CONNECTING
          && navigator.onLine) {

            console.log(`opening websocket to url ${this.socketUrl}`);
            this.socket = new WebSocket(this.socketUrl);

            this.socket.onopen = () => this.ports.opened.send(null);

            this.socket.onerror = (ev) => {
                console.log("socket closed cause of error", ev);
                this.ports.closed.send(null);
            };

            this.socket.onclose = () => {
                console.log("closing socket");
                this.ports.closed.send(null);
            }

            this.socket.addEventListener('message', (ev) => {
                console.log("received msg", ev);
                this.ports.receivedMessage.send(JSON.parse(ev.data));
            });
        } else {
            console.log("can't open websocket - bad state", this.socket?.readyState);
        }
    }

    private sendMessage(msg: object) {
        if(this.socket.readyState === WebSocket.OPEN) {
            this.socket.send(JSON.stringify(msg));
        } else {
            console.log("Can't send msg: socket not yet open", msg);
        }
    }
}
