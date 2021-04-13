export {};

declare global {
    interface Window {
        WS_URL: string;
        API_URL: string;
        MAPBOX_TOKEN: string;
    }
}
