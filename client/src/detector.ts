export interface Features {
    WebGL: boolean;
    WebSocket: boolean;
    OnlineStatus: boolean;
    Language: string;
}

function detectWebGL(): boolean {
    const canvas = document.createElement("canvas");
    const gl = canvas.getContext("webgl") || canvas.getContext("experimental-webgl");
    return Boolean(gl && gl instanceof WebGLRenderingContext);
}

function detectWebSocket(): boolean {
    return 'WebSocket' in window || 'MozWebSocket' in window;
}

function detectOnline(): boolean {
    return navigator.onLine ?? false;
}

function getLanguage(): string {
    return navigator.language;
}

export function detect(): Features {
    return {
        WebGL: detectWebGL(),
        WebSocket: detectWebSocket(),
        OnlineStatus: detectOnline(),
        Language: getLanguage()
    };
}
