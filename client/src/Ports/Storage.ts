import { IPort } from "./IPort";

export class StoragePort implements IPort {
    private browserStorage: Storage;

    constructor() {
        this.browserStorage = window.sessionStorage;
    }

    getToken(): string|null {
        return this.get("user");
    }

    bind(ports: any) {
        ports.saveToken.subscribe((token: string) => {
            this.save("user", token);
        });
    }

    private save(key: string, value: string) {
        this.browserStorage.setItem(key, value);
    }

    private get(key: string): string|null {
        return this.browserStorage.getItem(key);
    }
}
