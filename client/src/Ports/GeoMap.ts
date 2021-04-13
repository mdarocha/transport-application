import "@webcomponents/webcomponentsjs/custom-elements-es5-adapter";
import "@webcomponents/webcomponentsjs/webcomponents-loader";

import * as mapboxgl from "mapbox-gl";

const mapboxToken = window.MAPBOX_TOKEN;

interface MapPosition {
    longitude: number;
    latitude: number;
    zoom: number;
    rotation: number;
    pitch: number;
};

interface MapMarker {
    longitude: number;
    latitude: number;
};

const safeGetNumericAttribute = (el: HTMLElement, name: string, defaultValue: number = 0): number => {
    if(!el.hasAttribute(name))
        return defaultValue;

    const value = el.getAttribute(name);

    if(value == null)
        return defaultValue;

    const parsedValue = parseFloat(value);

    if(parsedValue == NaN)
        return defaultValue;

    return parsedValue;
};


class GeoMarker extends HTMLElement {
    private longitude: number;
    private latitude: number;
    private mapboxMarker: mapboxgl.Marker;

    constructor() {
        super();
    }

    connectedCallback() {
        this.longitude = safeGetNumericAttribute(this, 'lng');
        this.latitude = safeGetNumericAttribute(this, 'lat');
    }

    static get observedAttributes() { return ['lng', 'lat' ]; }

    setMapboxMarker(marker: mapboxgl.Marker) {
        this.mapboxMarker = marker;
        this.mapboxMarker.setLngLat([ this.longitude, this.latitude ]);
    }

    updateMapboxMarker() {
        if(this.mapboxMarker) {
            this.mapboxMarker.setLngLat([ this.longitude, this.latitude ]);
        }
    }

    attributeChangedCallback(name, oldValue, newValue) {
        switch(name) {
            case 'lng': {
                this.longitude = safeGetNumericAttribute(this, 'lng');
                this.updateMapboxMarker();
            }
            case 'lat': {
                this.latitude = safeGetNumericAttribute(this, 'lat');
                this.updateMapboxMarker();
            }
        }
    }

    disconnectedCallback() {
        if(this.mapboxMarker) {
            this.mapboxMarker.remove();
        }
    }

    get marker(): MapMarker {
        return { longitude: this.longitude, latitude: this.latitude };
    }
}

class GeoMap extends HTMLElement {
    private map: mapboxgl.Map;
    private mapHolder: HTMLElement;
    private mapPosition: MapPosition;
    private mapPositionUpdater: NodeJS.Timeout;

    private observer: MutationObserver;

    addMarker(m: GeoMarker) {
        const marker = new mapboxgl.Marker();
        const { longitude, latitude } = m.marker;
        marker.setLngLat([ longitude, latitude ]);
        marker.addTo(this.map);
        m.setMapboxMarker(marker);
    }

    constructor() {
        super();

        this.observer = new MutationObserver(muts => {
            muts.forEach(mut => {
                mut.addedNodes.forEach((m: GeoMarker) => { this.addMarker(m); });
            });
        });

        let root = this.attachShadow({ mode: 'closed' });

        const [style, wrapper, map] = this.initHtml();
        this.mapHolder = map;

        root.append(style, wrapper);
    }

    connectedCallback() {
        this.observer.observe(this, { childList: true });

        this.initMap();
        this.mapPositionUpdater = setInterval(() => {
            if(!this.map)
                return;

            if(!this.mapPosition)
                return;

            if(this.map.isMoving())
                return;

            const isPositionUpdated = () => {
                const rotMatches = this.mapPosition.rotation === this.map.getBearing();
                const zoomMatches = this.mapPosition.zoom === this.map.getZoom();

                const { lng, lat } = this.map.getCenter();
                const positionMatches = lng === this.mapPosition.longitude && lat === this.mapPosition.latitude;
                const pitchMatches = this.mapPosition.pitch === this.map.getPitch();

                return rotMatches && zoomMatches && positionMatches && pitchMatches;
            };

            if(isPositionUpdated())
                return;

            this.map.jumpTo({
                bearing: this.mapPosition.rotation,
                zoom: this.mapPosition.zoom,
                center: [ this.mapPosition.longitude, this.mapPosition.latitude ],
                pitch: this.mapPosition.pitch
            });
        }, 100);
    }

    disconnectedCallback() {
        this.map.remove();
        clearInterval(this.mapPositionUpdater);
    }

    initHtml() {
        const style = document.createElement('style');
        style.textContent = require("mapbox-gl/dist/mapbox-gl.css");

        const wrapper = document.createElement('div');
        wrapper.setAttribute('style', 'width: 100%; display: flex; align-items: stretch;');

        const inner = document.createElement('div');
        inner.setAttribute('style', 'width: 100%');

        wrapper.appendChild(inner);

        return [ style, wrapper, inner ];
    }

    initMap() {
        this.initMapPosition();

        this.map = new mapboxgl.Map({
            container: this.mapHolder,
            accessToken: mapboxToken,
            style: 'mapbox://styles/mapbox/streets-v11',
            center: [this.mapPosition.longitude, this.mapPosition.latitude],
            zoom: this.mapPosition.zoom,
            bearing: this.mapPosition.rotation,
            pitch: this.mapPosition.pitch
        });

        this.map.addControl(new mapboxgl.NavigationControl());

        setTimeout(() => this.map.resize(), 100); //fixes map size to fill container

        this.map.on('move', (_) => {
            const { lng, lat } = this.map.getCenter();
            this.mapPosition.longitude = lng;
            this.mapPosition.latitude = lat;
            this.mapPosition.rotation = this.map.getBearing();
            this.mapPosition.zoom = this.map.getZoom();
            this.mapPosition.pitch = this.map.getPitch();

            this.dispatchMapPositionUpdateEvent();
        });

        this.querySelectorAll('geo-marker').forEach((m: GeoMarker) => {
            this.addMarker(m);
        });
    }

    dispatchMapPositionUpdateEvent() {
        this.dispatchEvent(new CustomEvent('map-position-change', { detail: this.mapPosition }));
    }

    initMapPosition() {
        this.mapPosition = {
            longitude: safeGetNumericAttribute(this, 'lng'),
            latitude: safeGetNumericAttribute(this, 'lat'),
            zoom: safeGetNumericAttribute(this, 'zoom', 3),
            rotation: safeGetNumericAttribute(this, 'rotate'),
            pitch: safeGetNumericAttribute(this, 'pitch')
        };
    }

    static get observedAttributes() { return ['lng', 'lat', 'zoom', 'rotate', 'pitch']; }

    attributeChangedCallback(name, _oldValue, _newValue) {
        if(!this.mapPosition)
            return;

        switch(name) {
            case 'lng': {
                this.mapPosition.longitude = safeGetNumericAttribute(this, 'lng');
                break;
            }
            case 'lat': {
                this.mapPosition.latitude = safeGetNumericAttribute(this, 'lat');
                break;
            }
            case 'zoom': {
                this.mapPosition.zoom = safeGetNumericAttribute(this, 'zoom', 3);
                break;
            }
            case 'rotate': {
                this.mapPosition.rotation = safeGetNumericAttribute(this, 'rotate');
                break;
            }
            case 'pitch': {
                this.mapPosition.pitch = safeGetNumericAttribute(this, 'pitch');
                break;
            }
        }
    }
}

export function defineGeoMap() {
    customElements.define('geo-marker', GeoMarker);
    customElements.define('geo-map', GeoMap);

    return Promise.all([
        customElements.whenDefined('geo-marker'),
        customElements.whenDefined('geo-map')
    ]);
}
