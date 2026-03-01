# Network Topology

This document describes the network topology of the homelab infrastructure and how all machines communicate.

## Overview

The network consists of three main segments:

```
┌─────────────┐          ┌───────────┐          ┌──────────┐
│   Internet  │─────────▶│  Router   │─────────▶│  oven    │
│             │   WAN1   │  (254)    │   eth1   │(laptop)  │
└─────────────┘          │           │          └──────────┘
                         │  eth1     │
                         │           │
                         │   wifi    │
                         │           │
                         ▼           ▼
                  ┌──────────┐  ┌───────────┐
                  │homelab   │  │  Home     │
                  │(65)      │  │  Network  │
                  │          │  │192.168.1.0/24
                  └──────────┘  └───────────┘
```

## Network Segments

### Home Network (192.168.1.0/24)

The primary home network accessible via ethernet and WiFi:

| Host     | IP Address     | Role                    |
|----------|----------------|-------------------------|
| Router   | 192.168.1.254  | Gateway, DHCP server    |
| homelab  | 192.168.1.65   | Framework Desktop server|
| oven     | 192.168.1.67   | ThinkPad X1 Nano laptop |

### WireGuard VPN (192.168.7.0/24)

A private WireGuard network for secure remote access:

| Host    | IP Address     | Role                    |
|---------|----------------|-------------------------|
| Router  | 192.168.7.1    | WireGuard endpoint      |
| homelab | 192.168.7.2    | Accessible via WG only  |

## Services and Ports

### External Access (via Router → homelab:443)

Services exposed to the internet through `fedeizzo.dev`:

| Service       | Subdomain        | Port  | Authentication |
|---------------|------------------|-------|----------------|
| Authentik     | auth.fedeizzo.dev| 9000  | Yes            |
| Fedeizzo.dev  | fedeizzodev.fedeizzo.dev | 50001 | Yes       |
| Jellyfin      | jellyfin.fedeizzo.dev | 8096 | Yes            |
| Jellyseerr    | jellyseerr.fedeizzo.dev | 5055 | Yes            |
| Nextcloud     | nextcloud.fedeizzo.dev  | 8180 | Yes            |
| Collabora     | collabora.fedeizzo.dev  | 9980 | Yes            |
| Affine        | affine.fedeizzo.dev     | 3010 | Yes            |

### Internal Access (via WireGuard)

Services accessible only via WireGuard VPN:

| Service       | Subdomain           | Port  |
|---------------|---------------------|-------|
| Dashboard     | homlab.fedeizzo.dev | 8080  |
| Home Assistant| hass.fedeizzo.dev   | 8123  |
| Ollama        | llm.fedeizzo.dev    | 11434 |
| Open WebUI    | openwebui.fedeizzo.dev | 8080 |

## Reverse Proxy (Traefik)

Traefik is configured with two entrypoints:

- **websecure** (192.168.1.65:443): External HTTPS traffic
- **wgsecure** (192.168.7.1:443): WireGuard HTTPS traffic

Let's Encrypt certificates are obtained via Cloudflare DNS challenge.

## Firewall

The homelab firewall is configured to:

1. Only accept connections on necessary ports
2. Restrict websecure to the ethernet interface (192.168.1.65)
3. Restrict wgsecure to the WireGuard interface (192.168.7.1)
4. Log all connection attempts