package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/cloudflare/cloudflare-go"
)

func main() {
	cfg := struct {
		APITokenPath       string
		ZoneIDPath         string
		ExposedSubdomains  string
		InternalSubdomains string
		InternalIP         string
		Domain             string
	}{
		InternalIP: "192.168.7.1",
		Domain:     "fedeizzo.dev",
	}

	flag.StringVar(&cfg.APITokenPath, "token-path", "", "Path to CF API Token")
	flag.StringVar(&cfg.ZoneIDPath, "zone-id-path", "", "Path to Zone ID")
	flag.StringVar(&cfg.ExposedSubdomains, "exposed", "", "Space-separated exposed subdomains")
	flag.StringVar(&cfg.InternalSubdomains, "internal", "", "Space-separated internal subdomains")
	flag.Parse()

	apiToken := readSecret(cfg.APITokenPath)
	zoneID := readSecret(cfg.ZoneIDPath)

	// 1. Force IPv4 for Cloudflare API calls (as requested before)
	ipv4Client := &http.Client{
		Transport: &http.Transport{
			DialContext: func(ctx context.Context, network, addr string) (net.Conn, error) {
				return (&net.Dialer{Timeout: 10 * time.Second}).DialContext(ctx, "tcp4", addr)
			},
		},
	}

	api, err := cloudflare.NewWithAPIToken(apiToken, cloudflare.HTTPClient(ipv4Client))
	if err != nil {
		log.Fatalf("Error: %v", err)
	}

	publicIP, err := getPublicIP(ipv4Client)
	if err != nil {
		log.Fatalf("Error getting public IP: %v", err)
	}

	ctx := context.Background()
	rc := cloudflare.ZoneIdentifier(zoneID)

	// 2. Update Public Records
	for _, sub := range strings.Fields(cfg.ExposedSubdomains) {
		fqdn := fmt.Sprintf("%s.%s", sub, cfg.Domain)
		if shouldUpdate(fqdn, publicIP) {
			updateCloudflare(ctx, api, rc, sub, publicIP)
		} else {
			log.Printf("[%s] DNS already matches %s, skipping.", fqdn, publicIP)
		}
	}

	// 3. Update Internal Records
	for _, sub := range strings.Fields(cfg.InternalSubdomains) {
		fqdn := fmt.Sprintf("%s.%s", sub, cfg.Domain)
		if shouldUpdate(fqdn, cfg.InternalIP) {
			updateCloudflare(ctx, api, rc, sub, cfg.InternalIP)
		} else {
			log.Printf("[%s] DNS already matches %s, skipping.", fqdn, cfg.InternalIP)
		}
	}
}

// shouldUpdate performs a fresh DNS lookup to see if the record matches the target
func shouldUpdate(fqdn string, targetIP string) bool {
	// We use a custom resolver to bypass local system caching if possible
	resolver := &net.Resolver{
		PreferGo: true,
		Dial: func(ctx context.Context, network, address string) (net.Conn, error) {
			d := net.Dialer{Timeout: 2 * time.Second}
			return d.DialContext(ctx, "udp", "1.1.1.1:53") // Query Cloudflare's DNS directly
		},
	}

	ips, err := resolver.LookupHost(context.Background(), fqdn)
	if err != nil {
		// If the record doesn't exist (NXDOMAIN), we definitely should update/create it
		return true
	}

	for _, ip := range ips {
		if ip == targetIP {
			return false // Match found, no update needed
		}
	}
	return true
}

func updateCloudflare(ctx context.Context, api *cloudflare.API, rc *cloudflare.ResourceContainer, name string, content string) {
	// We still need to find the Record ID to perform a PUT/PATCH
	records, _, err := api.ListDNSRecords(ctx, rc, cloudflare.ListDNSRecordsParams{
		Type: "A",
		Name: name,
	})

	if err != nil {
		log.Printf("Error listing records for %s: %v", name, err)
		return
	}

	if len(records) > 0 {
		log.Printf("[%s] Updating to %s", name, content)
		_, err = api.UpdateDNSRecord(ctx, rc, cloudflare.UpdateDNSRecordParams{
			ID:      records[0].ID,
			Type:    "A",
			Name:    name,
			Content: content,
			TTL:     120,
			Proxied: cloudflare.BoolPtr(false),
		})
	} else {
		log.Printf("[%s] Creating with %s", name, content)
		_, err = api.CreateDNSRecord(ctx, rc, cloudflare.CreateDNSRecordParams{
			Type:    "A",
			Name:    name,
			Content: content,
			TTL:     120,
			Proxied: cloudflare.BoolPtr(false),
		})
	}

	if err != nil {
		log.Printf("Failed to apply change for %s: %v", name, err)
	}
}

func getPublicIP(client *http.Client) (string, error) {
	resp, err := client.Get("https://api.ipify.org")
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	ip, _ := io.ReadAll(resp.Body)
	return string(ip), nil
}

func readSecret(path string) string {
	data, _ := os.ReadFile(path)
	return strings.TrimSpace(string(data))
}
