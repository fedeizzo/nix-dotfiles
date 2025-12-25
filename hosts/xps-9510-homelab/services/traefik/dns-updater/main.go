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
	"slices"
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

	// Update Public Records
	for sub := range strings.FieldsSeq(cfg.ExposedSubdomains) {
		fqdn := getFQDN(sub, cfg.Domain)
		if shouldUpdate(fqdn, publicIP) {
			updateCloudflare(ctx, api, rc, sub, publicIP)
		} else {
			log.Printf("[%s] DNS already matches %s, skipping.", fqdn, publicIP)
		}
	}

	// Update Internal Records
	for sub := range strings.FieldsSeq(cfg.InternalSubdomains) {
		fqdn := getFQDN(sub, cfg.Domain)
		if shouldUpdate(fqdn, cfg.InternalIP) {
			updateCloudflare(ctx, api, rc, sub, cfg.InternalIP)
		} else {
			log.Printf("[%s] DNS already matches %s, skipping.", fqdn, cfg.InternalIP)
		}
	}
}

// getFQDN ensures we don't end up with sub.domain.com.domain.com
func getFQDN(sub, domain string) string {
	if strings.HasSuffix(sub, domain) {
		return sub
	}
	return fmt.Sprintf("%s.%s", sub, domain)
}

func shouldUpdate(fqdn string, targetIP string) bool {
	r := &net.Resolver{
		PreferGo: true,
		Dial: func(ctx context.Context, network, address string) (net.Conn, error) {
			d := net.Dialer{Timeout: 2 * time.Second}
			return d.DialContext(ctx, "udp", "1.1.1.1:53")
		},
	}

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	ips, err := r.LookupIP(ctx, "ip4", fqdn)
	if err != nil {
		log.Printf("[%s] Lookup failed (record likely missing): %v", fqdn, err)
		return true
	}

	var foundIPs []string
	for _, ip := range ips {
		ipStr := ip.String()
		// Detect Cloudflare Edge IPs (effectively Proxied)
		// 188.114.96.0/20 and similar ranges indicate the proxy is active
		if strings.HasPrefix(ipStr, "188.114.") || strings.HasPrefix(ipStr, "104.") || strings.HasPrefix(ipStr, "172.67.") {
			log.Printf("[%s] DNS returned Cloudflare Proxy IP: %s. Forcing update check.", fqdn, ipStr)
			return true
		}
		foundIPs = append(foundIPs, ipStr)
	}

	log.Printf("[%s] Live DNS check returned: %s", fqdn, strings.Join(foundIPs, " "))
	return !slices.Contains(foundIPs, targetIP)
}

func updateCloudflare(ctx context.Context, api *cloudflare.API, rc *cloudflare.ResourceContainer, name string, content string) {
	// List records to find the ID.
	// Note: name here should be the subdomain part or FQDN depending on your CF setup.
	records, _, err := api.ListDNSRecords(ctx, rc, cloudflare.ListDNSRecordsParams{
		Type: "A",
		Name: name,
	})

	if err != nil {
		log.Printf("Error listing records for %s: %v", name, err)
		return
	}

	params := cloudflare.UpdateDNSRecordParams{
		Type:    "A",
		Name:    name,
		Content: content,
		TTL:     120,
		Proxied: cloudflare.BoolPtr(false),
	}

	if len(records) > 0 {
		// Verify if the content is already correct in the CF Dashboard to save an update call
		if records[0].Content == content {
			log.Printf("[%s] API reports content is already %s. Skipping update.", name, content)
			return
		}

		log.Printf("[%s] Updating to %s", name, content)
		params.ID = records[0].ID
		_, err = api.UpdateDNSRecord(ctx, rc, params)
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
