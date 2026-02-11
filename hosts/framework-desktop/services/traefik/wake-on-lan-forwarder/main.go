package main

import (
	"fmt"
	"log"
	"net"
	"net/http"
	"os"
	"strconv"
	"time"

	"github.com/mdlayher/wol"
)

const (
	port          = 35867
	wakeTimeoutSec = 30
	broadcastAddr  = "255.255.255.255:9" // standard WoL port
)

// Read environment variables with defaults
var (
	machineMAC  = getEnv("WOL_MAC", "AA:BB:CC:DD:EE:FF")
	machineIP   = getEnv("WOL_IP", "192.168.1.67")
	machinePort = getEnvAsInt("REMOTE_SERVICE_PORT", 9999)
)

// Helper: get env variable or fallback
func getEnv(key, fallback string) string {
	if val := os.Getenv(key); val != "" {
		return val
	}
	return fallback
}

// Helper: get env variable as int
func getEnvAsInt(key string, fallback int) int {
	if val := os.Getenv(key); val != "" {
		if i, err := strconv.Atoi(val); err == nil {
			return i
		}
	}
	return fallback
}

// Check if the LLM server is up (TCP port open)
func isLLMUp() bool {
	conn, err := net.DialTimeout("tcp", fmt.Sprintf("%s:%d", machineIP, machinePort), 1*time.Second)
	if err != nil {
		return false
	}
	conn.Close()
	return true
}

// Send Wake-on-LAN and wait until the LLM is ready
func wakeLLM() bool {
	if isLLMUp() {
		log.Println("LLM already up")
		return true
	}

	mac, err := net.ParseMAC(machineMAC)
	if err != nil {
		log.Printf("Invalid MAC address: %v", err)
		return false
	}

	client, err := wol.NewClient()
	if err != nil {
		log.Printf("Failed to create WoL client: %v", err)
		return false
	}
	defer client.Close()

	if err := client.Wake(broadcastAddr, mac); err != nil {
		log.Printf("Error sending WoL packet: %v", err)
		return false
	}
	log.Println("WoL packet sent, waiting for LLM to be ready...")

	start := time.Now()
	for time.Since(start) < wakeTimeoutSec*time.Second {
		if isLLMUp() {
			log.Println("LLM is ready")
			return true
		}
		time.Sleep(1 * time.Second)
	}

	log.Println("LLM did not wake in time")
	return false
}

func wakeHandler(w http.ResponseWriter, r *http.Request) {
	log.Printf("Received request from %s", r.RemoteAddr)
	if wakeLLM() {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("OK"))
	} else {
		w.WriteHeader(http.StatusServiceUnavailable)
		w.Write([]byte("LLM did not wake in time"))
	}
}

func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("/wake", wakeHandler)

	server := &http.Server{
		Addr:         fmt.Sprintf("127.0.0.1:%d", port), // localhost-only
		Handler:      mux,
		ReadTimeout:  5 * time.Second,
		WriteTimeout: 60 * time.Second, // allow for slow boot
	}

	log.Printf("Wake-on-LAN ForwardAuth service running on 127.0.0.1:%d", port)
	if err := server.ListenAndServe(); err != nil {
		log.Fatalf("Server failed: %v", err)
	}
}
