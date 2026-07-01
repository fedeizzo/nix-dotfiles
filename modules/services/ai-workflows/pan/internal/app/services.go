package app

import (
	"fmt"

	"git.sr.ht/~rockorager/go-jmap"
	lm "github.com/Cidan/lunchmoney-go"
	hindsightSDK "github.com/vectorize-io/hindsight/hindsight-clients/go"

	"pan/internal/config"
	"pan/internal/fastmail"
	"pan/internal/fusion"
	"pan/internal/memory/hindsight"
)

type Services struct {
	Fastmail   fastmail.Service
	LunchMoney *lm.Client
	Fusion     fusion.Service
	Memory     *hindsight.Service
}

func buildServices(cfg *config.Config) (*Services, error) {
	client := &jmap.Client{
		SessionEndpoint: "https://api.fastmail.com/jmap/session",
	}
	client.WithAccessToken(cfg.Fastmail.API)
	if err := client.Authenticate(); err != nil {
		return nil, fmt.Errorf("fastmail auth: %w", err)
	}
	fastmailService := fastmail.New(client)

	lmClient, err := lm.NewClient(lm.WithStaticToken(cfg.LunchMoney.API))
	if err != nil {
		return nil, fmt.Errorf("lunchmoney init: %w", err)
	}

	fusionSvc := fusion.New(cfg.Fusion.Endpoint, cfg.Fusion.Password)

	configuration := hindsightSDK.NewConfiguration()
	if cfg.Hindsight.URL != "" {
		configuration.Servers = hindsightSDK.ServerConfigurations{{URL: cfg.Hindsight.URL}}
	}
	configuration.AddDefaultHeader("Authorization", "Bearer "+cfg.Hindsight.APIKey)
	hsClient := hindsightSDK.NewAPIClient(configuration)
	m := hindsight.NewService(hsClient, cfg.Hindsight.BankID)

	return &Services{
		Fastmail:   fastmailService,
		LunchMoney: lmClient,
		Fusion:     fusionSvc,
		Memory:     m,
	}, nil
}
