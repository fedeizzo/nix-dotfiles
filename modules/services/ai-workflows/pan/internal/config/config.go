package config

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/samber/oops"
	"github.com/spf13/viper"
)

type Config struct {
	Models     ModelsConfig     `mapstructure:"models"`
	Fastmail   FastmailConfig   `mapstructure:"fastmail"`
	Interface  InterfaceConfig  `mapstructure:"interface"`
	CLI        CLIConfig        `mapstructure:"cli"`
	Matrix     MatrixConfig     `mapstructure:"matrix"`
	Jobs       []JobConfig      `mapstructure:"jobs"`
	Log        LogConfig        `mapstructure:"log"`
	Hindsight  HindsightConfig  `mapstructure:"hindsight"`
	Telemetry  TelemetryConfig  `mapstructure:"telemetry"`
	LunchMoney LunchMoneyConfig `mapstructure:"lunchmoney"`
	Fusion     FusionConfig     `mapstructure:"fusion"`
}

type CLIConfig struct {
	ConversationPath string `mapstructure:"conversation_path"`
}

type TelemetryConfig struct {
	Port string `mapstructure:"port"`
}

type LogConfig struct {
	Path  string `mapstructure:"path"`
	Level string `mapstructure:"level"`
}

type HindsightConfig struct {
	URL    string `mapstructure:"url"`
	APIKey string `mapstructure:"api_key"`
	BankID string `mapstructure:"bank_id"`
}

type JobConfig struct {
	Name      string `mapstructure:"name"`
	Spec      string `mapstructure:"spec"`
	Condition string `mapstructure:"condition"`
	Prompt    string `mapstructure:"prompt"`
	Runner    string `mapstructure:"runner"`
}

type ModelsConfig struct {
	Name       string `mapstructure:"name"`
	OpenAIKey  string `mapstructure:"openai_api_key"`
	OpenAIBase string `mapstructure:"openai_base_url"`
}

type FastmailConfig struct {
	APIFile string `mapstructure:"api_file"`
	APICmd  string `mapstructure:"api_cmd"`
	API     string `mapstructure:"-"`
}

type LunchMoneyConfig struct {
	APIFile string `mapstructure:"api_file"`
	APICmd  string `mapstructure:"api_cmd"`
	API     string `mapstructure:"-"`
}

type FusionConfig struct {
	Endpoint     string `mapstructure:"endpoint"`
	PasswordFile string `mapstructure:"password_file"`
	PasswordCmd  string `mapstructure:"password_cmd"`
	Password     string `mapstructure:"-"`
}

type InterfaceConfig struct {
	Type string `mapstructure:"type"`
}

type MatrixConfig struct {
	Homeserver       string        `mapstructure:"homeserver"`
	User             string        `mapstructure:"user"`
	PasswordFile     string        `mapstructure:"password_file"`
	PasswordCmd      string        `mapstructure:"password_cmd"`
	Password         string        `mapstructure:"-"`
	AllowedUser      string        `mapstructure:"allowed_user"`
	AllowedRoom      string        `mapstructure:"allowed_room"`
	DataDir          string        `mapstructure:"data_dir"`
	NotificationRoom string        `mapstructure:"notification_room"`
	MessageRetention time.Duration `mapstructure:"message_retention"`
}

func resolveSecret(file, cmd string) (string, error) {
	if file != "" && cmd != "" {
		return "", fmt.Errorf("both file and cmd provided, they are mutually exclusive")
	}
	if file != "" {
		b, err := os.ReadFile(file)
		if err != nil {
			return "", fmt.Errorf("failed to read file at %s: %w", file, err)
		}
		return strings.TrimSpace(string(b)), nil
	}
	if cmd != "" {
		out, err := exec.Command("/bin/sh", "-c", cmd).Output()
		if err != nil {
			return "", fmt.Errorf("failed to execute cmd %q: %w", cmd, err)
		}
		return strings.TrimSpace(string(out)), nil
	}
	return "", nil
}

func Load(configPath string) (*Config, error) {
	v := viper.New()
	v.SetDefault("models.name", "qwen27")
	v.SetDefault("models.openai_api_key", "placeholder")
	v.SetDefault("models.openai_base_url", "https://llama.fedeizzo.dev/v1")
	v.SetDefault("log.path", "pan.log")
	v.SetDefault("log.level", "info")
	v.SetDefault("telemetry.port", "8080")

	// Set up config file reading
	if configPath != "" {
		v.SetConfigFile(configPath)
	} else {
		v.SetConfigName("config")
		v.SetConfigType("yaml")
		v.AddConfigPath(".")
	}

	// Support environment variables
	v.AutomaticEnv()

	// Map explicit viper keys to environment variables
	envBindings := map[string]string{
		"fastmail.api_file":        "FASTMAIL_API_FILE",
		"models.name":              "MODEL_NAME",
		"models.openai_api_key":    "OPENAI_API_KEY",
		"models.openai_base_url":   "OPENAI_BASE_URL",
		"interface.type":           "INTERFACE",
		"matrix.homeserver":        "MATRIX_HOMESERVER",
		"matrix.user":              "MATRIX_USER",
		"matrix.password_file":     "MATRIX_PASSWORD_FILE",
		"matrix.allowed_user":      "MATRIX_ALLOWED_USER",
		"matrix.allowed_room":      "MATRIX_ALLOWED_ROOM",
		"matrix.data_dir":          "MATRIX_DATA_DIR",
		"matrix.notification_room": "MATRIX_NOTIFICATION_ROOM",
		"matrix.message_retention": "MATRIX_MESSAGE_RETENTION",
		"log.path":                 "LOG_PATH",
		"log.level":                "LOG_LEVEL",
		"hindsight.url":            "HINDSIGHT_URL",
		"hindsight.api_key":        "HINDSIGHT_API_KEY",
		"hindsight.bank_id":        "HINDSIGHT_BANK_ID",
		"telemetry.port":           "TELEMETRY_PORT",
		"lunchmoney.api_file":      "LUNCHMONEY_API_FILE",
		"lunchmoney.api_cmd":       "LUNCHMONEY_API_CMD",
		"fusion.endpoint":          "FUSION_ENDPOINT",
		"fusion.password_file":     "FUSION_PASSWORD_FILE",
		"fusion.password_cmd":      "FUSION_PASSWORD_CMD",
		"fastmail.api_cmd":         "FASTMAIL_API_CMD",
		"matrix.password_cmd":      "MATRIX_PASSWORD_CMD",
	}

	for key, env := range envBindings {
		if err := v.BindEnv(key, env); err != nil {
			return nil, oops.In("config").Wrapf(err, "error binding env var %s", env)
		}
	}

	// Read config file if it exists
	if err := v.ReadInConfig(); err != nil {
		if _, ok := err.(viper.ConfigFileNotFoundError); !ok {
			return nil, oops.In("config").Wrapf(err, "error reading config file")
		}
	}

	var cfg Config
	if err := v.Unmarshal(&cfg); err != nil {
		return nil, oops.In("config").Wrapf(err, "unable to decode into config struct")
	}

	if val, err := resolveSecret(cfg.Fastmail.APIFile, cfg.Fastmail.APICmd); err != nil {
		return nil, oops.In("config").Wrapf(err, "fastmail secret error")
	} else if val != "" {
		cfg.Fastmail.API = val
	}

	if val, err := resolveSecret(cfg.Matrix.PasswordFile, cfg.Matrix.PasswordCmd); err != nil {
		return nil, oops.In("config").Wrapf(err, "matrix secret error")
	} else if val != "" {
		cfg.Matrix.Password = val
	}

	if val, err := resolveSecret(cfg.LunchMoney.APIFile, cfg.LunchMoney.APICmd); err != nil {
		return nil, oops.In("config").Wrapf(err, "lunchmoney secret error")
	} else if val != "" {
		cfg.LunchMoney.API = val
	}

	if val, err := resolveSecret(cfg.Fusion.PasswordFile, cfg.Fusion.PasswordCmd); err != nil {
		return nil, oops.In("config").Wrapf(err, "fusion secret error")
	} else if val != "" {
		cfg.Fusion.Password = val
	}

	return &cfg, nil
}
