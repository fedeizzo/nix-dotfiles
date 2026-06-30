package config

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLoad(t *testing.T) {
	t.Run("returns defaults when no config file and no env vars are provided", func(t *testing.T) {
		os.Clearenv()

		tmpFile := filepath.Join(t.TempDir(), "config.yaml")
		os.WriteFile(tmpFile, []byte{}, 0644)

		cfg, err := Load(tmpFile)
		require.NoError(t, err)

		assert.Equal(t, "qwen27", cfg.Models.Name)
		assert.Equal(t, "placeholder", cfg.Models.OpenAIKey)
		assert.Equal(t, "https://llama.fedeizzo.dev/v1", cfg.Models.OpenAIBase)
		assert.Equal(t, "pan.log", cfg.Log.Path)
		assert.Equal(t, "info", cfg.Log.Level)
		assert.Equal(t, "8080", cfg.Telemetry.Port)
	})

	t.Run("overrides defaults with environment variables", func(t *testing.T) {
		os.Clearenv()
		t.Setenv("MODEL_NAME", "gpt-4")
		t.Setenv("TELEMETRY_PORT", "9090")
		t.Setenv("LOG_LEVEL", "debug")

		tmpFile := filepath.Join(t.TempDir(), "config.yaml")
		os.WriteFile(tmpFile, []byte{}, 0644)

		cfg, err := Load(tmpFile)
		require.NoError(t, err)

		assert.Equal(t, "gpt-4", cfg.Models.Name)
		assert.Equal(t, "9090", cfg.Telemetry.Port)
		assert.Equal(t, "debug", cfg.Log.Level)
	})

	t.Run("reads values from api files and trims whitespace", func(t *testing.T) {
		os.Clearenv()

		tmpDir := t.TempDir()
		apiFilePath := filepath.Join(tmpDir, "fastmail_api.txt")
		err := os.WriteFile(apiFilePath, []byte("  super-secret-key  \n"), 0644)
		require.NoError(t, err)

		t.Setenv("FASTMAIL_API_FILE", apiFilePath)

		tmpFile := filepath.Join(t.TempDir(), "config.yaml")
		os.WriteFile(tmpFile, []byte{}, 0644)

		cfg, err := Load(tmpFile)
		require.NoError(t, err)

		assert.Equal(t, "super-secret-key", cfg.Fastmail.API)
	})

	t.Run("returns error when api file is specified but cannot be read", func(t *testing.T) {
		os.Clearenv()

		t.Setenv("FASTMAIL_API_FILE", "/path/to/nowhere/that/does/not/exist.txt")

		tmpFile := filepath.Join(t.TempDir(), "config.yaml")
		os.WriteFile(tmpFile, []byte{}, 0644)

		cfg, err := Load(tmpFile)
		require.Error(t, err)
		require.Nil(t, cfg)
		assert.Contains(t, err.Error(), "failed to read fastmail api file")
	})
}
