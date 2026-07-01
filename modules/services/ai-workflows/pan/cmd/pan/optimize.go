package main

import (
	"fmt"

	"github.com/spf13/cobra"
	"pan/internal/optimizer"
)

var optimizeCmd = &cobra.Command{
	Use:   "optimize [agent]",
	Short: "Optimize an agent's prompt using LLM meta-agent",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		agentName := args[0]
		if err := optimizer.Run(cmd.Context(), rootCfg, agentName); err != nil {
			return fmt.Errorf("optimization failed: %w", err)
		}
		return nil
	},
}

func init() {
	rootCmd.AddCommand(optimizeCmd)
}
