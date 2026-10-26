{
  flake.modules.nixos.forge = {
    # Unlocked by the TPM once enrolled (`systemd-cryptenroll --tpm2-device=auto --tpm2-pcrs=7`);
    # otherwise the initrd asks for the passphrase on the console (PiKVM).
    boot.initrd.luks.devices.cryptroot.crypttabExtraOpts = [ "tpm2-device=auto" ];

    # Only read by disko when formatting; nixos-anywhere copies it in with --disk-encryption-keys.
    disko.devices.disk.main.content.partitions.luks.content.passwordFile = "/tmp/forge-luks.key";
  };
}
