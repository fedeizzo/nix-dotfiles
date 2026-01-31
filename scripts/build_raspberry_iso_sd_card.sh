#!/usr/bin/env bash

set -euo pipefail

IMAGE_DIR="result/sd-image"
IMAGE_ZST=""
IMAGE_IMG=""
EXTRACTED_IMG=""

cleanup() {
  echo
  echo "==> Cleaning up..."
  [[ -n "$EXTRACTED_IMG" && -f "$EXTRACTED_IMG" ]] && rm -f "$EXTRACTED_IMG"
  [[ -e result ]] && rm -rf result
}

trap cleanup EXIT

echo "==> Building Raspberry Pi SD image..."
nix build .#raspberrySDImage

echo
read -rp "Do you want to write the image to a disk? [y/N] " confirm
if [[ ! "$confirm" =~ ^[Yy]$ ]]; then
  echo "Aborted."
  exit 0
fi

echo
echo "==> Connected block devices:"
lsblk -o NAME,SIZE,TYPE,MOUNTPOINT,MODEL
echo

read -rp "Enter the target disk (e.g. /dev/sda): " TARGET_DISK

if [[ ! -b "$TARGET_DISK" ]]; then
  echo "Error: $TARGET_DISK is not a valid block device."
  exit 1
fi

IMAGE_ZST=$(ls "$IMAGE_DIR"/*.img.zst 2>/dev/null | head -n 1)
if [[ -z "$IMAGE_ZST" ]]; then
  echo "Error: No .img.zst file found in $IMAGE_DIR"
  exit 1
fi

IMAGE_BASENAME=$(basename "$IMAGE_ZST")
EXTRACTED_IMG="${IMAGE_BASENAME%.zst}"

echo
echo "==> Image found:"
echo "    $IMAGE_ZST"
echo "==> Extracting to:"
echo "    $PWD/$EXTRACTED_IMG"
echo "==> Target disk:"
echo "    $TARGET_DISK"
echo
read -rp "THIS WILL ERASE ALL DATA ON $TARGET_DISK. Continue? [y/N] " confirm_dd
if [[ ! "$confirm_dd" =~ ^[Yy]$ ]]; then
  echo "Aborted."
  exit 0
fi

echo
echo "==> Decompressing image to working directory..."
zstd -d -c "$IMAGE_ZST" > "$EXTRACTED_IMG"

echo
echo "==> Writing image to disk (this may take a while)..."
doas dd \
  if="$EXTRACTED_IMG" \
  of="$TARGET_DISK" \
  bs=64M \
  status=progress \
  conv=fsync \
  oflag=direct

echo
echo "==> Syncing..."
sync

echo
echo "==> Done! SD card written successfully."
