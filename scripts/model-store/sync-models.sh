#!/usr/bin/env bash
# Union-sync the model store between this machine and a peer host, then
# regenerate the by-capability view on both. Run it after downloading a new
# model on either machine.
#
#   sync-models.sh [user@host]        # default: root@192.168.1.89
#
# Semantics: union merge, newest wins (-au). Safe for adding models; deletions
# are NOT propagated — delete a model by hand on both hosts, then re-run.
# Store layout contract: /persist/models/store/<Family-Size[-Variant]>/ with
# all quants of one model inside; by-capability/ is generated symlinks only.
set -euo pipefail

PEER="${1:-root@192.168.1.89}"
RS="rsync -au --partial --info=progress2"

# --- view generation (runs on both hosts; keep in sync with reality) --------
view_script() {
cat <<'VIEW'
set -euo pipefail
cd /persist/models
declare -A MEMBERS=(
  [language]="DeepSeek-V4-Flash Qwen3.8-27B Qwen3.8-27B-DFlash2 Qwen3.6-35B-A3B-MTP Qwen3.8-Flash-Next LFM3-1.7B"
  [vision]="Qwen3.8-27B Qwen3.6-35B-A3B-MTP Qwen3.8-Flash-Next LFM3-VL-2B D1-Omni-2B"
  [audio]="Qwen3-ASR-1.7B Qwen3-TTS-12Hz-1.7B-Base Qwen3-TTS-12Hz-1.7B-CustomVoice Qwen3-TTS-12Hz-1.7B-VoiceDesign LFM3-Audio-2B D1-Omni-2B Fish-Drama-3"
  [omni]="D1-Omni-2B"
  [embedding]="EmbeddingGemma-2"
)
for cap in "${!MEMBERS[@]}"; do
  mkdir -p "by-capability/$cap"
  find "by-capability/$cap" -maxdepth 1 -type l -delete
  for m in ${MEMBERS[$cap]}; do
    [ -d "store/$m" ] || { echo "WARN: store/$m missing, skipped" >&2; continue; }
    ln -sfn "../../store/$m" "by-capability/$cap/$m"
  done
done
VIEW
}

echo "==> pull union from $PEER"
$RS "$PEER:/persist/models/store/"   /persist/models/store/
$RS "$PEER:/persist/models/samples/" /persist/models/samples/

echo "==> push union to $PEER"
$RS /persist/models/store/   "$PEER:/persist/models/store/"
$RS /persist/models/samples/ "$PEER:/persist/models/samples/"

echo "==> regenerate by-capability views"
view_script | bash
view_script | ssh "$PEER" 'bash -s'

echo "done — both hosts hold the union."
