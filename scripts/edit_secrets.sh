#!/usr/bin/env bash

colorPrint() {
  echo -e "$(tput setaf 6)$1$(tput sgr0)"
}

errorPrint() {
  echo -e "$(tput setaf 1)$1$(tput sgr0)"
}

export EDITOR=nano

encrypt () {
    sops --encrypt $1 > $1.encoded
    rm $1
    mv $1.encoded $1
}

edit_secrets () {
    colorPrint "Edit secrets on file $1"
    rbw help 2&> /dev/null
    SHOULD_USE_RBW=$?
    if [[ $1 == "./secrets/raspberry-secrets.yaml" ]]; then
        if [[ $SHOULD_USE_RBW == 2 ]]; then
            SOPS_AGE_KEY=$(rbw get 'sops-age-keys-homelab') sops $1
        else
            SOPS_AGE_KEY=$(bw get item 'sops-age-keys-homelab' | jq -r ."notes") sops $1
        fi
    elif [[ $1 == "./secrets/xps-9510-secrets.yaml" ]]; then
        if [[ $SHOULD_USE_RBW == 2 ]]; then
            SOPS_AGE_KEY=$(rbw get 'sops-age-keys-xps-9510') sops $1
        else
            SOPS_AGE_KEY=$(bw get item 'sops-age-keys-xps-9510' | jq -r ."notes") sops $1
        fi
    elif [[ $1 == "./secrets/x1-carbon-secrets.yaml" ]]; then
        if [[ $SHOULD_USE_RBW == 2 ]]; then
            SOPS_AGE_KEY=$(rbw get 'sops-age-keys-x1-carbon') sops $1
        else
            SOPS_AGE_KEY=$(bw get item 'sops-age-keys-x1-carbon' | jq -r ."notes") sops $1
        fi
    fi
}

colorPrint "List of available secret files:"

index=1
files=$(find . -name '*secrets*.yaml')
for file in $files; do
    echo -e "\t${index}) ${file}"
    index=$((index + 1))
done

read -p "Select a number: " selectedIndex
selectedFile=$(echo $files | awk -F' ' "{print \$${selectedIndex}}")

echo $selectedFile
edit_secrets $selectedFile
