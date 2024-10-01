#!/usr/bin/env bash
helpMessage=""
[ -z $1 ] && echo -e $helpMessage && exit 1

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

    if [[ $1 == "./secrets/raspberry-secrets.yaml" ]]; then
        SOPS_AGE_KEY=$(bw get item 'sops-age-keys-homelab' | jq -r ."notes") sops $1
    elif [[ $1 == "./secrets/xps-9510-secrets.yaml" ]]; then
        SOPS_AGE_KEY=$(bw get item 'sops-age-keys-xps-9510' | jq -r ."notes") sops $1
    elif [[ $1 == "./secrets/x1-carbon-secrets.yaml" ]]; then
        SOPS_AGE_KEY=$(bw get item 'sops-age-keys-x1-carbon' | jq -r ."notes") sops $1
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
case $1 in
    "edit")
	edit_secrets $selectedFile
	shift
	;;
    "encrypt")
        [[ -z $2 ]] && echo "provide the key name: homelab or laptop"
	# encrypt $selectedFile
	shift
	;;
esac
