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

edit_secretes () {
    colorPrint "Edit secrtes on file $1"

    if [[ $1 == "./secrets/raspberry-secrets.yaml" ]]; then
        SOPS_AGE_KEY=$(bw get item 'sops-age-keys-homelab' | jq -r ."notes") sops $1
    elif [[ $1 == "./secrets/laptop-secrets.yaml" ]]; then
        SOPS_AGE_KEY=$(bw get item 'sops-age-keys-laptop' | jq -r ."notes") sops $1
    elif [[ $1 == "./secrets/ssh-public-keys-secrets.yaml" ]]; then
        SOPS_AGE_KEY=$(cat ~/.config/sops/age/keys.txt) sops $1
        # SOPS_AGE_KEY=$(bw get item 'sops-age-keys-laptop' | jq -r ."notes") sops $1
    elif [[ $1 == "./secrets.yaml" ]]; then
        SOPS_AGE_KEY=$(bw get item 'sops-age-keys-laptop' | jq -r ."notes") sops $1
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

case $1 in
    "edit")
	edit_secretes $selectedFile
	shift
	;;
    "encrypt")
        [[ -z $2 ]] && echo "provide the key name: homelab or laptop"
	# encrypt $selectedFile
	shift
	;;
esac
