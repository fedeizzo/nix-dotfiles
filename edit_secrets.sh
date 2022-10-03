#!/usr/bin/env bash
helpMessage="\nusage $0: <command>\ncommand:\n\tk3sencrypt: encrypt kubernetes secret\n\tedit: edit secret\n"
[ -z $1 ] && echo -e $helpMessage && exit 1

colorPrint() {
  echo -e "$(tput setaf 6)$1$(tput sgr0)"
}

errorPrint() {
  echo -e "$(tput setaf 1)$1$(tput sgr0)"
}

if [ $(id -u) -ne 0 ]; then
  errorPrint "Please run as root"
  exit 1
fi

export SOPS_AGE_KEY_FILE=/var/lib/sops/keys.txt

encrypt_kubernetes_secrets () {
    colorPrint "Encrypting kubernetes secrets $1"
    sops --encrypt --encrypted-regex '^(data|stringData)$' $1 > $1.encoded
    rm $1
    mv $1.encoded $1
}

edit_secretes () {
    colorPrint "Edit secrtes on file $1"
    sops $1
}

echo -e "List of available secret files:"
index=1
files=$(find . -name '*secrets*.yaml')
for file in $files; do
    echo -e "\t${index}) ${file}"
    index=$((index + 1))
done
read -p "Select a number: " selectedIndex
selectedFile=$(echo $files | awk -F' ' "{print \$${selectedIndex}}")

case $1 in
    "k3sencrypt")
	encrypt_kubernetes_secrets $selectedFile
	shift
	;;
    "edit")
	edit_secretes $selectedFile
	shift
	;;
esac
