#!/usr/bin/env bash

set -euo pipefail

# Colors
colorPrint() {
  echo -e "$(tput setaf 6)$1$(tput sgr0)"
}

errorPrint() {
  echo -e "$(tput setaf 1)[ERROR] $1$(tput sgr0)" >&2
}

# Globals
export EDITOR=vim
selected_file=""

encrypt_file() {
    local file="$1"

    if [[ ! -f "$file" ]]; then
        errorPrint "File '$file' does not exist."
        exit 1
    fi

    local dir
    local base
    dir=$(dirname "$file")
    base=$(basename "$file")
    local ext="${base##*.}"
    base="${base%.*}"  # Strip extension

    # Default to dotenv format if no extension or unsupported format
    if [[ -z "$ext" || ! "$ext" =~ ^(yaml|yml|json|env)$ ]]; then
        ext="env"
    fi

    # Determine input/output type based on extension
    local input_type
    case "$ext" in
        yaml|yml) input_type="yaml" ;;
        json) input_type="json" ;;
        env) input_type="dotenv" ;;
        *) input_type="dotenv" ;;  # Default to dotenv if no extension or unsupported extension
    esac

    local output_file="$dir/$base.$ext"  # Output file will have the same extension

    colorPrint "Encrypting $file (input type: $input_type) → $output_file..."

    output_file="$output_file.enc"

    # Run SOPS encryption with the appropriate input/output types
    sops --input-type "$input_type" --output-type "$input_type" --encrypt "$file" > "$output_file"
    mv $output_file $file

    colorPrint "Encryption complete: $file"
}

# Determine key_id based on file name
get_key_id() {
    local file="$1"

    if [[ "$file" == *"homelab"* ]]; then
        echo "sops-age-keys-homelab"
    elif [[ "$file" == *"xps"* ]]; then
        echo "sops-age-keys-xps-9510"
    elif [[ "$file" == *"oven"* ]]; then
        echo "sops-age-keys-x1-nano"
    else
        errorPrint "Unable to determine SOPS key from file name: $file"
        exit 1
    fi
}

# Edit secrets with correct key selection logic
edit_secrets() {
    local file="$1"
    colorPrint "Editing secrets in file: $file"

    rbw help &> /dev/null || true
    local should_use_rbw=$?

    local key_id
    key_id=$(get_key_id "$file")

    if [[ $should_use_rbw -eq 0 ]]; then
        SOPS_AGE_KEY=$(rbw get "$key_id") sops "$file"
    else
        SOPS_AGE_KEY=$(bw get item "$key_id" | jq -r .notes) sops "$file"
    fi
}

# List secrets and allow user selection
select_secret_file() {
    colorPrint "List of available secret files:"
    mapfile -t files < <(fd 'secrets.*$' --type f --exclude scripts | sort)

    if [[ ${#files[@]} -eq 0 ]]; then
        errorPrint "No secret files found."
        exit 1
    fi

    for i in "${!files[@]}"; do
        echo -e "\t$((i+1))) ${files[$i]}"
    done

    read -p "Select a number: " selectedIndex
    if ! [[ "$selectedIndex" =~ ^[0-9]+$ ]] || ((selectedIndex < 1 || selectedIndex > ${#files[@]})); then
        errorPrint "Invalid selection."
        exit 1
    fi

    selected_file="${files[$((selectedIndex - 1))]}"
}

# Print usage/help
print_help() {
  echo "Usage: $0 [-e FILE] [-x [FILE]] [-l] [-h]"
  echo
  echo "Options:"
  echo "  -e FILE     Edit the given secrets file"
  echo "  -x [FILE]   Encrypt the given (unencrypted) secrets file, or select one interactively"
  echo "  -l          List and select a secrets file to edit"
  echo "  -h          Show this help message"
}

# Main logic with argument parsing
main() {
    if [[ $# -eq 0 ]]; then
        print_help
        exit 0
    fi

    while getopts ":e:x::lh" opt; do
        case $opt in
            e)
                edit_secrets "$OPTARG"
                ;;
            x)
                if [[ -z "${OPTARG:-}" ]]; then
                    select_secret_file
                    encrypt_file "$selected_file"
                else
                    encrypt_file "$OPTARG"
                fi
                ;;
            l)
                select_secret_file
                edit_secrets "$selected_file"
                ;;
            h)
                print_help
                ;;
            \?)
                errorPrint "Invalid option: -$OPTARG"
                print_help
                exit 1
                ;;
            :)
                if [[ "$OPTARG" == "x" ]]; then
                    select_secret_file
                    encrypt_file "$selected_file"
                else
                    errorPrint "Option -$OPTARG requires an argument."
                    print_help
                    exit 1
                fi
                ;;
        esac
    done
}

main "$@"
