#!/usr/bin/env bash

function priv_clippit
(
    cat <<EOF
Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
EOF
)

function priv_pkgsearch
(
    if ((${#})); then
        for REPLY in "${@}"; do
            lazbuild --verbose-pkgsearch "${REPLY}" || lazbuild --add-package "${REPLY}"
        done
    fi
)

function priv_packages
(
    if [[ -d 'use' ]]; then
        git submodule update --init --recursive
    else
        mkdir 'use'
    fi
    if ((${#})); then
        for REPLY in "${@}"; do
            declare -A VAR=(
                [url]="https://packages.lazarus-ide.org/${REPLY}.zip"
                [out]=$(mktemp)
                [dir]=${REPLY##/*}
            )
            wget --output-document "${VAR[out]}" "${VAR[url]}"
            unzip -o "${VAR[out]}" -d "use/${VAR[dir]}"
            rm --verbose "${VAR[out]}"
        done
    fi
    find 'use' -type 'f' -name '*.lpk' -exec lazbuild --add-package-link {} +
)

function priv_main
(
    set -euo pipefail
    if ! (which lazbuild); then
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus
            ;;
        esac
    fi
    if ((${#})); then
        case ${1} in
            build)
                priv_pkgsearch
                priv_packages
                find 'source' -type 'f' -name '*.lpi' \
                    -exec lazbuild --no-write-project --recursive --no-write-project {} 1>&2 +
                ;;
        esac
    else
        priv_clippit
    fi
)

priv_main "${@}" >/dev/null
