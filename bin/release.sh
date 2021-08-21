#!/usr/bin/env sh

set -e

realpath() {
    [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}

HERE=$(dirname $(realpath "$0"))

usage() {
    cat >&2 <<EOS
USAGE:
        [-v|--version]: Version for this release. For example, "0.2.0".

EXAMPLES:
	$ ./bin/release.sh -v 0.2.0
	Creates a release tagged "v0.2.0" and updates the version in mxtodo.el to "0.2.0".

NOTES:
- The specified version should be a semver version and *should not* be prefixed with "v". 
- This script assumes that any added/removed functionality of note has been added to the "[UNRELEASED]" section of the CHANGELOG.
- This script requires a clean Git working tree.
EOS
}

current_version() {
    sed -n 's/^;; Version: \(.*\)/\1/p' $HERE/../mxtodo.el
}

increment_patch_version() {
    local current_version=$1
    echo $current_version | awk -F'[.]' '{
        major=$1;
        minor=$2;
        patch=$3;
        patch += 1;
        printf("%d.%d.%d\n", major, minor, patch);
    }'
}

update_package_version() {
    local release_version=$1
    sed -i '' -e "s/^;; Version: \(.*\)$/;; Version: $release_version/" $HERE/../mxtodo.el
}

capture_unreleased_changes_from_changelog() {
    local release_version=$1
    local release_date=$(date "+%Y-%m-%d")

    awk -v version="$release_version" -v date="$release_date" \
    '{
         print $0;
         if ($0 ~ /## \[Unreleased\]/) {
             printf("\n## [%s] - %s\n", version, date);
         }
     }' $HERE/../CHANGELOG.md
}

POSITIONAL=()
while [[ $# -gt 0 ]]; do
    key="$1"

    case $key in
        -h|--help)
            usage
            exit 1;;
        -v|--version)
            VERSION="$2"
            shift
            shift
            ;;
        *)                     # unknown option
            POSITIONAL+=("$1") # save it in an array for later
            shift
            ;;
    esac
done

if [ "$1" = "-h" -o "$1" = "--help" ]; then
    usage
fi

if [[ $(git diff --stat) != '' ]]; then
    echo "The git working tree is dirty.\n"
    usage
    exit 1
fi

DEFAULT_VERSION=$(current_version)
VERSION="${VARIABLE:-$DEFAULT_VERSION}"
RELEASE_VERSION=$(increment_patch_version $VERSION)

while true; do
    read -p "Updating from $VERSION to $RELEASE_VERSION. Is this correct? " yn
    case $yn in
        [Yy]* ) break;;
        [Nn]* ) echo "Please run again and specify the correct release version."; exit;;
        * ) echo "Please answer yes or no. ";;
    esac
done

update_package_version $RELEASE_VERSION
capture_unreleased_changes_from_changelog $RELEASE_VERSION > CHANGELOG.md.tmp && mv CHANGELOG.md.tmp $HERE/../CHANGELOG.md
git add $HERE/../mxtodo.el $HERE/../CHANGELOG.md && git commit -m "Updated version and CHANGELOG for release $RELEASE_VERSION."

TAG="v$RELEASE_VERSION"

gh release create $TAG -F CHANGELOG.md
