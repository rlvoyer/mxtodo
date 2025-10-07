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
    sed -i '' -e "s/^;; Version: .*$/;; Version: $release_version/" $HERE/../mxtodo.el
    sed -i '' -e "s/^version = \".*\"$/version = \"$release_version\"/" $HERE/../mxtodo-searcher/Cargo.toml
}

rollback_version_update() {
    git checkout HEAD -- $HERE/../mxtodo.el $HERE/../mxtodo-searcher/Cargo.toml
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

build_searcher_artifacts() {
    pushd $HERE/.. > /dev/null || return 1
    rm -rf release-artifacts || return 1
    mkdir release-artifacts || return 1
    pushd mxtodo-searcher > /dev/null || return 1
    cross build --verbose --release --target=x86_64-unknown-linux-gnu || return 1
    cargo build --verbose --release --target=x86_64-apple-darwin || return 1
    cargo build --verbose --release --target=aarch64-apple-darwin || return 1
    cp target/x86_64-unknown-linux-gnu/release/libmxtodo_searcher.so ../release-artifacts/libmxtodo_searcher.x86_64-unknown-linux-gnu.so || return 1
    cp target/x86_64-apple-darwin/release/libmxtodo_searcher.dylib ../release-artifacts/libmxtodo_searcher.x86_64-apple-darwin.dylib || return 1
    cp target/aarch64-apple-darwin/release/libmxtodo_searcher.dylib ../release-artifacts/libmxtodo_searcher.aarch64-apple-darwin.dylib || return 1
    popd > /dev/null && popd > /dev/null || return 1

    return 0
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

CURRENT_VERSION=$(current_version)
RELEASE_VERSION="${VERSION:-$(increment_patch_version $CURRENT_VERSION)}"

while true; do
    read -p "Updating from $CURRENT_VERSION to $RELEASE_VERSION. Is this correct? " yn
    case $yn in
        [Yy]* ) break;;
        [Nn]* ) echo "Please run again and specify the correct release version."; exit;;
        * ) echo "Please answer yes or no. ";;
    esac
done

update_package_version $RELEASE_VERSION

set +e

build_searcher_artifacts

if [[ $? -ne 0 ]]; then
    echo "An error occurred while building the search artifacts...\n"
    rollback_version_update
    exit 1
fi

set -e

capture_unreleased_changes_from_changelog $RELEASE_VERSION > CHANGELOG.md.tmp && mv CHANGELOG.md.tmp $HERE/../CHANGELOG.md

git add $HERE/../mxtodo.el $HERE/../CHANGELOG.md $HERE/../mxtodo-searcher/Cargo.toml && \
    git commit -m "Updated version and CHANGELOG for release $RELEASE_VERSION."
git push

TAG="v$RELEASE_VERSION"

gh release create $TAG -F CHANGELOG.md

for asset in `ls release-artifacts`;
do
    gh release upload --clobber $TAG $HERE/../release-artifacts/$asset
done
