## Deploy html to github pages branch
## Usage: bash ./build/deploy.sh ./src-html

SRC_ROOT="$1"
DEST_ROOT=$(cd -P $(dirname "$0") && cd .. && pwd) #project-root
DEPLOY_BRANCH="gh-pages"

if [ ! -d "$SRC_ROOT" ]; then
		echo "Invalid source directory, aborting." >&2
		exit 1
fi

if [ ! -d "$DEST_ROOT/.git" ]; then
		echo "Destination '$DEST_ROOT' is not a git repo, aborting."
		exit 1
fi
