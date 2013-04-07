## Usage: bash ./build/docgen indir outdir

PROJECT_ROOT=$(cd -P $(dirname "$0") && cd .. && pwd)
PAGE_TEMPLATE="$PROJECT_ROOT/build/templates/page.html"
NAVBAR_TEMPLATE="$PROJECT_ROOT/build/templates/navbar.html"
REF_ROOT="$PROJECT_ROOT/build/ref/data"

if [ ! -d "$1" ]; then
		echo "Invalid input directory, aborting." >&2
		exit 1
fi

DOC_ROOT=$(cd -P "$1" && pwd)
OUT_ROOT="$2"

if [ ! -d "$OUT_ROOT" ]; then mkdir -p "$OUT_ROOT"; fi

NAVBAR_HTML=$(cat "$NAVBAR_TEMPLATE")
REF_FILES=($(find "$REF_ROOT" -type f -name '*.md'))


function preprocess_file {
		local in_path="$1"
		local m4_init="m4_changequote(,)" #ignore all m4 quoting for markdown
		echo "$(cd $(dirname $in_path) && m4 --prefix-builtins <(echo $m4_init) $in_path)"
}

function format_title {
		local title=$(echo "$1" | tr '-' ' ')
		local arr=($title) #string to array
		echo "${arr[@]^}"  #capitalize first letter of each word
}

function convert_file {
		local in_path="$1" out_path="$2"
		local title=$(format_title $(basename "$in_path" '.md'))
		local page=$(preprocess_file "$in_path")
		local toc_html=$(echo "$page" | bash "$PROJECT_ROOT/build/page-toc.sh")
		
		## covert markdown to html using our template and external link references
		if pandoc -f markdown -t html5 --smart --standalone \
				--template "$PAGE_TEMPLATE" --output "$out_path" \
				-V title="$title" -V navbar="$NAVBAR_HTML" -V toc="$toc_html" \
				<(echo "$page") "${REF_FILES[@]}";
		then
				echo " → $out_path"
		else
				echo "Pandoc conversion error: '$in_path'"
				exit 1
		fi
}

function convert_dir {
		local in_path="$1" out_path="$2"
		while read filepath; do
				#remove api_root and beginning slash
				local rel_path="${filepath##$in_path}"
				rel_path="${rel_path#/}"
				#create namespace file name: name-space-file
				local ns=$(basename $(echo "$rel_path" | tr '/' '-') '.md' | tr '[:upper:]' '[:lower]')
				convert_file "$filepath" "$out_path/$ns.html"
		done < <(find "$in_path" -type f -name '*.md' | grep -v -i 'readme')
}

## check dependencies
if [[ ! -x $(which pandoc) || ! -x $(which m4) ]]; then
		echo "Requires pandoc and m4 to build, aborting." >&2
		exit 1
fi

## and off we go!
echo "Generating docs ..."
convert_dir "$DOC_ROOT" "$OUT_ROOT"

echo "Copying resources ..."
cp -a "$PROJECT_ROOT/build/include" "$OUT_ROOT/"

m4 --prefix-builtins "$PROJECT_ROOT/build/include/style.css" > "$OUT_ROOT/include/style.css"

index_html="$(cd $PROJECT_ROOT/build/templates && m4 --prefix-builtins index.html)"
echo "$index_html" > "$OUT_ROOT/index.html"

echo "Post-processing ..."
while read path; do
		sed -i '' 's/=&gt;/\&rArr;/g' "$path" #swap out arrow character
done < <(find "$OUT_ROOT" -type f -name '*.html')
