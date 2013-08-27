## Usage: cat file.md | bash create-toc.sh
## Generates an html list of links from markdown headers.

FILE=/dev/stdin

declare -a ID_ARR #keeps sequential order
declare -A ID_MAP #hashmap used to check for duplicates

while read line; do
		#lines starting with '[' are api links
		if [[ "$line" =~ \[ ]]; then
				# match first occurrence of brackets
				name=$(echo -n "$line" | cut -d ' ' -f1 | sed 's/^\[\(.*\)\]$/\1/')
				name=$(echo "$name" | cut -d ':' -f2-) #remove package name in sidebar
				is_header=0
		else
				#otherwise a header
				name="$line"
				is_header=1
		fi

		#id target conforms to pandocs url id generation
		id=$(echo -n "$line" | tr [:upper:] [:lower:] | tr -d ':' | \
				tr -C [:alnum:] '-' | tr -s '-' | sed 's/^-//' | sed 's/-$//')

		#if the link already exists pandoc appends '-{i}'
		temp_id="$id"
		i=1
		while [ -n "${ID_MAP[$temp_id]}" ]; do
				temp_id="$id-$i"
				i=$((i+1))
		done

		ID_MAP["$temp_id"]="$name|$is_header"
		ID_ARR+=("$temp_id")

done < <(cat "$FILE" | grep '^#' | sed 's/^#\{1,\}[[:space:]]//') #strip header notation


## generate html ...

echo "<ul>"
for id in "${ID_ARR[@]}"; do
		name=$(echo "${ID_MAP[$id]}" | cut -d '|' -f1)
		is_header=$(echo "${ID_MAP[$id]}" | cut -d '|' -f2)
		if [[ $is_header == 1 ]]; then
				echo "  <li><a href=\"#$id\" class=\"toc-header\">$name</a></li>"
		else
				echo "  <li><a href=\"#$id\">$name</a></li>"
		fi
done
echo "</ul>"
