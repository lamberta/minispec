DIR_ROOT=$(cd -P $(dirname "$0") && pwd)
URL_ROOT="http://www.lispworks.com/documentation/HyperSpec"
MAP_FILE="$DIR_ROOT/data/Map_Sym.txt"
MAP_EXTRA_FILE="$DIR_ROOT/data/Map_Sym_Extra.txt"
OUT_FILE="$DIR_ROOT/data/$(basename $0 '.sh').md"

if [ -f "$OUT_FILE" ]; then rm "$OUT_FILE"; fi

function process_file {
		## read two lines at a time
		while read line1; do
				read line2;
				#escape asterisks for markdown
				symbol_name=$(echo "$line1" |  sed 's/\*/\\*/g' | tr [:upper:] [:lower:])
				symbol_path="${line2#..}"
				echo "[$symbol_name]: $URL_ROOT$symbol_path" >> "$OUT_FILE"
		done < <(cat "$1")
}

#main hyoerspec
process_file "$MAP_FILE"
#specific references (class and such)
process_file "$MAP_EXTRA_FILE"
