/* Usage: phantomjs typeahead-index files/* > out.json
 */
var fs = require('fs'),
		page = require('webpage').create(),
		filelist = require('system').args.slice(1);

/* returns an object filled with {'hash': 'name'}
 */
function link_selector () {
	var links = {},
			nodes = document.querySelectorAll('article h3');

	Array.prototype.slice.call(nodes).forEach(function (elem) {
		var hash = elem.id,
				name = elem.childNodes[0].text;
		links[hash] = name;
	});
	return links;
}

function get_tokens (name) {
	var tokens = [name],
			symbol = name.split(':').pop(), //remove package name
			symbol_base = symbol.replace(/\*/g, ''); //strip '*'
	return tokens.concat(symbol, symbol_base, symbol_base.split('-'));
}

//remove files we don't want to collect links from
filelist = filelist.filter(function (filename) {
	var basename = fs.absolute(filename).split(fs.separator).pop(),
			ext = basename.split('.').pop();
	
	if (fs.isFile(filename) && ext === 'html' && basename !== 'index.html') {
		return true;
	} else {
		fs.write('/dev/stderr', filename + " is not an HTML file, skipping.\n");
		return false;
	}
})

var output = [],
		doc_root = '.';

filelist.forEach(function (filename) {
	var fp = fs.open(filename, 'r'),
			basename = fs.absolute(filename).split(fs.separator).pop();
		
	page.content = fp.read();
	fp.close();

	var links = page.evaluate(link_selector);

	for (var hash in links) {
		output.push({
			value: links[hash], //name
			tokens: get_tokens(links[hash]),
			filename: basename,
			url: [doc_root, basename +'#'+ hash].join('/')
		});
	}
});

fs.write('/dev/stdout', JSON.stringify(output), 'w');
phantom.exit();
