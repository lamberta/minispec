var page = require('webpage').create(),
		args = require('system').args,
		fs = require('fs'),
		jquery_src = "http://code.jquery.com/jquery.js",
		doc_root = "http://common-lisp.net/project/alexandria/draft/alexandria.html",
		pkg_name = "alexandria";

/* executed within the context of client html
 * returns object collection of {'fn-name': 'link-frag'}
 */
function link_selector () {
	var links = {};
	
	$('.defun').each(function (i, elem) {
		var el = $(elem),
				name = el.find('b').text(),
				hash = el.prev().find('a').attr('name');
		
		//escape asterisks for markdown
		name = name.toLowerCase().replace(/\*/g, '\\*')
		
		//ignore duplicates for now
		if (!links.hasOwnProperty(name)) {
			links[name] = '#' + hash;
		}
	});
	return links;
}

//determine out file path
var d = args[0].split(fs.separator);
d.pop();
d.push('data', pkg_name +'.md');
var out_file = d.join(fs.separator);

/* main
 */
page.open(doc_root, function (status) {
	if (status !== 'success') {
		console.warn('Failed to load url, aborting.');
		phantom.exit(1);
	} else {
		page.includeJs(jquery_src, function () {
			var links = page.evaluate(link_selector),
					file_lines = [];
			
			//markdown reference link form
			for (var name in links) {
				file_lines.push('['+ pkg_name +':'+ name +']: ' + doc_root + links[name]);
			}
			
			//overwrite and exit
			fs.write(out_file, file_lines.join('\n'), 'w');
			phantom.exit();
		});
	}
});

page.onResourceReceived  = function (res) {
	if (res.status === 404) {
		console.warn("Received response 404, aborting.")
		phantom.exit(1);
	}
};
