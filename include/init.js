$(document).ready(function () {
	/* Syntax highlighting <https://github.com/orthecreedence/highlight-lisp>
	 */
	$('pre code').each(function (i, elem) {
		$(elem).addClass('lisp');
	});
	HighlightLisp.highlight_auto();

	/* Navbar dropdown
	 */
	var navbar_li_ul = $('#navbar li > ul');
	$('#navbar li').click(function () {
		navbar_li_ul.not($(this).children('ul').toggle()).hide();
	});
	$('html').click(function () {
		navbar_li_ul.hide();  //if any click, hide dropdowns...
	});
	$('header').click(function (evt) {
		if (!$(evt.target).is('a')) { //if navbar link, then close...
			evt.stopPropagation();      //otherwise, leave dropdown open.
		}
	});
	navbar_li_ul.parent().click(function (evt) {
		if ($(this).children()[0] === evt.target) { //ignore clicks to dropdown heads
			return false;
		}
	});

	/* Sidebar, highlight active link
	 */
	var sidebar_li = $('aside nav li');
	$('aside nav li > a').click(function () {
		sidebar_li.removeClass('active');
		$(this).parent().toggleClass('active');
	});

	/* Minimize advanced sections
	 */
	var adv_headers = $('article h2:contains(Advanced)'); //case-sensitive
	if (adv_headers.length > 0) {
		var hash_ids = {}; //map header ids to a list of entry ids
		//minimize each section; hide elements from toc; associate ids
		adv_headers.each(function (i, h) {
			//set up dom hierarchy
			var adv_header = $(h).addClass('advanced-header'),
					adv_header_id = adv_header.attr('id'),
					adv_header_toggle = $('<span class="advanced-toggle">&nbsp;&oplus;</span><span class="advanced-toggle" style="display:none">&nbsp;&ominus;</span>').appendTo(adv_header),
					section = adv_header.nextUntil('h1,h2').wrapAll('<div class="advanced-section"/>').parent().hide(),
					container = section.wrap('<div class="advanced-container" />').parent(),
					collapse_filler = $('<p></p>').appendTo(container),
					toggleAdvancedSection = function () {
						section.toggle();
						adv_header_toggle.toggle();
						collapse_filler.toggle();
					};
			adv_header.click(toggleAdvancedSection);
			collapse_filler.click(toggleAdvancedSection);

			hash_ids[adv_header_id] = [];
			var name_list = [];
			//iterate entries
			section.find('h3, h4').each(function (i, h) {
				var entry = $(h),
						entry_id = entry.attr('id');
				
				//hide sidebar toc links
				$('aside nav a[href="#'+ entry_id +'"]').parent().hide();
				//associate section ids with child ids
				hash_ids[adv_header_id].push('#' + entry_id);
				
				//display symbol names in the collapsed section
				var entry_a = entry.find('a');
				if (entry_a.length !== 0) {
					name_list.push(entry_a.text());
				} else {
					name_list.push(entry.text());
				}
			});
			collapse_filler.append(name_list.join(', ') + "&nbsp;&hellip;");
		});

		//hash ids how hidden from search, scroll to parent section
		if (!$(document.location.hash).is(':visible')) {
			var hash = document.location.hash;
			for (var id in hash_ids) {
				if (hash_ids[id].indexOf(hash) !== -1) {
					$(document).scrollTop($('#'+id).offset().top - 15);
					break;
				}
			}
		}
	}

	

	var win = $(window),
			header = $('header'),
			aside = $('aside'),
			asidenav = $('aside nav'),
			article = $('div.article-wrapper'),
			entries = $('article h1, article h2, article h3'),
			sidebar_max_height = parseInt(asidenav.css('max-height')) * 0.01; //percentage

	/* Add navbar offset to entries, store original values
	 */
	entries.each(function (i, elem) {
		var h = $(elem),
				navbar_height = header.height();
		elem.cssMarginTop = parseInt(h.css('margin-top'));
		elem.cssPaddingTop	= parseInt(h.css('padding-top'));
		h.css({
			'margin-top': -(elem.cssMarginTop + navbar_height),
			'padding-top': elem.cssPaddingTop + navbar_height
		});
	});

	/* Fit content snuggly below navbar, re-adjust navbar offsets if necessary
	 */
	function onNavbarSizeChange () {
		var navbar_height = header.height();
		//detect change
		if (navbar_height !== parseInt(aside.css('margin-top'))) {
			//fit content below navbar, resize sidebar height
			article.css('margin-top', navbar_height);
			aside.css('margin-top', navbar_height);
			asidenav.css('max-height', parseInt((win.height() - navbar_height) * sidebar_max_height));
			//set new entry offsets using preiously stored original setting
			entries.each(function (i, elem) {
				$(elem).css({
					'margin-top': -(elem.cssMarginTop + navbar_height),
					'padding-top': elem.cssPaddingTop + navbar_height
				});
			});
		}
	}
	onNavbarSizeChange();
	$(window).resize(onNavbarSizeChange);
	
	/* Search typeahead <https://github.com/twitter/typeahead.js>
	 */ 
	var search_form = $('#navbar form');
	search_form.find('input').typeahead({
		name: 'index',
		prefetch: './search-index.json',
		limit: 20
	});
	search_form.find('input').on('typeahead:selected typeahead:autocompleted', function (evt, data) {
		document.location.href = data.url;
		search_form[0].reset();
		return false;
	});
});
