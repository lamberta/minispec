$(document).ready(function () {
	//syntax highlighting
	$('pre code').each(function (i, elem) {
		$(elem).addClass('lisp');
	});
	HighlightLisp.highlight_auto();

	/* set up bootstrap-scrollspy
	 */
	var navbar = $('header nav'),
			sidebar = $('aside nav'),
			top_offset = parseInt(sidebar.css('margin-top'));
	$('body').attr({
		'data-spy': "scroll",
		'data-offset': top_offset
	});
	sidebar.scrollspy();
	//offset main article beneath fixed top navbar
	sidebar.find('li a').click(function (evt) {
		evt.preventDefault();
		var target_id = $(this).attr('href').slice(1); //remove hash symbol
		document.getElementById(target_id).scrollIntoView();
		window.scrollBy(0, -top_offset);
		return false;
	});

	/***
	//keep active sidebar element in view when scrolling
	$(window).scroll(function () {
		var li_y = sidebar.find('li.active').position().top,
				sidebar_bot = sidebar.height(),
				scrollbar_y;
		
		if (li_y > sidebar_bot) {
			scrollbar_y = sidebar.scrollTop();
			sidebar.scrollTop(scrollbar_y + li_y - sidebar_bot);
		} else if (li_y < 0) {
			scrollbar_y = sidebar.scrollTop();
			sidebar.scrollTop(scrollbar_y + li_y);
		}
	});
	***/

	/* Minimize advanced section
	 */
	var adv_header = $('h2#advanced'),
			expand_switch = $('<span class="advanced-switch">&nbsp;⊕</span><span class="advanced-switch" style="display:none">&nbsp;⊖</span>'),
			adv_section = adv_header.nextUntil('h2').wrapAll('<div class="advanced-hidden"/>');
	adv_header.append(expand_switch);
	adv_section.hide();
	adv_header.click(function () {
		adv_section.toggle();
		expand_switch.toggle();
	});
	//hide sidebar heading
	adv_section.filter('h3').each(function (i, elem) {
		$('aside nav a[href="#'+ elem.id +'"]').hide();
	});
	
	/* typeahead: https://github.com/twitter/typeahead.js
	 */ 
	var search_form = navbar.find('form');
	search_form.find('input').typeahead({
		name: 'index',
		prefetch: './search-index.json'
	});
	search_form.find('input').on('typeahead:selected typeahead:autocompleted', function (evt, data) {
		console.log('event type', evt.type);
		document.location.href = data.url;
		search_form[0].reset();
		return false;
	});
});
