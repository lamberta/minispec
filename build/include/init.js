$(document).ready(function () {
	/* Syntax highlighting <https://github.com/orthecreedence/highlight-lisp>
	 */
	$('pre code').each(function (i, elem) {
		$(elem).addClass('lisp');
	});
	HighlightLisp.highlight_auto();

	/* Navbar dropdown
	 */
	$('#navbar li').click(function () {
		$('#navbar li > ul').not($(this).children('ul').toggle()).hide();
	});
	$('html').click(function () {
		$('#navbar li > ul').hide();  //if any click, hide dropdowns...
	});
	$('header').click(function (evt) {
		if (!$(evt.target).is('a')) { //if navbar link, then close...
			evt.stopPropagation();      //otherwise, leave dropdown open.
		}
	});
	$('#navbar li > ul').parent().click(function (evt) {
		if ($(this).children()[0] === evt.target) { //ignore clicks to dropdown heads
			return false;
		}
	});
	
	/* Sidebar toc bootstrap-scrollspy <http://getbootstrap.com/javascript/>
	 *
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
	**/

	/* Minimize advanced section
	 */
	var adv_header = $('h2#advanced'),
			expand_switch = $('<span class="advanced-switch">&nbsp;⊕</span><span class="advanced-switch" style="display:none">&nbsp;⊖</span>'),
			adv_section = adv_header.nextUntil('h1,h2').wrapAll('<div class="advanced-hidden"/>');
	adv_header.append(expand_switch);
	adv_section.hide();
	adv_header.click(function () {
		adv_section.toggle();
		expand_switch.toggle();
	});
	//hide sidebar toc links
	adv_section.filter('h3').each(function (i, elem) {
		$('aside nav a[href="#'+ elem.id +'"]').hide();
	});
	
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
