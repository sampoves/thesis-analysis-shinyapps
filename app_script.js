
// JavaScript for the analysis app of my thesis results

// "Parking of private cars and spatial accessibility in Helsinki Capital Region" 
// by Sampo Vesanen, 6.8.2020


// This JS function checks the state of the display parameter of inputted div "shrink_me"
// and edits the value accordingly. Also change the eye icon of the inputted "parent_div"
// parameter.
function show_hide(shrink_me, parent_div) {
	var elem_to_hide = document.getElementById(shrink_me);
	var parent_div = '#' + parent_div;

	if(elem_to_hide.style.display === 'none') {
		elem_to_hide.style.display = 'block';
		$(parent_div).find('i.icon.eye').toggleClass('eye eyeslash');
		$(parent_div).find('i.icon.eyeslash')[0].setAttribute('title', 'Hide element');
	} else {
		elem_to_hide.style.display = 'none';
		$(parent_div).find('i.icon.eyeslash').toggleClass('eyeslash eye');
		$(parent_div).find('i.icon.eye')[0].setAttribute('title', 'Show element');
	}
};

// Specified function to hide/show sidebar
function show_hide_sb() {
	var elem_to_hide = document.getElementById('sidebar');
	var showhidebutton = document.getElementById('showhidebutton');

	if(elem_to_hide.style.display === 'none') {
		elem_to_hide.style.display = 'block';
		$(showhidebutton).find('i.icon.eye').toggleClass('eye eyeslash');
		$(showhidebutton).find('i.icon.eyeslash')[0].setAttribute('title', 'Hide sidebar');
	} else {
		elem_to_hide.style.display = 'none';
		$(showhidebutton).find('i.icon.eyeslash').toggleClass('eyeslash eye');
		$(showhidebutton).find('i.icon.eye')[0].setAttribute('title', 'Show sidebar');
	}
};

// Insert sidebar hide/show button to col-sm-3, the div that contains the sidebar. Add
// the button after the actual sidebar div. The button appears on the top right corner
// of the sidebar.
$(function() {
	$('.well').after("<div class='hidesidebar'><button id='showhidebutton' onclick='show_hide_sb()'><i class='icon eyeslash' title='Hide sidebar'></i></button></div>");
	
	// Make ggiraph outputs untouchable
	$('#hist').addClass('noselect');
	$('#barplot_ord').addClass('noselect');
	$('#boxplot').addClass('noselect');
	$('#map').addClass('noselect');
	$('#interactive').addClass('noselect');
});

// Insert significance element right after table element so that overflow-x: auto;
// does not hide the text. Again with the hacky setTimeout() approach. Could not figure
// a better way in this timeframe.
$(document).one('shiny:idle', function(event) {
	var sig = "<p id='signif'>Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1</p>";
	
	setTimeout(function() {
		// Disable mobile keyboard on dropdown menus
		$('.selectize-input input').attr('readonly', 'readonly');
		
		$('#levene table').after(sig);
		$('#anova table').after(sig);
	}, 8000);
});

// This jQuery function listens to anchor link clicking. With this function I aimed to
// make repeated clicking possible, which was not with :target selectors I used earlier.
// In short, this detects with the amount of clicks if we fire a delayed highlight or
// an immediate one. A bit hacky setTimeout() is employed to remove the delayed highlight.
// leave console.log()'s in to illustrate the behaviour of this function.
$(function() {
	
	var timesClicked = 0;
	
	$('a').on('click', function() {
		timesClicked++;
		var dest_id = $(this)[0].hash;
		var dest = dest_id.replace('#', '');
		element = document.getElementById(dest);
		
		if(timesClicked === 1) {
			console.log(`${dest_id} timesclicked: ${timesClicked}`);			
			$(dest_id).addClass('animate');
			
			ticktock = function() {
				timeout_val = setTimeout(function() {
					$(dest_id).removeClass('animate');
					// The next line removes the immediate animation from the entire
					// document. Without this fast clicking around the document ends
					// with the last div id having a lingering animation class.
					$('.animate-immediately').removeClass('animate-immediately');
					timesClicked = 0;
					console.log("end");
				}, 2000);
			}
			ticktock();
		}
		
		// Detect a click event while the class .animate is present in dest_id
		if (timesClicked > 1) {
			console.log(`timesclicked: ${timesClicked}, interrupt start`);
			clearTimeout(timeout_val);
			element.classList.remove('animate');
			element.classList.remove('animate-immediately');
			
			// Trigger a DOM reflow with .offsetWidth. A simple elem.remove().add() will 
			// not work to interrupt a CSS animation and run it again. See:
			// https://css-tricks.com/restart-css-animation/
			void element.offsetWidth;
			element.classList.add('animate-immediately');
			ticktock();
		}
    });
});

// Add a subdivision switch on/off buttons for all municipalities. Achieve this by
// adding ids to subdivGroup checkboxes, which previously didn't have any. Use this
// crude function to programmatically click all correct subdivision buttons.
function munClick(id) {
	if(id === 1) { 
		$('#subdiv_0').click(); // Espoo
		$('#subdiv_1').click();
		$('#subdiv_2').click();
		$('#subdiv_3').click();
		$('#subdiv_4').click();
		$('#subdiv_5').click();
		$('#subdiv_6').click();
	} if (id === 2) {
		$('#subdiv_7').click(); // Helsinki
		$('#subdiv_8').click();
		$('#subdiv_9').click();
		$('#subdiv_10').click();
		$('#subdiv_11').click();
		$('#subdiv_12').click();
		$('#subdiv_13').click();
		$('#subdiv_14').click();
	} if (id === 3) {
		$('#subdiv_15').click(); // Kauniainen
	} if (id === 4) {
		$('#subdiv_16').click(); // Vantaa
		$('#subdiv_17').click();
		$('#subdiv_18').click();
		$('#subdiv_19').click();
		$('#subdiv_20').click();
		$('#subdiv_21').click();
		$('#subdiv_22').click();
	}
};

$(document).ready(function() {
	// Add identifiers to subdivGroup checkboxes
	var checkboxes = $('#subdivGroup').find('.checkbox input');
	for(var i = 0; i < checkboxes.length; i++) {
		checkboxes[i].setAttribute('id', 'subdiv_' + i);
	};
	
	// Add switches after subdivGroup
	var buttonCol = "<div class='mun-btn-container' id='contents'>" +
		"<button class='btn btn-default munbutton' onclick='munClick(1)'>Espoo</button>" +
		"<button class='btn btn-default munbutton' onclick='munClick(2)'>Helsinki</button>" +
		"<button class='btn btn-default munbutton' onclick='munClick(3)'>Kauniainen</button>" +
		"<button class='btn btn-default munbutton' onclick='munClick(4)'>Vantaa</button>" +
		"</div>";
	
	$('#subdivGroup div.shiny-options-group').after(buttonCol);
	
	// Move "reset subdivisions" action button inside the new button container
	$('#resetSubdivs').appendTo($('.mun-btn-container'));
});

// Attribute and CSS operations in sidebar
$(document).ready(function() {
	// Remove "for" attributes from slider inputs in this crude way. It seems that 
	// if for points to a non-existent value, the sidebar scrolls to top. It is 	annoying, 
	// get rid of it.
	
	// Add identifiers to all labels in sidebar
	var all_labels = $('#sidebar').find('label.control-label');
	for(var i = 0; i < all_labels.length; i++) {
		all_labels[i].setAttribute("id", "lbl_" + i);
	};
	// Remove for's
	$('label#lbl_0').removeAttr('for');
	$('label#lbl_1').removeAttr('for');
	$('label#lbl_5').removeAttr('for');
	$('label#lbl_10').removeAttr('for');
	
	// Additionally, find all on-off switches in Layer options and reduce their
	// margin-bottoms.
	var onoff = $('.onoff-container').find('.form-group.shiny-input-container');
	for(var i = 0; i < onoff.length; i++) {
		onoff[i].style.marginBottom = '6px';
	};
});

// Add tooltips to all items in all selectize dropdown menus. Do not provide tooltips for
// "Y axis for barplot" dropdown menu for the reason its content fluctuates with user's
// choices and that would make the implementation harder. Settle for this scope.
$(document).one('shiny:idle', function(event) {
	
	// Define tooltip texts for dropdown menu items
	var resp_arr = ['Searching for parking', 'Walking to destination'];
	var expl_arr = ['Survey question: "How familiar are you with this postal code area?"', 
	'Survey question: "What kind of parking spot do you usually take in this postal code area?"', 
	'Survey question: "At what time of the day do you usually park in this postal code area?"', 
	'Spatial data: "Artificial surfaces"', 
	'Spatial data: "Zones of urban structure"', 
	'Spatial data: "Subdivisions of municipalities"'];
	var intmap_arr = ['Answer count', 
	'Searching for parking, mean', 
	'Searching for parking, median', 
	'Walking to destination, mean', 
	'Walking to destination, median', 
	'Artificial surfaces'];
	
	$('.selectize-control').on('click', function (e) {

		// Find the current dropdown menu
		var drop_labels = $(this).children(1).find('.option');

		// Add tooltip content to dropdown menu items
		for(var j = 0; j < drop_labels.length; j++) {

			var textcontent = drop_labels[j].innerHTML;
			drop_labels[j].setAttribute('data-placement', 'right');
			
			if (drop_labels.length === 2) {
				drop_labels[j].setAttribute('title', resp_arr[j]);
				
			} else if (!textcontent.includes('jenks') && drop_labels.length === 6) {
				// Explanatory (ordinal) variable dropdown menu
				drop_labels[j].setAttribute('title', expl_arr[j]);
				
			} else if (textcontent.includes('jenks') && drop_labels.length === 6) {
				// Interactive map dropdown menu
				drop_labels[j].setAttribute('title', intmap_arr[j]);
			}
			
		};
	});
});