/**
 * jQuery PageSlide
 *
 * This jQuery plugin was inspired by the UI designs of Aza Raskin (http://www.azarask.in/),
 * in his Firefox mobile and Ubiquity mouse gesture prototypes, adapted for use as a jQuery lightBox-esque plugin.
 *
 * @name jquery-pageslide-0.2.js
 * @author Scott Robbin - http://srobbin.com
 * @version 0.2
 * @date January 7, 2009
 * @category jQuery plugin
 *
 * Dual licensed under the MIT and GPL licenses:
 *   http://www.opensource.org/licenses/mit-license.php
 *   http://www.gnu.org/licenses/gpl.html
 **/

(function($){
    $.fn.pageSlide = function(options) {
        // Define default settings and override with options.
		settings = $.extend({
		    width:          "300px", // Accepts fixed widths
		    duration:       "normal", // Accepts standard jQuery effects speeds (i.e. fast, normal or milliseconds)
		    start:        function(){},
		    stop:         function(){},
            loaded:       function(){}
		}, options);
		
		function _initialize() {
		    // Create and prepare elements for pageSlide
		    var psBodyWrap = document.createElement("div");
		    $(psBodyWrap).attr("id", "pageslide-body-wrap").width( $("body").width() );
		    
		    var psSlideContent = document.createElement("div");
		    $(psSlideContent).attr("id", "pageslide-content").width( settings.width );
		    
		    var psSlideWrap = document.createElement("div");
		    $(psSlideWrap).attr("id", "pageslide-slide-wrap").append( psSlideContent );
		    		    
		    // Wrap and append so that we have the slide containers
		    $("body").contents().wrapAll( psBodyWrap );
		    $("body").append( psSlideWrap );
		    
		    // If a user clicks the document, we should hide the pageslide
		    // and override that click functionality for the slide pane itself
		    $(document).click(function() {
		        _closeSlide();
		    });

 

		    $("#pageslide-slide-wrap").click(function(){ return false; });
		    
		    // Callback events for window resizing
		    $(window).resize(function(){
              $("#pageslide-body-wrap").width( $("body").width() );
            });
		};
		/**
		* Start the jQuery pageslide plugin
		*
		* Wraps the body's children inside of a DIV, so that it can slide upon start action
		*/
		
		function _openSlide(el) {
		    settings.start();
			$("#pageslide-slide-wrap").animate({width: settings.width}, settings.duration);
		    $("#pageslide-body-wrap").animate({left: "-" + settings.width}, settings.duration, function() {
		            settings.stop();
		    });
		    $.ajax({
		        type: "GET",
		        url: $(el).attr("href"),
		        success: function(data) {
		            $("#pageslide-content").html(data)
		                                   .queue( function() {
		                                       settings.loaded();

                                               // add a close button *within* the loaded element
                                                var closeButton = $("#pageslide-close");
                                                if(closeButton && closeButton.length > 0) { // may not exist
                                                  closeButton.click(function() {
                                                      _closeSlide();
                                                  });
                                                }

		                                       $(this).dequeue();
		                                    });
		        }
		    });
		};
		
		function _closeSlide() {
		    settings.start();
		    $("#pageslide-body-wrap").animate({left: "0" }, settings.duration);
	        $("#pageslide-slide-wrap").animate({width: "0"}, settings.duration, function() {
	            $("#pageslide-content").empty();
	            settings.stop();
	        });
		}
        
        // Initalize pageslide, if it hasn't already been done.
		if($("#pageslide-body-wrap").length == 0) _initialize();
		return this.each(function(){
			$(this).unbind("click").bind("click", function(){
			    _openSlide(this);
				return false;
			});
		});
    };

})(jQuery);