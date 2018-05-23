# Nick's implementation of multiwindow shiny for campfire
# Three windows: The controller, wall, and floor are hard
# coded, and any shiny objects are supported inside each
# window. This gives more flexibility than the other
# multiwindow shiny which can only display plotly objects
#
# UPDATED: 23 May 2018 (JSE): 
#    * Monitor window added; 
#    * css for centering added; 
#    * verified on Shiny Server

library(shiny)

campfireApp = function(controller = NA, wall = NA, floor = NA, monitor=NA, serverFunct = NA) {
	ui <- campfireUI(controller, wall, floor, monitor)

	serverValues = reactiveValues()
	campfire_server <- shinyServer(function(input, output) {

		observe({
			for (inputId in names(input)) {
				serverValues[[inputId]] <- input[[inputId]]
			}
		})
		serverFunct(serverValues, output)

	})

	options(shiny.port = 5480)
	shinyApp(ui, server = campfire_server)
}

campfireUI = function(controller, wall, floor, monitor) {
	ui <- shinyUI(bootstrapPage(
		HTML('<script type="text/javascript">
			$(function() {
				$("div.Window").hide(); 
				var tokens = window.location.href.split("?");
				if (tokens.length > 1) {
					var shown_window = tokens[1];
					$("div."+shown_window).show();
				} else {
					$("div.WindowSelector").show();
				}
			});
		     </script>'),
		div(class="WindowSelector Window",
		    HTML('<h2><a href="?Controller">Controller</a></h2>'),
		    HTML('<h2><a href="?Wall">Wall</a></h2>'),
		    HTML('<h2><a href="?Floor">Floor</a></h2>'),
		    HTML('<h2><a href="?Monitor">External Monitor</a></h2>'),
		    style='position: absolute; 
		          top: 50%; left: 50%; 
		          margin-right: -50%; 
		          transform: translate(-50%, -50%)'
		),
		div(class="Controller Window",
		    controller
		),
		div(class="Wall Window",
		    wall 
		),
		div(class="Floor Window",
		    floor
		),
		div(class="Monitor Window",
		    monitor
		)
		
	))

	return(ui)
}
