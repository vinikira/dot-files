local wibox = require("wibox")
local awful = require("awful")

battery_widget = wibox.widget.textbox()
battery_widget:set_align("right")
battery_widget:set_text("| Bat |")
battery_widget_timer = timer({ timeout = 5 })
battery_widget_timer:connect_signal("timeout",
	function()
		fh = assert(io.popen("acpi | cut -d, -f 2,3 | sed 's/remaining//'", "r"))
		battery_widget:set_text(" |" .. fh:read("*l") .. " | ")
		fh:close()
	end
)

battery_widget_timer:start()
