local gears = require("gears")
local lain = require("lain")
local awful = require("awful")
local wibox = require("wibox")

local math, string, os = math, string, os
local my_table = awful.util.table or gears.table

local theme = {}
theme.dir   = os.getenv("HOME") .. "/.config/awesome/themes/"
theme.wallpaper                                 = theme.dir .. "/wallpaper.jpg"
theme.font                                      = "RobotoMono Nerd Font 12"
theme.taglist_font                              = "RobotoMono Nerd Font 15"
theme.fg_normal                                 = "#FFFFFF"
theme.fg_focus                                  = "#FFFFFF"
theme.fg_urgent                                 = "#FFFFFF"
theme.bg_normal                                 = "#282C34"
theme.bg_focus                                  = "#282C34"
theme.bg_urgent                                 = "#282C34"
theme.taglist_fg_focus                          = "#FFFFFF"
theme.tasklist_bg_focus                         = "#282C34"
theme.tasklist_fg_focus                         = "#282C34"
theme.border_width                              = 4
theme.border_normal                             = "#DDDDDD"
theme.border_focus                              = "#FF0000"
theme.border_marked                             = "#CC9393"
theme.menu_height                               = 24
theme.menu_width                                = 140
theme.menu_submenu_icon                         = theme.dir .. "/icons/submenu.png"
theme.awesome_icon                              = theme.dir .. "/icons/awesome.png"
theme.taglist_squares_sel                       = theme.dir .. "/icons/square_sel.png"
theme.taglist_squares_unsel                     = theme.dir .. "/icons/square_unsel.png"
theme.layout_tile                               = theme.dir .. "/icons/tile.png"
theme.layout_tileleft                           = theme.dir .. "/icons/tileleft.png"
theme.layout_tilebottom                         = theme.dir .. "/icons/tilebottom.png"
theme.layout_tiletop                            = theme.dir .. "/icons/tiletop.png"
theme.layout_fairv                              = theme.dir .. "/icons/fairv.png"
theme.layout_fairh                              = theme.dir .. "/icons/fairh.png"
theme.layout_spiral                             = theme.dir .. "/icons/spiral.png"
theme.layout_dwindle                            = theme.dir .. "/icons/dwindle.png"
theme.layout_max                                = theme.dir .. "/icons/max.png"
theme.layout_fullscreen                         = theme.dir .. "/icons/fullscreen.png"
theme.layout_magnifier                          = theme.dir .. "/icons/magnifier.png"
theme.layout_floating                           = theme.dir .. "/icons/floating.png"
theme.widget_ac                                 = theme.dir .. "/icons/ac.png"
theme.widget_battery                            = theme.dir .. "/icons/battery.png"
theme.widget_battery_low                        = theme.dir .. "/icons/battery_low.png"
theme.widget_battery_empty                      = theme.dir .. "/icons/battery_empty.png"
theme.widget_mem                                = theme.dir .. "/icons/mem.png"
theme.widget_cpu                                = theme.dir .. "/icons/cpu.png"
theme.widget_temp                               = theme.dir .. "/icons/temp.png"
theme.widget_net                                = theme.dir .. "/icons/net.png"
theme.widget_hdd                                = theme.dir .. "/icons/hdd.png"
theme.widget_music                              = theme.dir .. "/icons/note.png"
theme.widget_music_on                           = theme.dir .. "/icons/note.png"
theme.widget_music_pause                        = theme.dir .. "/icons/pause.png"
theme.widget_music_stop                         = theme.dir .. "/icons/stop.png"
theme.widget_vol                                = theme.dir .. "/icons/vol.png"
theme.widget_vol_low                            = theme.dir .. "/icons/vol_low.png"
theme.widget_vol_no                             = theme.dir .. "/icons/vol_no.png"
theme.widget_vol_mute                           = theme.dir .. "/icons/vol_mute.png"
theme.widget_mail                               = theme.dir .. "/icons/mail.png"
theme.widget_mail_on                            = theme.dir .. "/icons/mail_on.png"
theme.widget_task                               = theme.dir .. "/icons/task.png"
theme.widget_scissors                           = theme.dir .. "/icons/scissors.png"
theme.widget_weather                            = theme.dir .. "/icons/dish.png"
theme.tasklist_plain_task_name                  = true
theme.tasklist_disable_icon                     = true
theme.useless_gap                               = 4

local markup = lain.util.markup
local seperators = lain.util.seperators

-- Textclock
local clockicon = wibox.widget.imagebox(theme.widget_clock)
local clock = awful.widget.watch(
    "date +'%a $d %b %R'", 60,
    function(widget, stdout)
        widget:set_markup(" " .. marup.font(theme.font, stdout))
    end
)

-- Calendar
theme.cal = lain.widget.call({
    attach_to = { claco },
    notification_preset = {
        font = "RobotoMono Nerd Font 10"

