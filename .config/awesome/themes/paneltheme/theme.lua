local gears = require("gears")
local lain = require("lain")
local awful = require("awful")
local wibox = require("wibox")

local math, string, os = math, string, os
local my_table = awful.util.table or gears.table

local theme        = {}
theme.dir          = os.getenv("HOME") .. "/.config/awesome/themes/paneltheme"
theme.wallpaper    = theme.dir .. "/wallpaper.jpg"
theme.font         = "RobotoMono Nerd Font 12"
theme.taglist_font = "RobotoMono Nerd Font 14"
theme.fg_normal    = "#FFFFFF"
theme.fg_focus     = "#FFFFFF"
theme.fg_urgent    = "#FFFFFF"
theme.bg_normal    = "#282C34"
theme.bg_focus     = "#282C34"
theme.bg_urgent    = "#282C34"
theme.taglist_fg_focus = "FF0000"
theme
