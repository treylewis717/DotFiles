from typing import List  # noqa: F401

from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, KeyChord, Key, Match, Screen
from libqtile.lazy import lazy

mod = "mod4"
myTerm = "alacritty"
myEditor = "emacsclient -c -a emacs"

keys = [
    # Windows and Layouts
    Key([mod], "h", lazy.layout.left(),
        desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(),
        desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(),
        desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(),
        desc="Move focus up"),
    Key([mod], "Tab", lazy.layout.next(),
        desc="Move window focus to other window"),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(),
        desc="Move window up"),
    Key([mod, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(),
        desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(),
        desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(),
        desc="Reset all window sizes"),
    Key([mod], "t", lazy.window.toggle_floating(),
        desc='toggle floating'),
    Key([mod], "space", lazy.next_layout(),
        desc="Toggle between layouts"),
    Key([mod, "shift"], "c", lazy.window.kill(),
        desc="Kill focused window"),

    # Qtile
    Key([mod], "q", lazy.restart(),
        desc="Restart Qtile"),
    Key([mod, "shift"], "q", lazy.shutdown(),
        desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawncmd(),
        desc="Spawn a command using a prompt widget"),

    # Multimedia
    Key([], "XF86AudioStop", lazy.spawn("playerctl -p spotify stop"),
        desc="Stop music"),
    Key([], "XF86AudioPlay", lazy.spawn("playerctl -p spotify play-pause"),
        desc="Play/Pause music"),
    Key([], "XF86AudioPrev", lazy.spawn("playerctl -p spotify previous"),
        desc="Goes to previous track"),
    Key([], "XF86AudioNext", lazy.spawn("playerctl -p spotify next"),
        desc="Goes to next track"),
    Key([], "XF86AudioMute", lazy.spawn("amixer set Master toggle"),
        desc="Mute audio"),
    Key([], "XF86AudioLowerVolume", lazy.spawn("amixer set Master 5%- unmute"),
        desc="Lower volume"),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer set Master 5%+ unmute")),

    # Programs
    Key([mod, "shift"], "Return", lazy.spawn(myTerm+" -e fish"),
        desc="Launch terminal"),
    KeyChord([mod, "shift"], "d", [
        Key([], "d", lazy.spawn("dmenu_run"),
            desc="Spawn normal dmenu"),
    ]),
    KeyChord([mod, "shift"], "e", [
        Key([], "e", lazy.spawn(myEditor),
            desc="Spawn Emacs"),
        Key([], "m", lazy.spawn(myEditor+"--eval '(mu4e)'"),
            desc="Spawn mu4e in Emacs"),
        Key([], "d", lazy.spawn(myEditor+"--eval '(dired nil)'"),
            desc="Spawn dired in Emacs"),
    ])
]

group_names = [("Main", {'layout': 'monadtall'}),
               ("Background", {'layout': 'monadtall'}),
               ("Extra", {'layout': 'monadtall'}),
               ("VM", {'layout': 'floating'})]

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name)))


layout_theme = {"border_width": 2,
                "margin": 4,
                "border_focus": "#FF0000",
                "border_normal": "#1d2330"
}

layouts = [
    # layout.Columns(**layout_theme),
    layout.Max(**layout_theme),
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    layout.MonadTall(**layout_theme),
    # layout.MonadWide(),
    # layout.RatioTile(),
    layout.Tile(**layout_theme),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='Ubuntu',
    fontsize=12,
    padding=3,
    background = "#282c34"
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.Sep(
                    linewidth = 0,
                    padding = 6,
                    foreground = "#FFFFFF"
                ),
                widget.Image(
                    filename = "~/.config/qtile/icons/python-white.png",
                    scale = "False",
                    padding = 10
                ),
                widget.Sep(
                    linewidth = 1,
                    padding = 15,
                    foreground = "#FFFFFF"
                ),
                widget.GroupBox(
                    font = "Ubuntu Bold",
                    active = "#98be65",
                    inactive = "#c792ea",
                    urgent = "#C45500",
                    highlight_method = "line"
                ),
                widget.Prompt(
                    padding = 10,
                    foreground = "#FFFFFF"
                ),
                widget.Sep(
                    linewidth = 1,
                    padding = 15,
                    foreground = "#FFFFFF"
                ),
                widget.WindowName(
                    padding = 5
                ),
                widget.CPU(
                    format = " CPU: {load_percent}%",
                    foreground = "#00EC00"
                ),
                widget.Sep(
                    linewidth = 1,
                    padding = 15,
                    foreground = "#FFFFFF"
                ),
                widget.TextBox(
                    text = " Memory:",
                    foreground = "#0000EC"
                ),
                widget.Memory(
                    foreground = "#0000EC"
                ),
                widget.Sep(
                    linewidth = 1,
                    padding = 15,
                    foreground = "#FFFFFF"
                ),
                widget.Net(
                    interface = "wlan0",
                    format = " {up}  {down}",
                    foreground = "#C800C8"
                ),
                widget.Sep(
                    linewidth = 1,
                    padding = 15,
                    foreground = "#FFFFFF"
                ),
                widget.CheckUpdates(
                    update_interval = 2000,
                    distro = "Arch_checkupdates",
                    display_format = "{updates} Updates",
                    foreground = "#00CBFF"
                ),
                widget.Sep(
                    linewidth = 1,
                    padding = 15,
                    foregroung = "#FFFFFF"
                ),
                widget.Clock(
                    format = " %A, %B %d, %Y",
                    foreground = "#EEC800"
                ),
                widget.Sep(
                    linewidth = 1,
                    padding = 15,
                    foregroung = "#FFFFFF"
                ),
                widget.Clock(
                    format = " %H:%M:%S",
                    foreground = "F26F00"
                ),
                widget.Sep(
                    linewidth = 1,
                    padding = 15,
                    foregroung = "#FFFFFF"
                ),
                widget.Systray(),
            ],
            24,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None  # WARNING: this is deprecated and will be removed soon
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    Match(title='branchdialog'),  # gitk
    Match(title='pinentry'),  # GPG key password entry
])
auto_fullscreen = True
focus_on_window_activation = "smart"

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/autostart/autostart.sh'])
