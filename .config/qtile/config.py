# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import subprocess
import os

from libqtile import bar, layout, widget, hook, qtile, extension
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from libqtile.log_utils import logger

import colors

mod = "mod4"
terminal = guess_terminal()

sticky_win_list = []

@hook.subscribe.startup_once
def xdg_autostart():
    subprocess.Popen(['/usr/bin/dex', '-a'])

@hook.subscribe.startup_once
def core_autostart():
    subprocess.Popen([os.path.expanduser('~/.config/qtile/autostart-x11.sh')])

@lazy.function
def show_key_binds(qtile):
    key_binds = ["{}+{} - {}".format(('+'.join(k.modifiers)), k.key, k.desc) for k in keys]
    dmenu = extension.Dmenu()
    dmenu._configure(qtile)
    dmenu.run(key_binds)

@lazy.function
def show_stuck_windows(qtile):
    global sticky_win_list
    dmenu = extension.Dmenu()
    dmenu._configure(qtile)
    wname = dmenu.run([w.info()["name"] for w in sticky_win_list])

def stick_win(qtile):
    global sticky_win_list
    if qtile.current_window not in sticky_win_list:
        sticky_win_list.append(qtile.current_window)

def unstick_win(qtile):
    global sticky_win_list
    if qtile.current_window in sticky_win_list:
        sticky_win_list.remove(qtile.current_window)

@hook.subscribe.setgroup
def move_win():
    global sticky_win_list
    for w in sticky_win_list:
        w.togroup(qtile.current_group.name)

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),
    Key(
        [mod],
        "f",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen on the focused window",
    ),
    Key([mod], "t", lazy.window.toggle_floating(), desc="Toggle floating on the focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.run_extension(extension.J4DmenuDesktop()), desc="Run a desktop file"),
    Key([mod, "shift"], "r", lazy.run_extension(extension.DmenuRun()), desc="Run a desktop file"),

    Key([mod], "e", lazy.spawn("xdg-open " + os.path.expanduser('~')), desc="Launch file manager"),
    Key([mod, "shift"], "slash", show_key_binds, desc="Show Keybinds"),
    Key([mod], "o", lazy.function(stick_win), desc="Stick Window"),
    Key([mod, "shift"], "o", lazy.function(unstick_win), desc="Unstick Window"),
    Key([mod, "control"], "o", show_stuck_windows, desc="Show stuck windows")
]

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

layouts = [
    layout.Columns(
        margin=10, 
        border_focus=colors.a1, 
        border_normal=colors.a2,
        border_width=2
    ),
    layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="Noto Sans Mono Bold",
    fontsize=12,
    padding=8,
    background=colors.bg,
    foreground=colors.fg
)

extension_defaults = dict(
    dmenu_font="Noto Sans Mono Bold",
    dmenu_bottom=True,
    dmenu_lines=5,
    dmenu_prompt=">",
    selected_background=colors.a1,
    selected_foreground=colors.bg,
)
extension_defaults.update(widget_defaults)

def search():
    qtile.cmd_spawn(launcher)

def sleep():
    qtile.cmd_spawn("systemctl suspend")

screens = [
    Screen(
        wallpaper='~/.local/share/wallpapers/eva-1.jpg',
        wallpaper_mode='fill',
        top=bar.Bar(
            [
                widget.TextBox(
                    fmt='󰤄 Sleep',
                    mouse_callbacks={"Button1": sleep},
                ),
                widget.GroupBox(
                    rounded=False,
                    highlight_method="block",
                    this_current_screen_border=colors.a1,
                    block_highlight_text_color=colors.bg,
                    padding=4
                ),
                widget.CurrentLayout(
                    fmt='{}',
                ),
                widget.TextBox(
                    fmt='󰍉 Search',
                    mouse_callbacks={"Button1": search},
                ),
                widget.WindowName(
                    format = "{name}",
                    empty_group_string = 'Desktop',
                    background=colors.a1,
                    foreground=colors.bg
                ),
                widget.Systray(),
                widget.Memory(
                    format='{MemUsed: .0f}{mm}',
                    update_interval=5,
                ),
                widget.Mpris2(
                    max_chars=20,
                ),
                widget.Volume(
                    fmt="󰕾 {}",
                    mouse_callbacks={"Button3": lazy.spawn("pavucontrol")},
                ),
                widget.Clock(
                    fmt=" {}",
                    format='%I:%M %p',
                ),
            ],
            30,
            # border_color = '#282738',
            # border_width = [0,0,0,0],
            # margin = [15,60,6,60],
        ),
        # You can uncomment this variable if you see that on X11 floating resize/moving is laggy
        # By default we handle these events delayed to already improve performance, however your system might still be struggling
        # This variable is set to None (no cap) by default, but you can set it to 60 to indicate that you limit it to 60 events per second
        # x11_drag_polling_rate = 60,
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    border_focus=colors.a1,
    border_normal=colors.a2,
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = False

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"


