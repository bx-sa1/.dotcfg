#!/usr/bin/env python3

import time
from Xlib import display, Xatom
display = display.Display()
root = display.screen().root

_NET_DESKTOP_NAMES = display.intern_atom("_NET_DESKTOP_NAMES")

class Module:
    def __init__(self, padding=0):
        self.padding = padding

    def display(self):
        pass

class Workspaces(Module):
    def display(self):
        workspaces = root.get_full_property(_NET_DESKTOP_NAMES, 0).value.split(b"\x00")
        workspace_names = [w.decode("utf-8") for w in workspaces]

        wstr = ""
        for w in workspace_names:
            wstr += "%{{O{padding}}}{wn}%{{O{padding}}} ".format(padding=self.padding, wn=w)

        return wstr

class Time(Module):
    def display(self):
        t = time.localtime()
        current_time = time.strftime("%H:%M:%S", t)

        return current_time

class Container(Module):
    def __init__(self, modules, padding=0):
        super().__init__(padding)
        self.modules = modules

    def display(self):
        mstr = ""
        for m in self.modules:
            mstr += "%{{O{padding}}}{m}%{{O{padding}}}".format(padding=self.padding, m=m.display())
        
        return mstr

class Left(Container):
    def display(self):
        return "%{{l}}%{{B#88888888}}{}".format(super().display())

class Center(Container):
    def display(self):
        return "%{{c}}%{{B-}}{}".format(super().display())

class Right(Container):
    def display(self):
        return "%{{r}}%{{B#88888888}}{}".format(super().display())

modules = Container([Left([Workspaces()]), Right([Time()])])
while True:
    print(modules.display())

