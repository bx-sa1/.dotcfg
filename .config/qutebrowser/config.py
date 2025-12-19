from qutebrowser.config.configfiles import ConfigAPI
from qutebrowser.config.config import ConfigContainer
config: ConfigAPI = config  # pyright: ignore
c: ConfigContainer = c  # pyright: ignore

config.source('colors.py')

c.editor.command = ['emacsclient', '-c', '-n', '-a ', '+{line0}:{column0}', '{file}' ]

config.bind(',V', 'hint links spawn python /usr/share/mpv/scripts/umpv {hint-url}')
config.bind(',v', 'spawn python /usr/share/mpv/scripts/umpv {url}')
config.bind('zl', 'spawn --userscript qute-pass --mode gopass')
config.bind('zol', 'spawn --userscript qute-pass --mode gopass --otp-only')
config.bind('zpl', 'spawn --userscript qute-pass --mode gopass --password-only')
config.bind('zul', 'spawn --userscript qute-pass --mode gopass --username-only')

config.load_autoconfig()
