config.source('colors.py')

c.editor.command = ['emacsclient', '-c', '-n', '-a ', '+{line0}:{column0}', '{file}' ]

config.bind(',V', 'hint links spawn mpv {hint-url}')
config.bind(',v', 'spawn mpv {url}')
config.bind('zl', 'spawn --userscript qute-pass --mode gopass')
config.bind('zol', 'spawn --userscript qute-pass --mode gopass --otp-only')
config.bind('zpl', 'spawn --userscript qute-pass --mode gopass --password-only')
config.bind('zul', 'spawn --userscript qute-pass --mode gopass --username-only')

config.load_autoconfig()
